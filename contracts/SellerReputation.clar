(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-SELLER-NOT-FOUND (err u101))
(define-constant ERR-INVALID-SCORE (err u102))
(define-constant ERR-ALREADY-REVIEWED (err u103))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u104))

(define-constant TRANSACTION-SCORE-WEIGHT u25)
(define-constant RATING-SCORE-WEIGHT u40)
(define-constant DISPUTE-PENALTY u50)
(define-constant MIN-REPUTATION-THRESHOLD u500)
(define-constant MAX-REPUTATION-SCORE u1000)

(define-map SellerReputation
    principal
    {
        base-score: uint,
        transaction-count: uint,
        positive-ratings: uint,
        total-ratings: uint,
        dispute-count: uint,
        reputation-tier: (string-ascii 10),
        last-updated: uint
    }
)

(define-map TransactionReviews
    { listing-id: uint, reviewer: principal }
    {
        transaction-score: uint,
        review-timestamp: uint,
        seller: principal
    }
)

(define-map ReputationHistory
    { seller: principal, period: uint }
    {
        period-score: uint,
        transactions: uint,
        reviews: uint
    }
)

(define-data-var reputation-period-length uint u2016)
(define-data-var current-reputation-period uint u1)

(define-private (calculate-reputation-score (seller principal))
    (let
        (
            (reputation-data (default-to 
                {
                    base-score: u0,
                    transaction-count: u0,
                    positive-ratings: u0,
                    total-ratings: u0,
                    dispute-count: u0,
                    reputation-tier: "BASIC",
                    last-updated: u0
                }
                (map-get? SellerReputation seller)
            ))
            (transaction-bonus (* (get transaction-count reputation-data) TRANSACTION-SCORE-WEIGHT))
            (rating-bonus (if (> (get total-ratings reputation-data) u0)
                (* (/ (* (get positive-ratings reputation-data) u100) (get total-ratings reputation-data)) RATING-SCORE-WEIGHT)
                u0))
            (dispute-penalty (* (get dispute-count reputation-data) DISPUTE-PENALTY))
            (calculated-score (+ (get base-score reputation-data) transaction-bonus rating-bonus))
            (final-score (if (> calculated-score dispute-penalty)
                (- calculated-score dispute-penalty)
                u0))
        )
        (if (> final-score MAX-REPUTATION-SCORE)
            MAX-REPUTATION-SCORE
            final-score)
    )
)

(define-private (determine-reputation-tier (score uint))
    (if (>= score u800)
        "ELITE"
        (if (>= score u600)
            "PREMIUM"
            (if (>= score u400)
                "VERIFIED"
                "BASIC")))
)

(define-public (initialize-seller-reputation (seller principal))
    (let
        (
            (existing-reputation (map-get? SellerReputation seller))
        )
        (asserts! (is-none existing-reputation) ERR-NOT-AUTHORIZED)
        (map-set SellerReputation seller {
            base-score: u100,
            transaction-count: u0,
            positive-ratings: u0,
            total-ratings: u0,
            dispute-count: u0,
            reputation-tier: "BASIC",
            last-updated: stacks-block-height
        })
        (ok true)
    )
)

(define-public (record-transaction-completion (listing-id uint) (seller principal))
    (let
        (
            (reputation-data (unwrap! (map-get? SellerReputation seller) ERR-SELLER-NOT-FOUND))
            (updated-reputation (merge reputation-data {
                transaction-count: (+ (get transaction-count reputation-data) u1),
                last-updated: stacks-block-height
            }))
        )
        (map-set SellerReputation seller updated-reputation)
        (try! (update-reputation-score seller))
        (ok true)
    )
)

(define-public (submit-transaction-review (listing-id uint) (seller principal) (score uint))
    (let
        (
            (reviewer tx-sender)
            (existing-review (map-get? TransactionReviews { listing-id: listing-id, reviewer: reviewer }))
            (reputation-data (unwrap! (map-get? SellerReputation seller) ERR-SELLER-NOT-FOUND))
        )
        (asserts! (is-none existing-review) ERR-ALREADY-REVIEWED)
        (asserts! (<= score u100) ERR-INVALID-SCORE)
        (map-set TransactionReviews { listing-id: listing-id, reviewer: reviewer } {
            transaction-score: score,
            review-timestamp: stacks-block-height,
            seller: seller
        })
        (let
            (
                (updated-reputation (merge reputation-data {
                    positive-ratings: (if (>= score u70) 
                        (+ (get positive-ratings reputation-data) u1)
                        (get positive-ratings reputation-data)),
                    total-ratings: (+ (get total-ratings reputation-data) u1),
                    last-updated: stacks-block-height
                }))
            )
            (map-set SellerReputation seller updated-reputation)
            (try! (update-reputation-score seller))
            (ok true)
        )
    )
)

(define-public (record-dispute-against-seller (seller principal))
    (let
        (
            (reputation-data (unwrap! (map-get? SellerReputation seller) ERR-SELLER-NOT-FOUND))
            (updated-reputation (merge reputation-data {
                dispute-count: (+ (get dispute-count reputation-data) u1),
                last-updated: stacks-block-height
            }))
        )
        (map-set SellerReputation seller updated-reputation)
        (try! (update-reputation-score seller))
        (ok true)
    )
)

(define-public (update-reputation-score (seller principal))
    (let
        (
            (reputation-data (unwrap! (map-get? SellerReputation seller) ERR-SELLER-NOT-FOUND))
            (new-score (calculate-reputation-score seller))
            (new-tier (determine-reputation-tier new-score))
            (current-period (var-get current-reputation-period))
        )
        (map-set SellerReputation seller (merge reputation-data {
            reputation-tier: new-tier,
            last-updated: stacks-block-height
        }))
        (map-set ReputationHistory { seller: seller, period: current-period } {
            period-score: new-score,
            transactions: (get transaction-count reputation-data),
            reviews: (get total-ratings reputation-data)
        })
        (ok new-score)
    )
)

(define-public (check-reputation-eligibility (seller principal) (required-tier (string-ascii 10)))
    (let
        (
            (reputation-data (unwrap! (map-get? SellerReputation seller) ERR-SELLER-NOT-FOUND))
            (current-tier (get reputation-tier reputation-data))
        )
        (ok (or 
            (is-eq current-tier "ELITE")
            (and (is-eq required-tier "PREMIUM") (or (is-eq current-tier "PREMIUM") (is-eq current-tier "ELITE")))
            (and (is-eq required-tier "VERIFIED") (not (is-eq current-tier "BASIC")))
            (is-eq required-tier "BASIC")
        ))
    )
)

(define-public (advance-reputation-period)
    (let
        (
            (current-period (var-get current-reputation-period))
            (new-period (+ current-period u1))
        )
        (var-set current-reputation-period new-period)
        (ok new-period)
    )
)

(define-public (boost-reputation-score (seller principal) (boost-amount uint))
    (let
        (
            (reputation-data (unwrap! (map-get? SellerReputation seller) ERR-SELLER-NOT-FOUND))
            (current-score (get base-score reputation-data))
            (new-base-score (+ current-score boost-amount))
        )
        (asserts! (is-eq tx-sender seller) ERR-NOT-AUTHORIZED)
        (map-set SellerReputation seller (merge reputation-data {
            base-score: new-base-score,
            last-updated: stacks-block-height
        }))
        (try! (update-reputation-score seller))
        (ok true)
    )
)

(define-read-only (get-seller-reputation (seller principal))
    (map-get? SellerReputation seller)
)

(define-read-only (get-transaction-review (listing-id uint) (reviewer principal))
    (map-get? TransactionReviews { listing-id: listing-id, reviewer: reviewer })
)

(define-read-only (get-reputation-history (seller principal) (period uint))
    (map-get? ReputationHistory { seller: seller, period: period })
)

(define-read-only (get-current-reputation-period)
    (var-get current-reputation-period)
)

(define-read-only (calculate-seller-reputation-score (seller principal))
    (calculate-reputation-score seller)
)

(define-read-only (get-reputation-tier-requirements)
    {
        basic: u0,
        verified: u400,
        premium: u600,
        elite: u800
    }
)

(define-read-only (get-seller-reputation-summary (seller principal))
    (let
        (
            (reputation-data (unwrap! (map-get? SellerReputation seller) (err u0)))
            (current-score (calculate-reputation-score seller))
            (success-rate (if (> (get total-ratings reputation-data) u0)
                (/ (* (get positive-ratings reputation-data) u100) (get total-ratings reputation-data))
                u0))
        )
        (ok {
            seller: seller,
            current-score: current-score,
            tier: (get reputation-tier reputation-data),
            transactions: (get transaction-count reputation-data),
            success-rate: success-rate,
            disputes: (get dispute-count reputation-data)
        })
    )
)
