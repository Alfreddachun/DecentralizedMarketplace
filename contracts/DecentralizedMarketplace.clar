;; DecentralizedMarketplace Contract
;; Constants for configuration
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ESCROW-PERCENTAGE u5) ;; 5% escrow fee
(define-constant VERIFICATION-STAKE u1000000) ;; 1M microSTX for seller verification
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-LISTING-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-VERIFIED (err u103))
(define-constant ERR-NOT-VERIFIED (err u104))
(define-constant ERR-DISPUTE-EXISTS (err u105))

;; Data maps
(define-map Sellers
    principal
    {
        verified: bool,
        stake-amount: uint,
        total-sales: uint
    }
)

(define-map Listings
    uint
    {
        seller: principal,
        price: uint,
        description: (string-ascii 256),
        active: bool,
        buyer: (optional principal),
        escrow-amount: uint
    }
)

(define-map Disputes
    uint  ;; listing-id
    {
        buyer: principal,
        seller: principal,
        status: (string-ascii 20),
        resolution: (optional bool)
    }
)

;; Data variables
(define-data-var listing-nonce uint u0)

;; Private functions
(define-private (calculate-escrow (price uint))
    (/ (* price ESCROW-PERCENTAGE) u100)
)

(define-private (is-verified (seller principal))
    (default-to false (get verified (map-get? Sellers seller)))
)

;; Public functions
(define-public (become-verified-seller)
    (let
        (
            (sender tx-sender)
            (existing-seller (map-get? Sellers sender))
        )
        (asserts! (is-none existing-seller) ERR-ALREADY-VERIFIED)
        (try! (stx-transfer? VERIFICATION-STAKE sender (as-contract tx-sender)))
        (map-set Sellers sender {
            verified: true,
            stake-amount: VERIFICATION-STAKE,
            total-sales: u0
        })
        (ok true)
    )
)

(define-public (create-listing (price uint) (description (string-ascii 256)))
    (let
        (
            (sender tx-sender)
            (listing-id (var-get listing-nonce))
        )
        (asserts! (is-verified sender) ERR-NOT-VERIFIED)
        (map-set Listings listing-id {
            seller: sender,
            price: price,
            description: description,
            active: true,
            buyer: none,
            escrow-amount: (calculate-escrow price)
        })
        (var-set listing-nonce (+ listing-id u1))
        (ok listing-id)
    )
)

(define-public (purchase-listing (listing-id uint))
    (let
        (
            (listing (unwrap! (map-get? Listings listing-id) ERR-LISTING-NOT-FOUND))
            (buyer tx-sender)
            (total-amount (+ (get price listing) (get escrow-amount listing)))
        )
        (asserts! (get active listing) ERR-LISTING-NOT-FOUND)
        (try! (stx-transfer? total-amount buyer (as-contract tx-sender)))
        (map-set Listings listing-id (merge listing {
            active: false,
            buyer: (some buyer)
        }))
        (ok true)
    )
)

(define-public (complete-purchase (listing-id uint))
    (let
        (
            (listing (unwrap! (map-get? Listings listing-id) ERR-LISTING-NOT-FOUND))
            (seller (get seller listing))
            (buyer (unwrap! (get buyer listing) ERR-NOT-AUTHORIZED))
        )
        (asserts! (is-eq tx-sender buyer) ERR-NOT-AUTHORIZED)
        (try! (as-contract (stx-transfer? (get price listing) (as-contract tx-sender) seller)))
        (try! (as-contract (stx-transfer? (get escrow-amount listing) (as-contract tx-sender) CONTRACT-OWNER)))
        (ok true)
    )
)

(define-public (raise-dispute (listing-id uint))
    (let
        (
            (listing (unwrap! (map-get? Listings listing-id) ERR-LISTING-NOT-FOUND))
            (buyer (unwrap! (get buyer listing) ERR-NOT-AUTHORIZED))
        )
        (asserts! (is-eq tx-sender buyer) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? Disputes listing-id)) ERR-DISPUTE-EXISTS)
        (map-set Disputes listing-id {
            buyer: buyer,
            seller: (get seller listing),
            status: "OPENED",
            resolution: none
        })
        (ok true)
    )
)

(define-public (resolve-dispute (listing-id uint) (in-favor-of-buyer bool))
    (let
        (
            (listing (unwrap! (map-get? Listings listing-id) ERR-LISTING-NOT-FOUND))
            (dispute (unwrap! (map-get? Disputes listing-id) ERR-LISTING-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (if in-favor-of-buyer
            (try! (as-contract (stx-transfer? (+ (get price listing) (get escrow-amount listing)) (as-contract tx-sender) (get buyer dispute))))
            (try! (as-contract (stx-transfer? (get price listing) (as-contract tx-sender) (get seller listing))))
        )
        (map-set Disputes listing-id (merge dispute {
            status: "RESOLVED",
            resolution: (some in-favor-of-buyer)
        }))
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-listing (listing-id uint))
    (map-get? Listings listing-id)
)

(define-read-only (get-seller-info (seller principal))
    (map-get? Sellers seller)
)

(define-read-only (get-dispute (listing-id uint))
    (map-get? Disputes listing-id)
)

