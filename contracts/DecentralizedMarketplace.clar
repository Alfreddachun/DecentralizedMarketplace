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

(define-map Categories 
    uint 
    (string-ascii 20)
)

(define-map ListingCategories
    uint  
    uint
)

(define-public (add-category (category-id uint) (category-name (string-ascii 20)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set Categories category-id category-name)
        (ok true)
    )
)

(define-public (create-listing-with-category (price uint) (description (string-ascii 256)) (category-id uint))
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
        (map-set ListingCategories listing-id category-id)
        (var-set listing-nonce (+ listing-id u1))
        (ok listing-id)
    )
)


(define-map SellerRatings
    { seller: principal, reviewer: principal }
    uint
)

(define-map AverageRatings
    principal
    { total-ratings: uint, rating-count: uint }
)

(define-public (rate-seller (seller principal) (rating uint))
    (let
        (
            (buyer tx-sender)
            (current-avg (default-to { total-ratings: u0, rating-count: u0 } 
                (map-get? AverageRatings seller)))
        )
        (asserts! (<= rating u5) ERR-INVALID-AMOUNT)
        (map-set SellerRatings { seller: seller, reviewer: buyer } rating)
        (map-set AverageRatings seller {
            total-ratings: (+ (get total-ratings current-avg) rating),
            rating-count: (+ (get rating-count current-avg) u1)
        })
        (ok true)
    )
)


(define-map ListingTimeLimits
    uint
    uint
)

(define-public (create-timed-listing (price uint) (description (string-ascii 256)) (duration uint))
    (let
        (
            (sender tx-sender)
            (listing-id (var-get listing-nonce))
            (expiry (+ stacks-block-height duration))
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
        (map-set ListingTimeLimits listing-id expiry)
        (var-set listing-nonce (+ listing-id u1))
        (ok listing-id)
    )
)


(define-map BulkDiscounts
    uint
    { min-quantity: uint, discount-percentage: uint }
)

(define-public (set-bulk-discount (listing-id uint) (min-quantity uint) (discount-percentage uint))
    (let
        (
            (listing (unwrap! (map-get? Listings listing-id) ERR-LISTING-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
        (map-set BulkDiscounts listing-id {
            min-quantity: min-quantity,
            discount-percentage: discount-percentage
        })
        (ok true)
    )
)

(define-map Wishlists
    { user: principal, listing-id: uint }
    bool
)

(define-public (add-to-wishlist (listing-id uint))
    (begin
        (map-set Wishlists { user: tx-sender, listing-id: listing-id } true)
        (ok true)
    )
)

(define-public (remove-from-wishlist (listing-id uint))
    (begin
        (map-delete Wishlists { user: tx-sender, listing-id: listing-id })
        (ok true)
    )
)


(define-constant REFERRAL-REWARD-PERCENTAGE u1)

(define-map Referrals
    principal
    principal
)

(define-public (register-referral (referrer principal))
    (begin
        (asserts! (not (is-eq tx-sender referrer)) ERR-NOT-AUTHORIZED)
        (map-set Referrals tx-sender referrer)
        (ok true)
    )
)

(define-public (process-referral-reward (listing-id uint))
    (let
        (
            (listing (unwrap! (map-get? Listings listing-id) ERR-LISTING-NOT-FOUND))
            (referrer (map-get? Referrals tx-sender))
            (reward-amount (/ (* (get price listing) REFERRAL-REWARD-PERCENTAGE) u100))
        )
        (match referrer referrer-principal
            (begin
                (try! (as-contract (stx-transfer? reward-amount (as-contract tx-sender) referrer-principal)))
                (ok true)
            )
            (ok false)
        )
    )
)


(define-map CounterOffers
    { listing-id: uint, buyer: principal }
    { amount: uint, status: (string-ascii 10) }
)

(define-public (make-counter-offer (listing-id uint) (offer-amount uint))
    (let
        (
            (listing (unwrap! (map-get? Listings listing-id) ERR-LISTING-NOT-FOUND))
        )
        (asserts! (get active listing) ERR-LISTING-NOT-FOUND)
        (map-set CounterOffers { listing-id: listing-id, buyer: tx-sender }
            { amount: offer-amount, status: "PENDING" }
        )
        (ok true)
    )
)

(define-public (accept-counter-offer (listing-id uint) (buyer principal))
    (let
        (
            (listing (unwrap! (map-get? Listings listing-id) ERR-LISTING-NOT-FOUND))
            (offer (unwrap! (map-get? CounterOffers { listing-id: listing-id, buyer: buyer }) (err u200)))
        )
        (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
        (map-set CounterOffers { listing-id: listing-id, buyer: buyer }
            (merge offer { status: "ACCEPTED" })
        )
        (ok true)
    )
)

(define-public (purchase-listing-with-affiliate (listing-id uint) (affiliate-code (string-ascii 40)))
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
    (try! (contract-call? .AffiliateProgram record-affiliated-purchase listing-id buyer affiliate-code))
    (ok true)
  )
)





(define-constant BASIC-TIER-FEE u100000000)
(define-constant PREMIUM-TIER-FEE u500000000) 
(define-constant ELITE-TIER-FEE u1000000000)

(define-map SellerTiers
    principal
    {
        tier: (string-ascii 10),
        expiry: uint,
        listings-remaining: uint
    }
)

(define-map TierBenefits
    (string-ascii 10)
    {
        escrow-rate: uint,
        max-listings: uint
    }
)

(define-public (initialize-tier-benefits)
    (begin
        (map-set TierBenefits "BASIC" {escrow-rate: u5, max-listings: u10})
        (map-set TierBenefits "PREMIUM" {escrow-rate: u3, max-listings: u50})
        (map-set TierBenefits "ELITE" {escrow-rate: u1, max-listings: u100})
        (ok true)
    )
)

(define-public (subscribe-to-tier (tier-name (string-ascii 10)))
    (let
        (
            (fee (if (is-eq tier-name "BASIC") 
                BASIC-TIER-FEE
                (if (is-eq tier-name "PREMIUM")
                    PREMIUM-TIER-FEE
                    ELITE-TIER-FEE)))
            (benefits (unwrap! (map-get? TierBenefits tier-name) ERR-INVALID-AMOUNT))
        )
        (try! (stx-transfer? fee tx-sender (as-contract tx-sender)))
        (map-set SellerTiers tx-sender {
            tier: tier-name,
            expiry: (+ stacks-block-height u52560),
            listings-remaining: (get max-listings benefits)
        })
        (ok true)
    )
)

(define-map CategoryPriceHistory
    uint
    {
        total-sales: uint,
        total-value: uint,
        last-updated: uint
    }
)

(define-map ListingPriceAdjustments
    uint
    {
        original-price: uint,
        current-price: uint,
        last-adjustment: uint
    }
)

(define-public (update-dynamic-price (listing-id uint))
    (let
        (
            (listing (unwrap! (map-get? Listings listing-id) ERR-LISTING-NOT-FOUND))
            (category-id (unwrap! (map-get? ListingCategories listing-id) ERR-LISTING-NOT-FOUND))
            (price-history (default-to {total-sales: u0, total-value: u0, last-updated: u0} 
                (map-get? CategoryPriceHistory category-id)))
            (avg-price (if (> (get total-sales price-history) u0)
                (/ (get total-value price-history) (get total-sales price-history))
                u0))
            (new-price (if (> avg-price u0)
                (+ (get price listing) (/ (- avg-price (get price listing)) u10))
                (get price listing)))
        )
        (map-set ListingPriceAdjustments listing-id {
            original-price: (get price listing),
            current-price: new-price,
            last-adjustment: stacks-block-height
        })
        (ok true)
    )
)
