;; AffiliateProgram Contract
;; This contract allows sellers to create affiliate programs for their listings,
;; enabling other users to earn commissions by promoting products.

;; ---
;; Constants
;; ---
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-LISTING-NOT-FOUND (err u404))
(define-constant ERR-AFFILIATE-CODE-EXISTS (err u409))
(define-constant ERR-INVALID-COMMISSION (err u422))
(define-constant ERR-INVALID-AFFILIATE-CODE (err u423))
(define-constant ERR-PURCHASE-NOT-FOUND (err u424))
(define-constant ERR-REWARD-ALREADY-PROCESSED (err u425))

;; ---
;; Data Storage
;; ---

;; Maps affiliate codes to their respective listing and creator.
;; (map { affiliate-code: (string-ascii 40) } { listing-id: uint, creator: principal })
(define-map AffiliateCodes
  (string-ascii 40)
  {
    listing-id: uint,
    creator: principal
  }
)

;; Stores the commission rate for each listing's affiliate program.
;; (map { listing-id: uint } { commission-percentage: uint })
(define-map ListingCommissions
  uint
  {
    commission-percentage: uint
  }
)

;; Tracks which affiliate code was used for a specific purchase.
;; The key is a tuple of the buyer and the listing ID.
;; (map { buyer: principal, listing-id: uint } { affiliate-code: (string-ascii 40) })
(define-map PurchaseAffiliations
  {
    buyer: principal,
    listing-id: uint
  }
  {
    affiliate-code: (string-ascii 40)
  }
)

;; Tracks whether a reward for a purchase has been processed.
;; (map { buyer: principal, listing-id: uint } bool)
(define-map RewardStatus
  {
    buyer: principal,
    listing-id: uint
  }
  bool
)

;; ---
;; Helper Functions
;; ---

;; Verifies that the transaction sender is the seller of a given listing.
(define-private (is-seller (listing-seller principal))
  (is-eq tx-sender listing-seller)
)

;; ---
;; Public Functions
;; ---

;; Creates an affiliate program for a specific listing.
;; Only the seller of the listing can call this function.
;; @param listing-id: The ID of the listing.
;; @param commission-percentage: The commission rate for affiliates (e.g., u5 for 5%).
(define-public (create-affiliate-program (listing-id uint) (listing-seller principal) (commission-percentage uint))
  (begin
    (asserts! (is-seller listing-seller) ERR-NOT-AUTHORIZED)
    ;; Commission must be between 1% and 50%
    (asserts! (and (> commission-percentage u0) (<= commission-percentage u50)) ERR-INVALID-COMMISSION)
    (map-set ListingCommissions listing-id { commission-percentage: commission-percentage })
    (ok true)
  )
)

;; Allows a user to generate a unique affiliate code for a listing.
;; @param listing-id: The ID of the listing to promote.
;; @param affiliate-code: A unique string to identify the affiliate.
(define-public (generate-affiliate-code (listing-id uint) (affiliate-code (string-ascii 40)))
  (begin
    ;; Ensure the listing has an affiliate program.
    (asserts! (is-some (map-get? ListingCommissions listing-id)) ERR-LISTING-NOT-FOUND)
    ;; Ensure the affiliate code is unique.
    (asserts! (is-none (map-get? AffiliateCodes affiliate-code)) ERR-AFFILIATE-CODE-EXISTS)
    (map-set AffiliateCodes affiliate-code {
      listing-id: listing-id,
      creator: tx-sender
    })
    (ok true)
  )
)

;; Associates a purchase with an affiliate code.
;; This should be called by the main marketplace contract during the purchase flow.
;; @param listing-id: The ID of the listing being purchased.
;; @param buyer: The principal of the buyer.
;; @param affiliate-code: The affiliate code used for the purchase.
(define-public (record-affiliated-purchase (listing-id uint) (buyer principal) (affiliate-code (string-ascii 40)))
  (let
    (
      (code-details (unwrap! (map-get? AffiliateCodes affiliate-code) ERR-INVALID-AFFILIATE-CODE))
    )
    ;; Ensure the affiliate code matches the listing being purchased.
    (asserts! (is-eq (get listing-id code-details) listing-id) ERR-INVALID-AFFILIATE-CODE)
    (map-set PurchaseAffiliations { buyer: buyer, listing-id: listing-id } { affiliate-code: affiliate-code })
    (ok true)
  )
)

;; Processes the commission payout for an affiliate after a purchase is completed.
;; This should be called after the main marketplace confirms the purchase is complete and funds are released.
;; @param listing-id: The ID of the completed purchase.
;; @param buyer: The principal of the buyer who made the purchase.
(define-public (process-affiliate-reward (listing-id uint) (buyer principal) (price uint))
  (let
    (
      (purchase-affiliation (unwrap! (map-get? PurchaseAffiliations { buyer: buyer, listing-id: listing-id }) ERR-PURCHASE-NOT-FOUND))
      (affiliate-code (get affiliate-code purchase-affiliation))
      (code-details (unwrap! (map-get? AffiliateCodes affiliate-code) ERR-INVALID-AFFILIATE-CODE))
      (affiliate (get creator code-details))
      (commission-details (unwrap! (map-get? ListingCommissions listing-id) ERR-LISTING-NOT-FOUND))
      (commission-percentage (get commission-percentage commission-details))
      (reward-amount (/ (* price commission-percentage) u100))
    )
    ;; Ensure the reward has not already been processed.
    (asserts! (is-none (map-get? RewardStatus { buyer: buyer, listing-id: listing-id })) ERR-REWARD-ALREADY-PROCESSED)

    ;; Transfer the commission from the contract to the affiliate.
    (try! (as-contract (stx-transfer? reward-amount (as-contract tx-sender) affiliate)))

    ;; Mark the reward as processed.
    (map-set RewardStatus { buyer: buyer, listing-id: listing-id } true)
    (print {
      action: "affiliate-reward-processed",
      listing-id: listing-id,
      buyer: buyer,
      affiliate: affiliate,
      reward: reward-amount
    })
    (ok true)
  )
)

;; ---
;; Read-Only Functions
;; ---

;; Retrieves the details for a given affiliate code.
;; @param affiliate-code: The affiliate code to query.
(define-read-only (get-affiliate-code-details (affiliate-code (string-ascii 40)))
  (map-get? AffiliateCodes affiliate-code)
)

;; Retrieves the commission details for a listing.
;; @param listing-id: The ID of the listing.
(define-read-only (get-listing-commission (listing-id uint))
  (map-get? ListingCommissions listing-id)
)

;; Checks if a reward for a specific purchase has been processed.
;; @param listing-id: The ID of the listing.
;; @param buyer: The principal of the buyer.
(define-read-only (get-reward-status (listing-id uint) (buyer principal))
  (default-to false (map-get? RewardStatus { buyer: buyer, listing-id: listing-id }))
)
