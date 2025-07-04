(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PURCHASE-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-COMPLETED (err u102))
(define-constant ERR-DISPUTE-ACTIVE (err u103))
(define-constant ERR-TIMEOUT-NOT-REACHED (err u104))

(define-constant DEFAULT-ESCROW-TIMEOUT u1008)
(define-constant CONTRACT-OWNER tx-sender)

(define-map PurchaseTimeouts
    uint
    {
        purchase-block: uint,
        timeout-blocks: uint,
        auto-release-enabled: bool,
        completed: bool
    }
)

(define-map EscrowSettings
    principal
    {
        timeout-blocks: uint,
        auto-release-enabled: bool
    }
)

(define-data-var total-auto-releases uint u0)

(define-public (set-escrow-timeout (timeout-blocks uint))
    (begin
        (map-set EscrowSettings tx-sender {
            timeout-blocks: timeout-blocks,
            auto-release-enabled: true
        })
        (ok true)
    )
)

(define-public (disable-auto-release)
    (let
        (
            (current-settings (default-to {timeout-blocks: DEFAULT-ESCROW-TIMEOUT, auto-release-enabled: true} 
                (map-get? EscrowSettings tx-sender)))
        )
        (map-set EscrowSettings tx-sender (merge current-settings {auto-release-enabled: false}))
        (ok true)
    )
)

(define-public (register-purchase-timeout (listing-id uint) (seller principal))
    (let
        (
            (seller-settings (default-to {timeout-blocks: DEFAULT-ESCROW-TIMEOUT, auto-release-enabled: true} 
                (map-get? EscrowSettings seller)))
        )
        (asserts! (get auto-release-enabled seller-settings) (ok false))
        (map-set PurchaseTimeouts listing-id {
            purchase-block: stacks-block-height,
            timeout-blocks: (get timeout-blocks seller-settings),
            auto-release-enabled: true,
            completed: false
        })
        (ok true)
    )
)

(define-public (trigger-auto-release (listing-id uint))
    (let
        (
            (timeout-info (unwrap! (map-get? PurchaseTimeouts listing-id) ERR-PURCHASE-NOT-FOUND))
            (expiry-block (+ (get purchase-block timeout-info) (get timeout-blocks timeout-info)))
        )
        (asserts! (get auto-release-enabled timeout-info) ERR-NOT-AUTHORIZED)
        (asserts! (not (get completed timeout-info)) ERR-ALREADY-COMPLETED)
        (asserts! (>= stacks-block-height expiry-block) ERR-TIMEOUT-NOT-REACHED)
        (asserts! (is-none (contract-call? .DecentralizedMarketplace get-dispute listing-id)) ERR-DISPUTE-ACTIVE)
        
        (map-set PurchaseTimeouts listing-id (merge timeout-info {completed: true}))
        (var-set total-auto-releases (+ (var-get total-auto-releases) u1))
        (try! (contract-call? .DecentralizedMarketplace complete-purchase listing-id))
        (ok true)
    )
)

(define-public (mark-manual-completion (listing-id uint))
    (let
        (
            (timeout-info (unwrap! (map-get? PurchaseTimeouts listing-id) ERR-PURCHASE-NOT-FOUND))
        )
        (map-set PurchaseTimeouts listing-id (merge timeout-info {completed: true}))
        (ok true)
    )
)

(define-public (check-auto-release-eligibility (listing-id uint))
    (let
        (
            (timeout-info (unwrap! (map-get? PurchaseTimeouts listing-id) ERR-PURCHASE-NOT-FOUND))
            (expiry-block (+ (get purchase-block timeout-info) (get timeout-blocks timeout-info)))
        )
        (ok {
            eligible: (and 
                (get auto-release-enabled timeout-info)
                (not (get completed timeout-info))
                (>= stacks-block-height expiry-block)
                (is-none (contract-call? .DecentralizedMarketplace get-dispute listing-id))
            ),
            blocks-remaining: (if (>= stacks-block-height expiry-block) u0 (- expiry-block stacks-block-height)),
            expiry-block: expiry-block
        })
    )
)

(define-read-only (get-purchase-timeout (listing-id uint))
    (map-get? PurchaseTimeouts listing-id)
)

(define-read-only (get-seller-escrow-settings (seller principal))
    (map-get? EscrowSettings seller)
)

(define-read-only (get-total-auto-releases)
    (var-get total-auto-releases)
)

(define-read-only (get-default-timeout)
    DEFAULT-ESCROW-TIMEOUT
)