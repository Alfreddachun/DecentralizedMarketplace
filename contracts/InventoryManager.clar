;; InventoryManager Contract - Advanced stock tracking and pre-order system
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-LISTING-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-STOCK (err u102))
(define-constant ERR-INVALID-QUANTITY (err u103))
(define-constant ERR-RESERVATION-NOT-FOUND (err u104))
(define-constant ERR-ALREADY-EXISTS (err u105))
(define-constant ERR-PREORDER-NOT-FOUND (err u106))
(define-constant ERR-INVALID-STOCK-LEVEL (err u107))

;; Stock tracking for each listing
(define-map InventoryStock
    uint ;; listing-id
    {
        total-stock: uint,
        available-stock: uint,
        reserved-stock: uint,
        low-stock-threshold: uint,
        allow-preorders: bool,
        last-restocked: uint
    }
)

;; Temporary reservations during purchase process
(define-map StockReservations
    { listing-id: uint, buyer: principal }
    {
        quantity-reserved: uint,
        reservation-timestamp: uint,
        expiry-block: uint
    }
)

;; Pre-orders for out-of-stock items
(define-map PreOrders
    uint ;; preorder-id
    {
        listing-id: uint,
        buyer: principal,
        quantity: uint,
        price-locked: uint,
        order-timestamp: uint,
        fulfilled: bool,
        priority-score: uint
    }
)

;; Batch operations for multiple listings
(define-map BatchOperations
    uint ;; batch-id
    {
        seller: principal,
        operation-type: (string-ascii 20),
        listings-count: uint,
        completed-count: uint,
        created-at: uint
    }
)

;; Stock alerts for sellers
(define-map StockAlerts
    { seller: principal, listing-id: uint }
    {
        alert-type: (string-ascii 15),
        triggered-at: uint,
        acknowledged: bool
    }
)

;; Restocking notifications for buyers
(define-map RestockNotifications
    { listing-id: uint, buyer: principal }
    {
        notification-active: bool,
        registered-at: uint
    }
)

;; Global counters
(define-data-var preorder-nonce uint u0)
(define-data-var batch-operation-nonce uint u0)
(define-data-var reservation-timeout-blocks uint u144) ;; ~24 hours

;; Initialize inventory for a new listing
(define-public (initialize-inventory (listing-id uint) (initial-stock uint) (low-threshold uint))
    (let
        (
            (listing-info (unwrap! (contract-call? .DecentralizedMarketplace get-listing listing-id) ERR-LISTING-NOT-FOUND))
            (seller (get seller listing-info))
        )
        ;; Only seller can initialize inventory
        (asserts! (is-eq tx-sender seller) ERR-NOT-AUTHORIZED)
        (asserts! (> initial-stock u0) ERR-INVALID-QUANTITY)
        (asserts! (is-none (map-get? InventoryStock listing-id)) ERR-ALREADY-EXISTS)
        
        (map-set InventoryStock listing-id {
            total-stock: initial-stock,
            available-stock: initial-stock,
            reserved-stock: u0,
            low-stock-threshold: low-threshold,
            allow-preorders: true,
            last-restocked: stacks-block-height
        })
        (ok true)
    )
)

;; Reserve stock during purchase process
(define-public (reserve-stock (listing-id uint) (quantity uint))
    (let
        (
            (stock-info (unwrap! (map-get? InventoryStock listing-id) ERR-LISTING-NOT-FOUND))
            (buyer tx-sender)
            (expiry-block (+ stacks-block-height (var-get reservation-timeout-blocks)))
            (existing-reservation (map-get? StockReservations { listing-id: listing-id, buyer: buyer }))
        )
        ;; Check if enough stock available
        (asserts! (>= (get available-stock stock-info) quantity) ERR-INSUFFICIENT-STOCK)
        (asserts! (> quantity u0) ERR-INVALID-QUANTITY)
        (asserts! (is-none existing-reservation) ERR-ALREADY-EXISTS)
        
        ;; Update stock levels
        (map-set InventoryStock listing-id (merge stock-info {
            available-stock: (- (get available-stock stock-info) quantity),
            reserved-stock: (+ (get reserved-stock stock-info) quantity)
        }))
        
        ;; Create reservation
        (map-set StockReservations { listing-id: listing-id, buyer: buyer } {
            quantity-reserved: quantity,
            reservation-timestamp: stacks-block-height,
            expiry-block: expiry-block
        })
        
        ;; Check for low stock alert
        (try! (check-and-create-stock-alert listing-id))
        (ok true)
    )
)

;; Confirm stock reservation (complete purchase)
(define-public (confirm-stock-reservation (listing-id uint) (buyer principal))
    (let
        (
            (reservation (unwrap! (map-get? StockReservations { listing-id: listing-id, buyer: buyer }) ERR-RESERVATION-NOT-FOUND))
            (stock-info (unwrap! (map-get? InventoryStock listing-id) ERR-LISTING-NOT-FOUND))
            (reserved-quantity (get quantity-reserved reservation))
        )
        ;; Update stock levels - remove from reserved
        (map-set InventoryStock listing-id (merge stock-info {
            reserved-stock: (- (get reserved-stock stock-info) reserved-quantity),
            total-stock: (- (get total-stock stock-info) reserved-quantity)
        }))
        
        ;; Remove reservation
        (map-delete StockReservations { listing-id: listing-id, buyer: buyer })
        (ok true)
    )
)

;; Release expired reservations
(define-public (release-expired-reservation (listing-id uint) (buyer principal))
    (let
        (
            (reservation (unwrap! (map-get? StockReservations { listing-id: listing-id, buyer: buyer }) ERR-RESERVATION-NOT-FOUND))
            (stock-info (unwrap! (map-get? InventoryStock listing-id) ERR-LISTING-NOT-FOUND))
            (reserved-quantity (get quantity-reserved reservation))
        )
        ;; Check if reservation has expired
        (asserts! (>= stacks-block-height (get expiry-block reservation)) ERR-NOT-AUTHORIZED)
        
        ;; Return stock to available pool
        (map-set InventoryStock listing-id (merge stock-info {
            available-stock: (+ (get available-stock stock-info) reserved-quantity),
            reserved-stock: (- (get reserved-stock stock-info) reserved-quantity)
        }))
        
        ;; Remove reservation
        (map-delete StockReservations { listing-id: listing-id, buyer: buyer })
        (ok true)
    )
)

;; Create pre-order for out-of-stock items
(define-public (create-preorder (listing-id uint) (quantity uint) (max-price uint))
    (let
        (
            (stock-info (unwrap! (map-get? InventoryStock listing-id) ERR-LISTING-NOT-FOUND))
            (listing-info (unwrap! (contract-call? .DecentralizedMarketplace get-listing listing-id) ERR-LISTING-NOT-FOUND))
            (buyer tx-sender)
            (preorder-id (var-get preorder-nonce))
            (buyer-reputation u100)
        )
        ;; Check if preorders are allowed and stock is insufficient
        (asserts! (get allow-preorders stock-info) ERR-NOT-AUTHORIZED)
        (asserts! (< (get available-stock stock-info) quantity) ERR-INSUFFICIENT-STOCK)
        (asserts! (> quantity u0) ERR-INVALID-QUANTITY)
        
        ;; Create pre-order with priority based on buyer reputation
        (map-set PreOrders preorder-id {
            listing-id: listing-id,
            buyer: buyer,
            quantity: quantity,
            price-locked: max-price,
            order-timestamp: stacks-block-height,
            fulfilled: false,
            priority-score: (+ buyer-reputation stacks-block-height)
        })
        
        (var-set preorder-nonce (+ preorder-id u1))
        (ok preorder-id)
    )
)

;; Restock inventory
(define-public (restock-inventory (listing-id uint) (additional-stock uint))
    (let
        (
            (stock-info (unwrap! (map-get? InventoryStock listing-id) ERR-LISTING-NOT-FOUND))
            (listing-info (unwrap! (contract-call? .DecentralizedMarketplace get-listing listing-id) ERR-LISTING-NOT-FOUND))
            (seller (get seller listing-info))
        )
        ;; Only seller can restock
        (asserts! (is-eq tx-sender seller) ERR-NOT-AUTHORIZED)
        (asserts! (> additional-stock u0) ERR-INVALID-QUANTITY)
        
        ;; Update stock levels
        (map-set InventoryStock listing-id (merge stock-info {
            total-stock: (+ (get total-stock stock-info) additional-stock),
            available-stock: (+ (get available-stock stock-info) additional-stock),
            last-restocked: stacks-block-height
        }))
        (ok true)
    )
)

;; Process pending pre-orders when stock becomes available
(define-private (process-pending-preorders (listing-id uint))
    (let
        (
            (stock-info (unwrap! (map-get? InventoryStock listing-id) ERR-LISTING-NOT-FOUND))
        )
        ;; Simple implementation - in practice would need to iterate through pre-orders by priority
        ;; This is a placeholder for the core logic
        (ok true)
    )
)

;; Check and create stock alerts for low inventory
(define-private (check-and-create-stock-alert (listing-id uint))
    (let
        (
            (stock-info (unwrap! (map-get? InventoryStock listing-id) ERR-LISTING-NOT-FOUND))
            (listing-info (unwrap! (contract-call? .DecentralizedMarketplace get-listing listing-id) ERR-LISTING-NOT-FOUND))
            (seller (get seller listing-info))
        )
        (if (<= (get available-stock stock-info) (get low-stock-threshold stock-info))
            (begin
                (map-set StockAlerts { seller: seller, listing-id: listing-id } {
                    alert-type: "LOW_STOCK",
                    triggered-at: stacks-block-height,
                    acknowledged: false
                })
                (ok true)
            )
            (ok false)
        )
    )
)

;; Trigger restock notifications
(define-private (trigger-restock-notifications (listing-id uint))
    (begin
        ;; Implementation would notify all buyers who registered for restock alerts
        ;; This is a placeholder for the notification logic
        (ok true)
    )
)

;; Batch update stock levels for multiple listings
(define-public (batch-update-stock (listing-ids (list 10 uint)) (stock-levels (list 10 uint)))
    (let
        (
            (batch-id (var-get batch-operation-nonce))
            (listings-count (len listing-ids))
        )
        (asserts! (is-eq (len listing-ids) (len stock-levels)) ERR-INVALID-QUANTITY)
        
        (map-set BatchOperations batch-id {
            seller: tx-sender,
            operation-type: "STOCK_UPDATE",
            listings-count: listings-count,
            completed-count: u0,
            created-at: stacks-block-height
        })
        
        (var-set batch-operation-nonce (+ batch-id u1))
        ;; In practice, would iterate through lists and update each listing
        (ok batch-id)
    )
)

;; Register for restock notifications
(define-public (register-restock-notification (listing-id uint))
    (begin
        (map-set RestockNotifications { listing-id: listing-id, buyer: tx-sender } {
            notification-active: true,
            registered-at: stacks-block-height
        })
        (ok true)
    )
)

;; Acknowledge stock alert
(define-public (acknowledge-stock-alert (listing-id uint))
    (let
        (
            (alert (unwrap! (map-get? StockAlerts { seller: tx-sender, listing-id: listing-id }) ERR-LISTING-NOT-FOUND))
        )
        (map-set StockAlerts { seller: tx-sender, listing-id: listing-id } (merge alert {
            acknowledged: true
        }))
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-inventory-stock (listing-id uint))
    (map-get? InventoryStock listing-id)
)

(define-read-only (get-stock-reservation (listing-id uint) (buyer principal))
    (map-get? StockReservations { listing-id: listing-id, buyer: buyer })
)

(define-read-only (get-preorder (preorder-id uint))
    (map-get? PreOrders preorder-id)
)

(define-read-only (get-batch-operation (batch-id uint))
    (map-get? BatchOperations batch-id)
)

(define-read-only (get-stock-alert (seller principal) (listing-id uint))
    (map-get? StockAlerts { seller: seller, listing-id: listing-id })
)

(define-read-only (check-stock-availability (listing-id uint) (desired-quantity uint))
    (let
        (
            (stock-info (unwrap! (map-get? InventoryStock listing-id) ERR-LISTING-NOT-FOUND))
        )
        (ok {
            available: (>= (get available-stock stock-info) desired-quantity),
            current-stock: (get available-stock stock-info),
            can-preorder: (and 
                (< (get available-stock stock-info) desired-quantity)
                (get allow-preorders stock-info)
            )
        })
    )
)

(define-read-only (get-inventory-summary (listing-id uint))
    (let
        (
            (stock-info (unwrap! (map-get? InventoryStock listing-id) ERR-LISTING-NOT-FOUND))
        )
        (ok {
            total-stock: (get total-stock stock-info),
            available-stock: (get available-stock stock-info),
            reserved-stock: (get reserved-stock stock-info),
            stock-utilization: (if (> (get total-stock stock-info) u0)
                (/ (* (- (get total-stock stock-info) (get available-stock stock-info)) u100) (get total-stock stock-info))
                u0),
            low-stock-alert: (<= (get available-stock stock-info) (get low-stock-threshold stock-info)),
            preorders-enabled: (get allow-preorders stock-info)
        })
    )
)


