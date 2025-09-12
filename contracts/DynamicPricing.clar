;; Dynamic Pricing Engine for DecentralizedMarketplace
;; Enables automated price adjustments based on market conditions

(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-LISTING-NOT-FOUND (err u201))
(define-constant ERR-PRICING-RULE-EXISTS (err u202))
(define-constant ERR-INVALID-PARAMETERS (err u203))
(define-constant ERR-RULE-NOT-FOUND (err u204))
(define-constant ERR-PRICE-BOUNDS-EXCEEDED (err u205))

;; Pricing strategy types
(define-constant STRATEGY-DEMAND-BASED u1)
(define-constant STRATEGY-TIME-BASED u2)
(define-constant STRATEGY-INVENTORY-BASED u3)
(define-constant STRATEGY-COMPETITION-BASED u4)

;; Dynamic pricing rules for listings
(define-map PricingRules uint
  {
    listing-id: uint,
    strategy-type: uint,
    base-price: uint,
    min-price: uint,
    max-price: uint,
    adjustment-percentage: uint,
    active: bool,
    created-at: uint,
    last-updated: uint
  }
)

;; Time-based pricing schedules
(define-map TimeBasedSchedule uint
  {
    rule-id: uint,
    start-block: uint,
    end-block: uint,
    price-multiplier: uint,
    schedule-name: (string-ascii 50)
  }
)

;; Demand tracking for listings
(define-map DemandMetrics uint
  {
    listing-id: uint,
    view-count: uint,
    purchase-attempts: uint,
    successful-purchases: uint,
    last-activity: uint,
    demand-score: uint
  }
)

;; Competition price tracking
(define-map CompetitorPrices { category-id: uint, price-range: uint }
  {
    avg-price: uint,
    min-price: uint,
    max-price: uint,
    listing-count: uint,
    last-updated: uint
  }
)

;; Price change history
(define-map PriceHistory { listing-id: uint, change-id: uint }
  {
    old-price: uint,
    new-price: uint,
    change-reason: (string-ascii 30),
    timestamp: uint,
    strategy-used: uint
  }
)

;; Global counters
(define-data-var pricing-rule-nonce uint u0)
(define-data-var schedule-nonce uint u0)
(define-data-var price-change-nonce uint u0)

;; Configuration
(define-data-var price-update-frequency uint u144) ;; ~24 hours
(define-data-var demand-decay-factor uint u95) ;; 5% decay per period

;; Create dynamic pricing rule for a listing
(define-public (create-pricing-rule 
  (listing-id uint)
  (strategy-type uint)
  (base-price uint)
  (min-price uint)
  (max-price uint)
  (adjustment-percentage uint))
  (let (
    (rule-id (var-get pricing-rule-nonce))
    (listing (unwrap! (contract-call? .DecentralizedMarketplace get-listing listing-id) ERR-LISTING-NOT-FOUND))
  )
    ;; Validate seller authorization
    (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
    
    ;; Validate parameters
    (asserts! (and (>= strategy-type u1) (<= strategy-type u4)) ERR-INVALID-PARAMETERS)
    (asserts! (and (> min-price u0) (<= min-price max-price)) ERR-INVALID-PARAMETERS)
    (asserts! (and (>= base-price min-price) (<= base-price max-price)) ERR-INVALID-PARAMETERS)
    (asserts! (<= adjustment-percentage u50) ERR-INVALID-PARAMETERS) ;; Max 50% adjustment
    (asserts! (is-none (map-get? PricingRules rule-id)) ERR-PRICING-RULE-EXISTS)
    
    ;; Create pricing rule
    (map-set PricingRules rule-id {
      listing-id: listing-id,
      strategy-type: strategy-type,
      base-price: base-price,
      min-price: min-price,
      max-price: max-price,
      adjustment-percentage: adjustment-percentage,
      active: true,
      created-at: stacks-block-height,
      last-updated: stacks-block-height
    })
    
    ;; Initialize demand metrics
    (map-set DemandMetrics listing-id {
      listing-id: listing-id,
      view-count: u0,
      purchase-attempts: u0,
      successful-purchases: u0,
      last-activity: stacks-block-height,
      demand-score: u100
    })
    
    (var-set pricing-rule-nonce (+ rule-id u1))
    (ok rule-id)
  )
)

;; Record market activity to influence pricing
(define-public (record-listing-activity 
  (listing-id uint)
  (activity-type (string-ascii 20)))
  (let (
    (demand-data (default-to {
      listing-id: listing-id,
      view-count: u0,
      purchase-attempts: u0,
      successful-purchases: u0,
      last-activity: u0,
      demand-score: u100
    } (map-get? DemandMetrics listing-id)))
  )
    ;; Update demand metrics based on activity
    (map-set DemandMetrics listing-id
      (if (is-eq activity-type "view")
        (merge demand-data { 
          view-count: (+ (get view-count demand-data) u1),
          last-activity: stacks-block-height
        })
        (if (is-eq activity-type "purchase-attempt")
          (merge demand-data { 
            purchase-attempts: (+ (get purchase-attempts demand-data) u1),
            last-activity: stacks-block-height
          })
          (if (is-eq activity-type "purchase")
            (merge demand-data { 
              successful-purchases: (+ (get successful-purchases demand-data) u1),
              last-activity: stacks-block-height
            })
            demand-data))))
    
    ;; Recalculate demand score
    (try! (update-demand-score listing-id))
    (ok true)
  )
)

;; Calculate and apply dynamic price adjustment
(define-public (apply-dynamic-pricing (listing-id uint))
  (let (
    (listing (unwrap! (contract-call? .DecentralizedMarketplace get-listing listing-id) ERR-LISTING-NOT-FOUND))
    (rule (unwrap! (get-active-rule listing-id) ERR-RULE-NOT-FOUND))
    (demand-data (default-to {
      listing-id: listing-id, view-count: u0, purchase-attempts: u0,
      successful-purchases: u0, last-activity: u0, demand-score: u100
    } (map-get? DemandMetrics listing-id)))
    (new-price (calculate-new-price rule demand-data))
    (change-id (var-get price-change-nonce))
  )
    ;; Validate price bounds
    (asserts! (and (>= new-price (get min-price rule)) 
                   (<= new-price (get max-price rule))) ERR-PRICE-BOUNDS-EXCEEDED)
    
    ;; Record price change
    (map-set PriceHistory { listing-id: listing-id, change-id: change-id } {
      old-price: (get price listing),
      new-price: new-price,
      change-reason: (get-strategy-name (get strategy-type rule)),
      timestamp: stacks-block-height,
      strategy-used: (get strategy-type rule)
    })
    
    ;; Update rule timestamp
    (map-set PricingRules (get-rule-id-by-listing listing-id)
      (merge rule { last-updated: stacks-block-height }))
    
    (var-set price-change-nonce (+ change-id u1))
    (ok new-price)
  )
)

;; Create time-based pricing schedule
(define-public (create-time-schedule 
  (rule-id uint)
  (start-block uint)
  (end-block uint)
  (price-multiplier uint)
  (schedule-name (string-ascii 50)))
  (let (
    (schedule-id (var-get schedule-nonce))
    (rule (unwrap! (map-get? PricingRules rule-id) ERR-RULE-NOT-FOUND))
  )
    ;; Validate parameters
    (asserts! (> end-block start-block) ERR-INVALID-PARAMETERS)
    (asserts! (and (>= price-multiplier u50) (<= price-multiplier u200)) ERR-INVALID-PARAMETERS)
    
    ;; Create schedule
    (map-set TimeBasedSchedule schedule-id {
      rule-id: rule-id,
      start-block: start-block,
      end-block: end-block,
      price-multiplier: price-multiplier,
      schedule-name: schedule-name
    })
    
    (var-set schedule-nonce (+ schedule-id u1))
    (ok schedule-id)
  )
)

;; Helper function to calculate minimum of two values
(define-private (min-val (a uint) (b uint))
  (if (<= a b) a b)
)

;; Update demand score based on activity metrics
(define-private (update-demand-score (listing-id uint))
  (let (
    (demand-data (unwrap! (map-get? DemandMetrics listing-id) ERR-LISTING-NOT-FOUND))
    (view-score (min-val u50 (/ (get view-count demand-data) u10)))
    (purchase-score (* (get successful-purchases demand-data) u30))
    (attempt-ratio (if (> (get purchase-attempts demand-data) u0)
      (/ (* (get successful-purchases demand-data) u100) (get purchase-attempts demand-data))
      u50))
    (new-demand-score (+ view-score purchase-score (/ attempt-ratio u2)))
  )
    (map-set DemandMetrics listing-id 
      (merge demand-data { demand-score: (min-val u1000 new-demand-score) }))
    (ok true)
  )
)

;; Calculate new price based on strategy and market data
(define-private (calculate-new-price (rule { listing-id: uint, strategy-type: uint, base-price: uint, min-price: uint, max-price: uint, adjustment-percentage: uint, active: bool, created-at: uint, last-updated: uint }) (demand { listing-id: uint, view-count: uint, purchase-attempts: uint, successful-purchases: uint, last-activity: uint, demand-score: uint }))
  (let (
    (base-price (get base-price rule))
    (adjustment-pct (get adjustment-percentage rule))
    (demand-factor (/ (get demand-score demand) u100))
  )
    (if (is-eq (get strategy-type rule) STRATEGY-DEMAND-BASED)
      ;; Demand-based pricing
      (+ base-price (/ (* base-price adjustment-pct (- demand-factor u1)) u100))
      ;; Default: return base price for other strategies (simplified)
      base-price
    )
  )
)

;; Helper function to get strategy name
(define-private (get-strategy-name (strategy-type uint))
  (if (is-eq strategy-type STRATEGY-DEMAND-BASED)
    "DEMAND_ADJUSTMENT"
    (if (is-eq strategy-type STRATEGY-TIME-BASED)
      "TIME_ADJUSTMENT"
      (if (is-eq strategy-type STRATEGY-INVENTORY-BASED)
        "INVENTORY_ADJUSTMENT"
        "COMPETITION_ADJUSTMENT")))
)

;; Helper function to get active rule for listing
(define-private (get-active-rule (listing-id uint))
  (map-get? PricingRules (get-rule-id-by-listing listing-id))
)

;; Helper function to get rule ID by listing (simplified)
(define-private (get-rule-id-by-listing (listing-id uint))
  listing-id ;; Simplified - in practice would need proper lookup
)

;; Helper function to filter rules (placeholder)
(define-private (filter-pricing-rules-by-listing (listing-id uint))
  (list listing-id) ;; Simplified implementation
)

;; Read-only functions
(define-read-only (get-pricing-rule (rule-id uint))
  (map-get? PricingRules rule-id)
)

(define-read-only (get-demand-metrics (listing-id uint))
  (map-get? DemandMetrics listing-id)
)

(define-read-only (get-price-history (listing-id uint) (change-id uint))
  (map-get? PriceHistory { listing-id: listing-id, change-id: change-id })
)

(define-read-only (get-time-schedule (schedule-id uint))
  (map-get? TimeBasedSchedule schedule-id)
)

(define-read-only (calculate-suggested-price (listing-id uint))
  (match (get-active-rule listing-id)
    rule (let (
      (demand (default-to { listing-id: listing-id, view-count: u0, purchase-attempts: u0, 
                           successful-purchases: u0, last-activity: u0, demand-score: u100 }
                          (map-get? DemandMetrics listing-id)))
    )
      (ok (calculate-new-price rule demand))
    )
    (err ERR-RULE-NOT-FOUND)
  )
)
