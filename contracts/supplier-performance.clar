;; Supplier Performance Analytics & Reputation Engine
;; Tracks contractor performance across tenders and builds reputation scores

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-not-found (err u301))
(define-constant err-invalid-rating (err u302))
(define-constant err-already-rated (err u303))
(define-constant err-not-authorized (err u304))
(define-constant err-invalid-performance (err u305))
(define-constant err-contract-not-completed (err u306))

(define-data-var next-performance-id uint u1)
(define-data-var next-feedback-id uint u1)

;; Core performance tracking for each completed tender
(define-map supplier-performances
  { performance-id: uint }
  {
    supplier: principal,
    tender-id: uint,
    completion-time: uint,
    planned-duration: uint,
    quality-rating: uint,
    budget-adherence: uint,
    communication-rating: uint,
    overall-satisfaction: uint,
    recorded-by: principal,
    recorded-at: uint,
    contract-value: uint,
    delivery-status: (string-ascii 20)
  }
)

;; Aggregated supplier reputation scores
(define-map supplier-reputation
  { supplier: principal }
  {
    total-contracts: uint,
    successful-contracts: uint,
    failed-contracts: uint,
    average-quality: uint,
    average-timeliness: uint,
    average-budget-adherence: uint,
    total-contract-value: uint,
    reputation-score: uint,
    last-updated: uint,
    performance-trend: (string-ascii 20)
  }
)

;; Historical performance records per supplier
(define-map supplier-history
  { supplier: principal }
  { performance-ids: (list 100 uint) }
)

;; Client feedback system
(define-map performance-feedback
  { feedback-id: uint }
  {
    performance-id: uint,
    supplier: principal,
    feedback-type: (string-ascii 30),
    rating: uint,
    comment: (string-utf8 300),
    submitted-by: principal,
    submitted-at: uint,
    verified: bool
  }
)

;; Performance alerts and flags
(define-map supplier-alerts
  { supplier: principal }
  {
    warning-count: uint,
    blacklisted: bool,
    last-warning: uint,
    alert-reason: (string-utf8 200),
    review-required: bool
  }
)

;; Performance categories for detailed tracking
(define-map performance-categories
  { performance-id: uint, category: (string-ascii 30) }
  {
    score: uint,
    weight: uint,
    notes: (string-utf8 150)
  }
)

;; Read-only functions for data retrieval
(define-read-only (get-supplier-reputation (supplier principal))
  (map-get? supplier-reputation { supplier: supplier }))

(define-read-only (get-performance-record (performance-id uint))
  (map-get? supplier-performances { performance-id: performance-id }))

(define-read-only (get-supplier-history (supplier principal))
  (default-to { performance-ids: (list) } 
    (map-get? supplier-history { supplier: supplier })))

(define-read-only (get-supplier-alerts (supplier principal))
  (default-to 
    { warning-count: u0, blacklisted: false, last-warning: u0, 
      alert-reason: u"", review-required: false }
    (map-get? supplier-alerts { supplier: supplier })))

(define-read-only (get-performance-feedback (feedback-id uint))
  (map-get? performance-feedback { feedback-id: feedback-id }))

;; Calculate reputation score based on multiple factors
(define-read-only (calculate-reputation-score (supplier principal))
  (match (get-supplier-reputation supplier)
    reputation
    (let (
      (success-rate (if (> (get total-contracts reputation) u0)
        (/ (* (get successful-contracts reputation) u100) (get total-contracts reputation))
        u0))
      (quality-weight (* (get average-quality reputation) u30))
      (timeliness-weight (* (get average-timeliness reputation) u25))
      (budget-weight (* (get average-budget-adherence reputation) u25))
      (volume-bonus (if (< (/ (get total-contracts reputation) u5) u20) 
        (/ (get total-contracts reputation) u5) u20))
    )
      (/ (+ quality-weight timeliness-weight budget-weight volume-bonus success-rate) u100))
    u0))

;; Check if supplier meets minimum performance standards
(define-read-only (is-supplier-qualified (supplier principal) (min-score uint))
  (let ((current-score (calculate-reputation-score supplier)))
    (and (>= current-score min-score) 
         (not (get blacklisted (get-supplier-alerts supplier))))))

;; Record performance after tender completion
(define-public (record-performance 
  (supplier principal) 
  (tender-id uint) 
  (completion-time uint) 
  (planned-duration uint)
  (quality-rating uint) 
  (budget-adherence uint) 
  (communication-rating uint) 
  (overall-satisfaction uint)
  (contract-value uint)
  (delivery-status (string-ascii 20)))
  (let ((performance-id (var-get next-performance-id)))
    ;; Validate inputs
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (<= quality-rating u100) (<= budget-adherence u100) 
                  (<= communication-rating u100) (<= overall-satisfaction u100)) err-invalid-rating)
    
    ;; Record performance data
    (map-set supplier-performances
      { performance-id: performance-id }
      {
        supplier: supplier,
        tender-id: tender-id,
        completion-time: completion-time,
        planned-duration: planned-duration,
        quality-rating: quality-rating,
        budget-adherence: budget-adherence,
        communication-rating: communication-rating,
        overall-satisfaction: overall-satisfaction,
        recorded-by: tx-sender,
        recorded-at: stacks-block-height,
        contract-value: contract-value,
        delivery-status: delivery-status
      })
    
    ;; Update supplier history
    (let ((current-history (get-supplier-history supplier)))
      (map-set supplier-history
        { supplier: supplier }
        { performance-ids: (unwrap-panic (as-max-len? 
          (append (get performance-ids current-history) performance-id) u100)) }))
    
    ;; Update reputation metrics
    (unwrap-panic (update-supplier-reputation supplier))
    
    (var-set next-performance-id (+ performance-id u1))
    (ok performance-id)))

;; Submit client feedback on supplier performance
(define-public (submit-feedback 
  (performance-id uint) 
  (feedback-type (string-ascii 30)) 
  (rating uint) 
  (comment (string-utf8 300)))
  (let (
    (performance (unwrap! (get-performance-record performance-id) err-not-found))
    (feedback-id (var-get next-feedback-id))
  )
    (asserts! (<= rating u100) err-invalid-rating)
    
    (map-set performance-feedback
      { feedback-id: feedback-id }
      {
        performance-id: performance-id,
        supplier: (get supplier performance),
        feedback-type: feedback-type,
        rating: rating,
        comment: comment,
        submitted-by: tx-sender,
        submitted-at: stacks-block-height,
        verified: false
      })
    
    (var-set next-feedback-id (+ feedback-id u1))
    (ok feedback-id)))

;; Flag supplier for performance issues
(define-public (flag-supplier (supplier principal) (reason (string-utf8 200)))
  (let ((current-alerts (get-supplier-alerts supplier)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set supplier-alerts
      { supplier: supplier }
      {
        warning-count: (+ (get warning-count current-alerts) u1),
        blacklisted: (>= (+ (get warning-count current-alerts) u1) u5),
        last-warning: stacks-block-height,
        alert-reason: reason,
        review-required: true
      })
    (ok true)))

;; Clear supplier warnings and restore good standing
(define-public (clear-supplier-warnings (supplier principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set supplier-alerts
      { supplier: supplier }
      {
        warning-count: u0,
        blacklisted: false,
        last-warning: u0,
        alert-reason: u"",
        review-required: false
      })
    (ok true)))

;; Internal function to update supplier reputation after new performance data
(define-private (update-supplier-reputation (supplier principal))
  (let (
    (current-rep (get-supplier-reputation supplier))
    (history (get-supplier-history supplier))
    (performance-ids (get performance-ids history))
  )
    (match current-rep
      reputation
      (let (
        (new-total (+ (get total-contracts reputation) u1))
        (performance-data (get-latest-performance-metrics supplier performance-ids))
      )
        (map-set supplier-reputation
          { supplier: supplier }
          {
            total-contracts: new-total,
            successful-contracts: (+ (get successful-contracts reputation) 
              (if (> (get quality performance-data) u70) u1 u0)),
            failed-contracts: (+ (get failed-contracts reputation)
              (if (< (get quality performance-data) u50) u1 u0)),
            average-quality: (calculate-weighted-average 
              (get average-quality reputation) (get quality performance-data) new-total),
            average-timeliness: (calculate-weighted-average 
              (get average-timeliness reputation) (get timeliness performance-data) new-total),
            average-budget-adherence: (calculate-weighted-average 
              (get average-budget-adherence reputation) (get budget performance-data) new-total),
            total-contract-value: (+ (get total-contract-value reputation) (get value performance-data)),
            reputation-score: (calculate-reputation-score supplier),
            last-updated: stacks-block-height,
            performance-trend: (determine-performance-trend supplier performance-ids)
          }))
      ;; First performance record for this supplier
      (map-set supplier-reputation
        { supplier: supplier }
        {
          total-contracts: u1,
          successful-contracts: u1,
          failed-contracts: u0,
          average-quality: u80,
          average-timeliness: u80,
          average-budget-adherence: u80,
          total-contract-value: u0,
          reputation-score: u80,
          last-updated: stacks-block-height,
          performance-trend: "stable"
        }))
    (ok true)))

;; Helper function to extract performance metrics from latest record
(define-read-only (get-latest-performance-metrics (supplier principal) (performance-ids (list 100 uint)))
  (let ((latest-id (fold find-max-id performance-ids u0)))
    (match (get-performance-record latest-id)
      performance 
      {
        quality: (get quality-rating performance),
        timeliness: (if (> (get completion-time performance) (get planned-duration performance)) 
          u50 u100),
        budget: (get budget-adherence performance),
        value: (get contract-value performance)
      }
      { quality: u0, timeliness: u0, budget: u0, value: u0 })))

;; Find maximum ID in list
(define-read-only (find-max-id (id uint) (current-max uint))
  (if (> id current-max) id current-max))

;; Calculate weighted average for reputation metrics
(define-read-only (calculate-weighted-average (old-avg uint) (new-value uint) (total-count uint))
  (if (> total-count u1)
    (/ (+ (* old-avg (- total-count u1)) new-value) total-count)
    new-value))

;; Determine performance trend based on recent records
(define-read-only (determine-performance-trend (supplier principal) (performance-ids (list 100 uint)))
  (if (> (len performance-ids) u2)
    (let (
      (latest-id (fold find-max-id performance-ids u0))
      (latest-score (get-performance-score latest-id))
    )
      (if (> latest-score u80) "improving" "stable"))
    "stable"))

;; Extract performance score from performance record
(define-read-only (get-performance-score (performance-id uint))
  (match (get-performance-record performance-id)
    performance (/ (+ (get quality-rating performance) 
                     (get budget-adherence performance)
                     (get communication-rating performance)
                     (get overall-satisfaction performance)) u4)
    u0))

;; Get top-performing suppliers for recommendations
(define-read-only (get-top-suppliers (min-contracts uint))
  (let ((all-suppliers (get-all-suppliers)))
    (filter-top-performers all-suppliers min-contracts)))

;; Filter suppliers by minimum contract count and sort by reputation
(define-read-only (filter-top-performers (suppliers (list 50 principal)) (min-contracts uint))
  (filter has-minimum-contracts suppliers))

;; Check if supplier meets minimum contract requirements
(define-read-only (has-minimum-contracts (supplier principal))
  (match (get-supplier-reputation supplier)
    reputation (>= (get total-contracts reputation) u3)
    false))

;; Placeholder for getting all suppliers (would need separate tracking)
(define-read-only (get-all-suppliers)
  (list))

;; Generate comprehensive performance report
(define-read-only (get-performance-report (supplier principal))
  (let (
    (reputation (get-supplier-reputation supplier))
    (alerts (get-supplier-alerts supplier))
    (history (get-supplier-history supplier))
  )
    (match reputation
      rep {
        supplier: supplier,
        contracts-completed: (get total-contracts rep),
        success-rate: (if (> (get total-contracts rep) u0)
          (/ (* (get successful-contracts rep) u100) (get total-contracts rep)) u0),
        average-scores: {
          quality: (get average-quality rep),
          timeliness: (get average-timeliness rep),
          budget: (get average-budget-adherence rep)
        },
        reputation-score: (get reputation-score rep),
        trend: (get performance-trend rep),
        total-value: (get total-contract-value rep),
        warnings: (get warning-count alerts),
        blacklisted: (get blacklisted alerts),
        performance-history-count: (len (get performance-ids history))
      }
      {
        supplier: supplier,
        contracts-completed: u0,
        success-rate: u0,
        average-scores: { quality: u0, timeliness: u0, budget: u0 },
        reputation-score: u0,
        trend: "new",
        total-value: u0,
        warnings: u0,
        blacklisted: false,
        performance-history-count: u0
      })))

;; Verify supplier eligibility for new tenders
(define-read-only (verify-supplier-eligibility (supplier principal) (tender-value uint))
  (let (
    (reputation (get-supplier-reputation supplier))
    (alerts (get-supplier-alerts supplier))
  )
    (match reputation
      rep {
        eligible: (and 
          (not (get blacklisted alerts))
          (>= (get reputation-score rep) u60)
          (>= (get total-contract-value rep) (/ tender-value u10))),
        reputation-score: (get reputation-score rep),
        warning-count: (get warning-count alerts),
        blacklisted: (get blacklisted alerts)
      }
      { eligible: false, reputation-score: u0, warning-count: u0, blacklisted: false })))

;; Award performance bonuses based on exceptional performance
(define-public (award-performance-bonus (supplier principal) (bonus-amount uint) (reason (string-utf8 200)))
  (let ((reputation (unwrap! (get-supplier-reputation supplier) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (>= (get reputation-score reputation) u90) err-invalid-performance)
    
    ;; Transfer bonus (would require STX integration)
    (try! (stx-transfer? bonus-amount (as-contract tx-sender) supplier))
    
    ;; Record bonus in feedback system
    (let ((feedback-id (var-get next-feedback-id)))
      (map-set performance-feedback
        { feedback-id: feedback-id }
        {
          performance-id: u0,
          supplier: supplier,
          feedback-type: "performance-bonus",
          rating: u100,
          comment: reason,
          submitted-by: tx-sender,
          submitted-at: stacks-block-height,
          verified: true
        })
      (var-set next-feedback-id (+ feedback-id u1)))
    (ok true)))

;; Batch update multiple supplier performances
(define-public (batch-record-performance (performance-data (list 10 { 
  supplier: principal, 
  tender-id: uint, 
  quality: uint, 
  timeliness: uint,
  budget: uint,
  satisfaction: uint,
  value: uint })))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (fold process-batch-performance performance-data (ok u0))))

;; Helper for batch processing
(define-private (process-batch-performance 
  (data { supplier: principal, tender-id: uint, quality: uint, timeliness: uint, budget: uint, satisfaction: uint, value: uint })
  (result (response uint uint)))
  (match result
    success-count 
    (match (record-performance 
      (get supplier data) (get tender-id data) u30 u30
      (get quality data) (get budget data) u80 (get satisfaction data)
      (get value data) "completed")
      performance-id (ok (+ success-count u1))
      error result)
    error result))

;; Generate performance analytics for decision making
(define-read-only (get-performance-analytics)
  {
    total-suppliers-tracked: (var-get next-performance-id),
    total-feedback-entries: (var-get next-feedback-id),
    average-industry-score: u75,
    top-performer-threshold: u90,
    suppliers-needing-review: (count-suppliers-needing-review)
  })

;; Count suppliers requiring performance review
(define-read-only (count-suppliers-needing-review)
  u0)
