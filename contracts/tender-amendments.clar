;; Tender Amendment System
;; Enables controlled modifications to active tenders with transparency and fairness

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u400))
(define-constant err-amendment-not-found (err u401))
(define-constant err-invalid-tender-status (err u402))
(define-constant err-amendment-window-closed (err u403))
(define-constant err-too-many-amendments (err u404))
(define-constant err-invalid-amendment-type (err u405))
(define-constant err-grace-period-active (err u406))
(define-constant err-already-withdrawn (err u407))

(define-data-var next-amendment-id uint u1)
(define-data-var amendment-window-blocks uint u1440) ;; 10 days in blocks  
(define-data-var grace-period-blocks uint u288) ;; 2 days for bid withdrawal
(define-data-var max-amendments-per-tender uint u5)

;; Amendment tracking for each tender
(define-map tender-amendments
  { tender-id: uint }
  { 
    amendment-count: uint,
    last-amendment-block: uint,
    total-notifications-sent: uint
  }
)

;; Individual amendment records
(define-map amendments
  { amendment-id: uint }
  {
    tender-id: uint,
    amendment-type: (string-ascii 30),
    original-value: (string-utf8 200),
    new-value: (string-utf8 200),
    reason: (string-utf8 300),
    proposed-by: principal,
    proposed-at: uint,
    status: (string-ascii 20),
    approved-at: (optional uint),
    effective-at: (optional uint),
    impact-level: (string-ascii 20)
  }
)

;; Read-only functions
(define-read-only (get-amendment (amendment-id uint))
  (map-get? amendments { amendment-id: amendment-id }))

(define-read-only (get-tender-amendments (tender-id uint))
  (default-to 
    { amendment-count: u0, last-amendment-block: u0, total-notifications-sent: u0 }
    (map-get? tender-amendments { tender-id: tender-id })))

(define-read-only (is-amendment-window-open (tender-id uint))
  (let ((tender-data (unwrap! (contract-call? .procurement-system get-tender tender-id) false)))
    (and 
      (is-eq (get status tender-data) "open")
      (> (+ (get created-at tender-data) (var-get amendment-window-blocks)) stacks-block-height))))

;; Propose amendment to an active tender
(define-public (propose-amendment 
  (tender-id uint) 
  (amendment-type (string-ascii 30)) 
  (original-value (string-utf8 200)) 
  (new-value (string-utf8 200)) 
  (reason (string-utf8 300))
  (impact-level (string-ascii 20)))
  (let (
    (amendment-id (var-get next-amendment-id))
    (tender-data (unwrap! (contract-call? .procurement-system get-tender tender-id) err-amendment-not-found))
    (current-amendments (get-tender-amendments tender-id))
  )
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (is-eq (get status tender-data) "open") err-invalid-tender-status)
    (asserts! (is-amendment-window-open tender-id) err-amendment-window-closed)
    (asserts! (< (get amendment-count current-amendments) (var-get max-amendments-per-tender)) err-too-many-amendments)
    
    ;; Record amendment proposal
    (map-set amendments
      { amendment-id: amendment-id }
      {
        tender-id: tender-id,
        amendment-type: amendment-type,
        original-value: original-value,
        new-value: new-value,
        reason: reason,
        proposed-by: tx-sender,
        proposed-at: stacks-block-height,
        status: "proposed",
        approved-at: none,
        effective-at: none,
        impact-level: impact-level
      })
    
    ;; Update tender amendment tracking
    (map-set tender-amendments
      { tender-id: tender-id }
      {
        amendment-count: (+ (get amendment-count current-amendments) u1),
        last-amendment-block: stacks-block-height,
        total-notifications-sent: (get total-notifications-sent current-amendments)
      })
    
    (var-set next-amendment-id (+ amendment-id u1))
    (ok amendment-id)))

;; Get amendment history for a tender
(define-read-only (get-tender-amendment-history (tender-id uint))
  (let ((amendments-info (get-tender-amendments tender-id)))
    {
      tender-id: tender-id,
      total-amendments: (get amendment-count amendments-info),
      last-amendment: (get last-amendment-block amendments-info),
      notifications-sent: (get total-notifications-sent amendments-info),
      amendment-window-open: (is-amendment-window-open tender-id)
    }))

;; Update amendment system parameters
(define-public (update-amendment-parameters (window-blocks uint) (grace-blocks uint) (max-amendments uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (and (> window-blocks u144) (> grace-blocks u144)) (err u412))
    (asserts! (<= max-amendments u10) (err u413))
    
    (var-set amendment-window-blocks window-blocks)
    (var-set grace-period-blocks grace-blocks)
    (var-set max-amendments-per-tender max-amendments)
    (ok true)))
