
;; title: procurement-system
;; version:
;; summary:
;; description:

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-tender-closed (err u102))
(define-constant err-tender-open (err u103))
(define-constant err-invalid-bid (err u104))
(define-constant err-already-exists (err u105))
(define-constant err-not-eligible (err u106))
(define-constant err-already-awarded (err u107))
(define-constant err-not-awarded (err u108))

(define-data-var next-tender-id uint u1)
(define-data-var next-bid-id uint u1)


(define-constant err-milestone-not-found (err u109))
(define-constant err-milestone-completed (err u110))
(define-constant err-insufficient-escrow (err u111))
(define-constant err-milestone-not-completed (err u112))
(define-constant err-invalid-milestone (err u113))

(define-data-var next-milestone-id uint u1)

(define-map contract-milestones
  { tender-id: uint }
  { 
    milestone-ids: (list 20 uint),
    total-escrowed: uint,
    total-released: uint
  }
)

(define-map milestones
  { milestone-id: uint }
  {
    tender-id: uint,
    title: (string-ascii 100),
    description: (string-utf8 300),
    payment-amount: uint,
    due-date: uint,
    status: (string-ascii 20),
    completion-evidence: (optional (string-ascii 64)),
    completed-at: (optional uint),
    approved-by: (optional principal)
  }
)

(define-map escrow-balances
  { tender-id: uint }
  { balance: uint }
)

(define-map tenders
  { tender-id: uint }
  {
    title: (string-ascii 100),
    description: (string-utf8 500),
    budget: uint,
    deadline: uint,
    status: (string-ascii 20),
    created-by: principal,
    created-at: uint,
    min-qualification-score: uint,
    awarded-to: (optional uint),
    document-hash: (string-ascii 64)
  }
)

(define-map bids
  { bid-id: uint }
  {
    tender-id: uint,
    bidder: principal,
    amount: uint,
    proposal-hash: (string-ascii 64),
    timestamp: uint,
    qualification-score: uint,
    status: (string-ascii 20)
  }
)

(define-map tender-bids
  { tender-id: uint }
  { bid-ids: (list 50 uint) }
)

(define-map bidder-qualifications
  { bidder: principal }
  {
    experience-years: uint,
    previous-contracts: uint,
    certification-level: uint,
    financial-stability: uint
  }
)

(define-map tender-evaluations
  { tender-id: uint, bid-id: uint }
  {
    technical-score: uint,
    financial-score: uint,
    compliance-score: uint,
    evaluator: principal,
    timestamp: uint,
    comments: (string-utf8 200)
  }
)

(define-read-only (get-tender (tender-id uint))
  (map-get? tenders { tender-id: tender-id })
)

(define-read-only (get-bid (bid-id uint))
  (map-get? bids { bid-id: bid-id })
)

(define-read-only (get-tender-bids (tender-id uint))
  (default-to { bid-ids: (list) } (map-get? tender-bids { tender-id: tender-id }))
)

(define-read-only (get-bidder-qualification (bidder principal))
  (map-get? bidder-qualifications { bidder: bidder })
)

(define-read-only (get-tender-evaluation (tender-id uint) (bid-id uint))
  (map-get? tender-evaluations { tender-id: tender-id, bid-id: bid-id })
)

(define-read-only (calculate-qualification-score (bidder principal))
  (let ((qualification (get-bidder-qualification bidder)))
    (if (is-some qualification)
      (let ((qual (unwrap-panic qualification)))
        (+ (* (get experience-years qual) u2)
           (* (get previous-contracts qual) u3)
           (* (get certification-level qual) u5)
           (get financial-stability qual)))
      u0)))

(define-public (create-tender (title (string-ascii 100)) (description (string-utf8 500)) (budget uint) (deadline uint) (min-qualification-score uint) (document-hash (string-ascii 64)))
  (let ((tender-id (var-get next-tender-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set tenders
      { tender-id: tender-id }
      {
        title: title,
        description: description,
        budget: budget,
        deadline: deadline,
        status: "open",
        created-by: tx-sender,
        created-at: stacks-block-height,
        min-qualification-score: min-qualification-score,
        awarded-to: none,
        document-hash: document-hash
      }
    )
    (var-set next-tender-id (+ tender-id u1))
    (ok tender-id)))

(define-public (submit-bid (tender-id uint) (amount uint) (proposal-hash (string-ascii 64)))
  (let (
    (tender (unwrap! (get-tender tender-id) err-not-found))
    (bid-id (var-get next-bid-id))
    (qualification-score (calculate-qualification-score tx-sender))
    (tender-bids-entry (get-tender-bids tender-id))
  )
    (asserts! (is-eq (get status tender) "open") err-tender-closed)
    (asserts! (<= amount (get budget tender)) err-invalid-bid)
    (asserts! (>= qualification-score (get min-qualification-score tender)) err-not-eligible)
    
    (map-set bids
      { bid-id: bid-id }
      {
        tender-id: tender-id,
        bidder: tx-sender,
        amount: amount,
        proposal-hash: proposal-hash,
        timestamp: stacks-block-height,
        qualification-score: qualification-score,
        status: "submitted"
      }
    )
    
    (map-set tender-bids
      { tender-id: tender-id }
      { bid-ids: (unwrap-panic (as-max-len? (append (get bid-ids tender-bids-entry) bid-id) u50)) }
    )
    
    (var-set next-bid-id (+ bid-id u1))
    (ok bid-id)))

(define-public (register-bidder-qualification (bidder principal) (experience-years uint) (previous-contracts uint) (certification-level uint) (financial-stability uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set bidder-qualifications
      { bidder: bidder }
      {
        experience-years: experience-years,
        previous-contracts: previous-contracts,
        certification-level: certification-level,
        financial-stability: financial-stability
      }
    )
    (ok true)))

(define-public (close-tender (tender-id uint))
  (let ((tender (unwrap! (get-tender tender-id) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status tender) "open") err-tender-closed)
    
    (map-set tenders
      { tender-id: tender-id }
      (merge tender { status: "closed" })
    )
    (ok true)))

(define-public (evaluate-bid (tender-id uint) (bid-id uint) (technical-score uint) (financial-score uint) (compliance-score uint) (comments (string-utf8 200)))
  (let (
    (tender (unwrap! (get-tender tender-id) err-not-found))
    (bid (unwrap! (get-bid bid-id) err-not-found))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get tender-id bid) tender-id) err-not-found)
    (asserts! (is-eq (get status tender) "closed") err-tender-open)
    
    (map-set tender-evaluations
      { tender-id: tender-id, bid-id: bid-id }
      {
        technical-score: technical-score,
        financial-score: financial-score,
        compliance-score: compliance-score,
        evaluator: tx-sender,
        timestamp: stacks-block-height,
        comments: comments
      }
    )
    (ok true)))

(define-read-only (get-bid-total-score (tender-id uint) (bid-id uint))
  (match (get-tender-evaluation tender-id bid-id)
    evaluation (+ (+ (get technical-score evaluation) (get financial-score evaluation)) (get compliance-score evaluation))
    u0))

(define-public (award-tender (tender-id uint) (bid-id uint))
  (let (
    (tender (unwrap! (get-tender tender-id) err-not-found))
    (bid (unwrap! (get-bid bid-id) err-not-found))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status tender) "closed") err-tender-open)
    (asserts! (is-none (get awarded-to tender)) err-already-awarded)
    (asserts! (is-eq (get tender-id bid) tender-id) err-not-found)
    
    (map-set tenders
      { tender-id: tender-id }
      (merge tender { 
        status: "awarded",
        awarded-to: (some bid-id)
      })
    )
    
    (map-set bids
      { bid-id: bid-id }
      (merge bid { status: "awarded" })
    )
    (ok true)))

(define-public (finalize-tender (tender-id uint))
  (let ((tender (unwrap! (get-tender tender-id) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status tender) "awarded") err-not-awarded)
    
    (map-set tenders
      { tender-id: tender-id }
      (merge tender { status: "completed" })
    )
    (ok true)))

(define-public (cancel-tender (tender-id uint) (reason (string-utf8 200)))
  (let ((tender (unwrap! (get-tender tender-id) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (or (is-eq (get status tender) "open") (is-eq (get status tender) "closed")) err-not-found)
    
    (map-set tenders
      { tender-id: tender-id }
      (merge tender { status: "cancelled" })
    )
    (ok true)))

(define-read-only (get-winning-bid (tender-id uint))
  (let ((tender (unwrap-panic (get-tender tender-id))))
    (match (get awarded-to tender)
      bid-id (get-bid bid-id)
      none)))

(define-read-only (get-all-tender-bids (tender-id uint))
  (let ((bid-ids (get bid-ids (get-tender-bids tender-id))))
    (map get-bid bid-ids)))


(define-read-only (get-milestone (milestone-id uint))
  (map-get? milestones { milestone-id: milestone-id })
)

(define-read-only (get-contract-milestones (tender-id uint))
  (default-to 
    { milestone-ids: (list), total-escrowed: u0, total-released: u0 }
    (map-get? contract-milestones { tender-id: tender-id })
  )
)

(define-read-only (get-escrow-balance (tender-id uint))
  (default-to { balance: u0 } (map-get? escrow-balances { tender-id: tender-id }))
)

(define-read-only (get-tender-milestones (tender-id uint))
  (let ((milestone-ids (get milestone-ids (get-contract-milestones tender-id))))
    (map get-milestone milestone-ids)
  )
)

(define-public (create-milestone (tender-id uint) (title (string-ascii 100)) (description (string-utf8 300)) (payment-amount uint) (due-date uint))
  (let (
    (tender (unwrap! (get-tender tender-id) err-not-found))
    (milestone-id (var-get next-milestone-id))
    (current-milestones (get-contract-milestones tender-id))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status tender) "awarded") err-not-awarded)
    (asserts! (> payment-amount u0) err-invalid-milestone)
    
    (map-set milestones
      { milestone-id: milestone-id }
      {
        tender-id: tender-id,
        title: title,
        description: description,
        payment-amount: payment-amount,
        due-date: due-date,
        status: "pending",
        completion-evidence: none,
        completed-at: none,
        approved-by: none
      }
    )
    
    (map-set contract-milestones
      { tender-id: tender-id }
      {
        milestone-ids: (unwrap-panic (as-max-len? (append (get milestone-ids current-milestones) milestone-id) u20)),
        total-escrowed: (get total-escrowed current-milestones),
        total-released: (get total-released current-milestones)
      }
    )
    
    (var-set next-milestone-id (+ milestone-id u1))
    (ok milestone-id)
  )
)

(define-public (fund-escrow (tender-id uint))
  (let (
    (tender (unwrap! (get-tender tender-id) err-not-found))
    (current-balance (get balance (get-escrow-balance tender-id)))
    (funding-amount (get budget tender))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status tender) "awarded") err-not-awarded)
    
    (try! (stx-transfer? funding-amount tx-sender (as-contract tx-sender)))
    
    (map-set escrow-balances
      { tender-id: tender-id }
      { balance: (+ current-balance funding-amount) }
    )
    
    (let ((current-milestones (get-contract-milestones tender-id)))
      (map-set contract-milestones
        { tender-id: tender-id }
        (merge current-milestones { total-escrowed: (+ (get total-escrowed current-milestones) funding-amount) })
      )
    )
    (ok true)
  )
)

(define-public (submit-milestone-completion (milestone-id uint) (evidence-hash (string-ascii 64)))
  (let (
    (milestone (unwrap! (get-milestone milestone-id) err-milestone-not-found))
    (tender (unwrap! (get-tender (get tender-id milestone)) err-not-found))
    (winning-bid (unwrap! (get-winning-bid (get tender-id milestone)) err-not-found))
  )
    (asserts! (is-eq tx-sender (get bidder winning-bid)) err-owner-only)
    (asserts! (is-eq (get status milestone) "pending") err-milestone-completed)
    
    (map-set milestones
      { milestone-id: milestone-id }
      (merge milestone {
        status: "submitted",
        completion-evidence: (some evidence-hash),
        completed-at: (some stacks-block-height)
      })
    )
    (ok true)
  )
)

(define-public (approve-milestone (milestone-id uint))
  (let (
    (milestone (unwrap! (get-milestone milestone-id) err-milestone-not-found))
    (tender-id (get tender-id milestone))
    (tender (unwrap! (get-tender tender-id) err-not-found))
    (payment-amount (get payment-amount milestone))
    (current-balance (get balance (get-escrow-balance tender-id)))
    (winning-bid (unwrap! (get-winning-bid tender-id) err-not-found))
    (current-milestones (get-contract-milestones tender-id))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status milestone) "submitted") err-milestone-not-completed)
    (asserts! (>= current-balance payment-amount) err-insufficient-escrow)
    
    (try! (as-contract (stx-transfer? payment-amount tx-sender (get bidder winning-bid))))
    
    (map-set milestones
      { milestone-id: milestone-id }
      (merge milestone {
        status: "approved",
        approved-by: (some tx-sender)
      })
    )
    
    (map-set escrow-balances
      { tender-id: tender-id }
      { balance: (- current-balance payment-amount) }
    )
    
    (map-set contract-milestones
      { tender-id: tender-id }
      (merge current-milestones { total-released: (+ (get total-released current-milestones) payment-amount) })
    )
    (ok true)
  )
)

(define-public (reject-milestone (milestone-id uint) (reason (string-utf8 200)))
  (let ((milestone (unwrap! (get-milestone milestone-id) err-milestone-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status milestone) "submitted") err-milestone-not-completed)
    
    (map-set milestones
      { milestone-id: milestone-id }
      (merge milestone {
        status: "rejected",
        completion-evidence: none,
        completed-at: none
      })
    )
    (ok true)
  )
)

(define-read-only (get-milestone-progress (tender-id uint))
  (let ((milestones-data (get-contract-milestones tender-id)))
    {
      total-milestones: (len (get milestone-ids milestones-data)),
      total-escrowed: (get total-escrowed milestones-data),
      total-released: (get total-released milestones-data),
      remaining-balance: (get balance (get-escrow-balance tender-id))
    }
  )
)