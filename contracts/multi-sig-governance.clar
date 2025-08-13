(define-constant err-not-signer (err u200))
(define-constant err-proposal-not-found (err u201))
(define-constant err-already-voted (err u202))
(define-constant err-proposal-expired (err u203))
(define-constant err-insufficient-approvals (err u204))
(define-constant err-proposal-executed (err u205))
(define-constant err-invalid-threshold (err u206))


(define-data-var proposal-threshold uint u3)
(define-data-var next-proposal-id uint u1)

(define-map authorized-signers
  { signer: principal }
  { active: bool, added-at: uint }
)

(define-map governance-proposals
  { proposal-id: uint }
  {
    proposal-type: (string-ascii 30),
    target-id: uint,
    proposer: principal,
    created-at: uint,
    expires-at: uint,
    status: (string-ascii 20),
    approvals: (list 20 principal),
    executed-at: (optional uint),
    metadata: (string-utf8 500)
  }
)

(define-map signer-votes
  { proposal-id: uint, signer: principal }
  { voted: bool, timestamp: uint }
)

(define-data-var signers-count uint u0)

(define-read-only (is-authorized-signer (signer principal))
  (match (map-get? authorized-signers { signer: signer })
    entry (get active entry)
    false))

(define-read-only (get-proposal (proposal-id uint))
  (map-get? governance-proposals { proposal-id: proposal-id }))

(define-read-only (get-signer-vote (proposal-id uint) (signer principal))
  (map-get? signer-votes { proposal-id: proposal-id, signer: signer }))

(define-read-only (get-proposal-approvals (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal (len (get approvals proposal))
    u0))

(define-read-only (is-proposal-executable (proposal-id uint))
  (let ((proposal (unwrap-panic (get-proposal proposal-id))))
    (and 
      (>= (get-proposal-approvals proposal-id) (var-get proposal-threshold))
      (< stacks-block-height (get expires-at proposal))
      (is-eq (get status proposal) "active"))))

(define-public (authorize-signer (signer principal))
  (begin
    (asserts! (is-eq tx-sender signer) (err u207))
    (map-set authorized-signers
      { signer: signer }
      { active: true, added-at: stacks-block-height })
    (var-set signers-count (+ (var-get signers-count) u1))
    (ok true)))

(define-public (revoke-signer (signer principal))
  (begin
    (asserts! (is-eq tx-sender tx-sender) (err u207))
    (asserts! (is-authorized-signer signer) err-not-signer)
    (map-set authorized-signers
      { signer: signer }
      { active: false, added-at: stacks-block-height })
    (var-set signers-count (- (var-get signers-count) u1))
    (ok true)))

(define-public (set-approval-threshold (threshold uint))
  (begin
    (asserts! (is-eq tx-sender tx-sender) (err u207))
    (asserts! (and (> threshold u0) (<= threshold (var-get signers-count))) err-invalid-threshold)
    (var-set proposal-threshold threshold)
    (ok true)))

(define-public (create-governance-proposal (proposal-type (string-ascii 30)) (target-id uint) (metadata (string-utf8 500)) (duration uint))
  (let ((proposal-id (var-get next-proposal-id)))
    (asserts! (is-authorized-signer tx-sender) err-not-signer)
    (map-set governance-proposals
      { proposal-id: proposal-id }
      {
        proposal-type: proposal-type,
        target-id: target-id,
        proposer: tx-sender,
        created-at: stacks-block-height,
        expires-at: (+ stacks-block-height duration),
        status: "active",
        approvals: (list),
        executed-at: none,
        metadata: metadata
      })
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)))

(define-public (vote-on-proposal (proposal-id uint) (approve bool))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) err-proposal-not-found))
    (existing-vote (get-signer-vote proposal-id tx-sender))
  )
    (asserts! (is-authorized-signer tx-sender) err-not-signer)
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (< stacks-block-height (get expires-at proposal)) err-proposal-expired)
    (asserts! (is-eq (get status proposal) "active") err-proposal-executed)
    
    (map-set signer-votes
      { proposal-id: proposal-id, signer: tx-sender }
      { voted: true, timestamp: stacks-block-height })
    
    ;; (if approve
    ;;   (map-set governance-proposals
    ;;     { proposal-id: proposal-id }
    ;;     (merge proposal { 
    ;;       approvals: (unwrap-panic (as-max-len? (append (get approvals proposal) tx-sender) u20))
    ;;     }))
    ;;   (ok true))
    (ok approve)))

(define-public (execute-proposal (proposal-id uint))
  (let ((proposal (unwrap! (get-proposal proposal-id) err-proposal-not-found)))
    (asserts! (is-authorized-signer tx-sender) err-not-signer)
    (asserts! (is-proposal-executable proposal-id) err-insufficient-approvals)
    
    (map-set governance-proposals
      { proposal-id: proposal-id }
      (merge proposal { 
        status: "executed",
        executed-at: (some stacks-block-height)
      }))
    (ok true)))

(define-public (multi-sig-award-tender (proposal-id uint))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) err-proposal-not-found))
    (tender-id (get target-id proposal))
  )
    (asserts! (is-eq (get proposal-type proposal) "award-tender") (err u404))
    (asserts! (is-eq (get status proposal) "executed") err-proposal-executed)
    
    (contract-call? .procurement-system award-tender tender-id tender-id)
  ))

(define-public (multi-sig-approve-milestone (proposal-id uint))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) err-proposal-not-found))
    (milestone-id (get target-id proposal))
  )
    (asserts! (is-eq (get proposal-type proposal) "approve-milestone") (err u404))
    (asserts! (is-eq (get status proposal) "executed") err-proposal-executed)
    
    (contract-call? .procurement-system approve-milestone milestone-id)
  ))

(define-public (multi-sig-fund-escrow (proposal-id uint))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) err-proposal-not-found))
    (tender-id (get target-id proposal))
  )
    (asserts! (is-eq (get proposal-type proposal) "fund-escrow") (err u404))
    (asserts! (is-eq (get status proposal) "executed") err-proposal-executed)
    
    (contract-call? .procurement-system fund-escrow tender-id)
  ))

(define-read-only (get-governance-stats)
  {
    total-signers: (var-get signers-count),
    approval-threshold: (var-get proposal-threshold),
    total-proposals: (- (var-get next-proposal-id) u1)
  })


;; Removed get-proposal-id as it is no longer needed

(define-read-only (is-proposal-active (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal (and 
      (is-eq (get status proposal) "active")
      (< stacks-block-height (get expires-at proposal)))
    false))