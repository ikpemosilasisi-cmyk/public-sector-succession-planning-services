;; Public Sector Succession Planning Tracker
;; Manages succession planning for public sector positions with role transitions

(define-constant ERR_NOT_FOUND 404)
(define-constant ERR_UNAUTHORIZED 401)
(define-constant ERR_INVALID_INPUT 400)
(define-constant ERR_DUPLICATE_RECORD 409)
(define-constant ERR_INVALID_STATUS 422)

(define-data-var position-counter uint u0)
(define-data-var transition-counter uint u0)
(define-data-var next-position-id uint u1)

(define-map positions
  { position-id: uint }
  {
    title: (string-ascii 128),
    department: (string-ascii 64),
    created-at: uint,
    retirement-date: uint,
    status: (string-ascii 16),
    current-holder: principal,
    salary-level: uint
  }
)

(define-map succession-plans
  { plan-id: uint }
  {
    position-id: uint,
    primary-successor: principal,
    backup-successor: principal,
    plan-date: uint,
    training-status: (string-ascii 16)
  }
)

(define-map transitions
  { transition-id: uint }
  {
    position-id: uint,
    from-holder: principal,
    to-holder: principal,
    transition-date: uint,
    completion-status: (string-ascii 16)
  }
)

(define-public (register-position (title (string-ascii 128)) (department (string-ascii 64)) (retirement-date uint) (salary uint))
  (let ((position-id (var-get next-position-id)))
    (if (and (> (len title) u0) (> (len department) u0) (> salary u0))
      (begin
        (map-insert positions
          { position-id: position-id }
          {
            title: title,
            department: department,
            created-at: burn-block-height,
            retirement-date: retirement-date,
            status: "vacant",
            current-holder: tx-sender,
            salary-level: salary
          }
        )
        (var-set position-counter (+ (var-get position-counter) u1))
        (var-set next-position-id (+ position-id u1))
        (ok position-id)
      )
      (err ERR_INVALID_INPUT)
    )
  )
)

(define-public (create-succession-plan (position-id uint) (primary principal) (backup principal))
  (match (map-get? positions { position-id: position-id })
    position
    (if (and (is-eq (get status position) "vacant") (not (is-eq primary backup)))
      (let ((plan-id (+ (var-get position-counter) (var-get transition-counter))))
        (begin
          (map-insert succession-plans
            { plan-id: plan-id }
            {
              position-id: position-id,
              primary-successor: primary,
              backup-successor: backup,
              plan-date: burn-block-height,
              training-status: "pending"
            }
          )
          (ok plan-id)
        )
      )
      (err ERR_INVALID_STATUS)
    )
    (err ERR_NOT_FOUND)
  )
)

(define-public (update-training-status (plan-id uint) (new-status (string-ascii 16)))
  (match (map-get? succession-plans { plan-id: plan-id })
    plan
    (if (or (is-eq new-status "pending") (or (is-eq new-status "in-progress") (or (is-eq new-status "completed") (is-eq new-status "certified"))))
      (begin
        (map-set succession-plans
          { plan-id: plan-id }
          (merge plan { training-status: new-status })
        )
        (ok plan-id)
      )
      (err ERR_INVALID_INPUT)
    )
    (err ERR_NOT_FOUND)
  )
)

(define-public (execute-transition (position-id uint) (new-holder principal))
  (match (map-get? positions { position-id: position-id })
    position
    (let ((transition-id (var-get transition-counter)))
      (begin
        (map-insert transitions
          { transition-id: transition-id }
          {
            position-id: position-id,
            from-holder: (get current-holder position),
            to-holder: new-holder,
            transition-date: burn-block-height,
            completion-status: "initiated"
          }
        )
        (map-set positions
          { position-id: position-id }
          (merge position { current-holder: new-holder, status: "filled" })
        )
        (var-set transition-counter (+ transition-id u1))
        (ok transition-id)
      )
    )
    (err ERR_NOT_FOUND)
  )
)

(define-public (complete-transition (transition-id uint))
  (match (map-get? transitions { transition-id: transition-id })
    transition
    (begin
      (map-set transitions
        { transition-id: transition-id }
        (merge transition { completion-status: "completed" })
      )
      (ok transition-id)
    )
    (err ERR_NOT_FOUND)
  )
)

(define-read-only (get-position (position-id uint))
  (map-get? positions { position-id: position-id })
)

(define-read-only (get-succession-plan (plan-id uint))
  (map-get? succession-plans { plan-id: plan-id })
)

(define-read-only (get-transition (transition-id uint))
  (map-get? transitions { transition-id: transition-id })
)

(define-read-only (get-total-positions)
  (var-get position-counter)
)

(define-read-only (get-total-transitions)
  (var-get transition-counter)
)

