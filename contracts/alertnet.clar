;; title: alert-net
;; version: 1.0.0
;; summary: Decentralized emergency response coordination system
;; description: Coordinates emergency response through tokenized volunteer networks,
;; reputation-based task assignment, and dynamic resource allocation

;; traits
;;

;; token definitions
;;

;; constants
;;
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-input (err u104))
(define-constant err-insufficient-stake (err u105))
(define-constant err-emergency-inactive (err u106))
(define-constant err-already-assigned (err u107))
(define-constant err-not-assigned (err u108))

;; Emergency severity levels
(define-constant severity-low u1)
(define-constant severity-medium u2)
(define-constant severity-high u3)
(define-constant severity-critical u4)

;; Task status
(define-constant status-pending u1)
(define-constant status-assigned u2)
(define-constant status-in-progress u3)
(define-constant status-completed u4)
(define-constant status-verified u5)

;; Minimum stake required (in microSTX)
(define-constant min-stake-amount u1000000)

;; data vars
;;
(define-data-var emergency-nonce uint u0)
(define-data-var task-nonce uint u0)
(define-data-var total-volunteers uint u0)

;; data maps
;;

;; Volunteer registration and profiles
(define-map volunteers
    principal
    {
        skills: (string-utf8 256),
        reputation-score: uint,
        total-responses: uint,
        successful-responses: uint,
        stake-amount: uint,
        is-active: bool,
        registered-at: uint
    }
)

;; Emergency incidents
(define-map emergencies
    uint ;; emergency-id
    {
        alert-type: (string-utf8 64),
        severity: uint,
        location: (string-utf8 128),
        description: (string-utf8 512),
        required-skills: (string-utf8 256),
        is-active: bool,
        created-by: principal,
        created-at: uint,
        resolved-at: (optional uint)
    }
)

;; Tasks within emergencies
(define-map tasks
    uint ;; task-id
    {
        emergency-id: uint,
        task-description: (string-utf8 256),
        required-skill: (string-utf8 64),
        assigned-to: (optional principal),
        status: uint,
        reward-credits: uint,
        created-at: uint,
        completed-at: (optional uint)
    }
)

;; Task assignments tracking
(define-map volunteer-assignments
    { volunteer: principal, task-id: uint }
    {
        assigned-at: uint,
        started-at: (optional uint),
        completed-at: (optional uint),
        verified: bool
    }
)

;; Response credits earned by volunteers
(define-map response-credits
    principal
    uint
)

;; public functions
;;

;; Register as a volunteer with skills and stake
(define-public (register-volunteer (skills (string-utf8 256)))
    (let
        (
            (caller tx-sender)
            (stake-amount min-stake-amount)
        )
        ;; Check if already registered
        (asserts! (is-none (map-get? volunteers caller)) err-already-exists)
        
        ;; Validate input
        (asserts! (> (len skills) u0) err-invalid-input)
        
        ;; Create volunteer profile
        (map-set volunteers caller {
            skills: skills,
            reputation-score: u100,
            total-responses: u0,
            successful-responses: u0,
            stake-amount: stake-amount,
            is-active: true,
            registered-at: stacks-block-height
        })
        
        ;; Initialize credits
        (map-set response-credits caller u0)
        
        ;; Increment total volunteers
        (var-set total-volunteers (+ (var-get total-volunteers) u1))
        
        (ok true)
    )
)

;; Update volunteer skills
(define-public (update-skills (new-skills (string-utf8 256)))
    (let
        (
            (caller tx-sender)
            (volunteer-data (unwrap! (map-get? volunteers caller) err-not-found))
        )
        (asserts! (> (len new-skills) u0) err-invalid-input)
        
        (map-set volunteers caller
            (merge volunteer-data { skills: new-skills })
        )
        
        (ok true)
    )
)

;; Set volunteer availability status
(define-public (set-availability (is-active bool))
    (let
        (
            (caller tx-sender)
            (volunteer-data (unwrap! (map-get? volunteers caller) err-not-found))
        )
        (map-set volunteers caller
            (merge volunteer-data { is-active: is-active })
        )
        
        (ok true)
    )
)

;; Create an emergency alert
(define-public (create-emergency 
    (alert-type (string-utf8 64))
    (severity uint)
    (location (string-utf8 128))
    (description (string-utf8 512))
    (required-skills (string-utf8 256)))
    (let
        (
            (emergency-id (+ (var-get emergency-nonce) u1))
        )
        ;; Validate severity level
        (asserts! (and (>= severity severity-low) (<= severity severity-critical)) err-invalid-input)
        (asserts! (> (len alert-type) u0) err-invalid-input)
        (asserts! (> (len location) u0) err-invalid-input)
        
        ;; Create emergency record
        (map-set emergencies emergency-id {
            alert-type: alert-type,
            severity: severity,
            location: location,
            description: description,
            required-skills: required-skills,
            is-active: true,
            created-by: tx-sender,
            created-at: stacks-block-height,
            resolved-at: none
        })
        
        ;; Increment nonce
        (var-set emergency-nonce emergency-id)
        
        (ok emergency-id)
    )
)

;; Create a task within an emergency
(define-public (create-task
    (emergency-id uint)
    (task-description (string-utf8 256))
    (required-skill (string-utf8 64))
    (reward-credits uint))
    (let
        (
            (task-id (+ (var-get task-nonce) u1))
            (emergency-data (unwrap! (map-get? emergencies emergency-id) err-not-found))
        )
        ;; Verify emergency is active
        (asserts! (get is-active emergency-data) err-emergency-inactive)
        (asserts! (> (len task-description) u0) err-invalid-input)
        
        ;; Create task
        (map-set tasks task-id {
            emergency-id: emergency-id,
            task-description: task-description,
            required-skill: required-skill,
            assigned-to: none,
            status: status-pending,
            reward-credits: reward-credits,
            created-at: stacks-block-height,
            completed-at: none
        })
        
        ;; Increment nonce
        (var-set task-nonce task-id)
        
        (ok task-id)
    )
)

;; Volunteer accepts a task assignment
(define-public (accept-task (task-id uint))
    (let
        (
            (caller tx-sender)
            (task-data (unwrap! (map-get? tasks task-id) err-not-found))
            (volunteer-data (unwrap! (map-get? volunteers caller) err-not-found))
        )
        ;; Verify volunteer is active
        (asserts! (get is-active volunteer-data) err-unauthorized)
        
        ;; Verify task is pending
        (asserts! (is-eq (get status task-data) status-pending) err-already-assigned)
        
        ;; Assign task to volunteer
        (map-set tasks task-id
            (merge task-data {
                assigned-to: (some caller),
                status: status-assigned
            })
        )
        
        ;; Create assignment record
        (map-set volunteer-assignments
            { volunteer: caller, task-id: task-id }
            {
                assigned-at: stacks-block-height,
                started-at: none,
                completed-at: none,
                verified: false
            }
        )
        
        (ok true)
    )
)

;; Mark task as in progress
(define-public (start-task (task-id uint))
    (let
        (
            (caller tx-sender)
            (task-data (unwrap! (map-get? tasks task-id) err-not-found))
            (assignment-data (unwrap! (map-get? volunteer-assignments { volunteer: caller, task-id: task-id }) err-not-assigned))
        )
        ;; Verify caller is assigned to this task
        (asserts! (is-eq (get assigned-to task-data) (some caller)) err-unauthorized)
        
        ;; Update task status
        (map-set tasks task-id
            (merge task-data { status: status-in-progress })
        )
        
        ;; Update assignment
        (map-set volunteer-assignments
            { volunteer: caller, task-id: task-id }
            (merge assignment-data { started-at: (some stacks-block-height) })
        )
        
        (ok true)
    )
)

;; Complete a task
(define-public (complete-task (task-id uint))
    (let
        (
            (caller tx-sender)
            (task-data (unwrap! (map-get? tasks task-id) err-not-found))
            (assignment-data (unwrap! (map-get? volunteer-assignments { volunteer: caller, task-id: task-id }) err-not-assigned))
        )
        ;; Verify caller is assigned to this task
        (asserts! (is-eq (get assigned-to task-data) (some caller)) err-unauthorized)
        
        ;; Update task status
        (map-set tasks task-id
            (merge task-data {
                status: status-completed,
                completed-at: (some stacks-block-height)
            })
        )
        
        ;; Update assignment
        (map-set volunteer-assignments
            { volunteer: caller, task-id: task-id }
            (merge assignment-data { completed-at: (some stacks-block-height) })
        )
        
        (ok true)
    )
)

;; Verify task completion and award credits (emergency services or authorized verifier)
(define-public (verify-task-completion (task-id uint) (volunteer principal))
    (let
        (
            (task-data (unwrap! (map-get? tasks task-id) err-not-found))
            (emergency-data (unwrap! (map-get? emergencies (get emergency-id task-data)) err-not-found))
            (assignment-data (unwrap! (map-get? volunteer-assignments { volunteer: volunteer, task-id: task-id }) err-not-assigned))
            (volunteer-data (unwrap! (map-get? volunteers volunteer) err-not-found))
            (current-credits (default-to u0 (map-get? response-credits volunteer)))
        )
        ;; Only emergency creator can verify
        (asserts! (is-eq tx-sender (get created-by emergency-data)) err-unauthorized)
        
        ;; Verify task is completed
        (asserts! (is-eq (get status task-data) status-completed) err-invalid-input)
        
        ;; Update task status
        (map-set tasks task-id
            (merge task-data { status: status-verified })
        )
        
        ;; Update assignment as verified
        (map-set volunteer-assignments
            { volunteer: volunteer, task-id: task-id }
            (merge assignment-data { verified: true })
        )
        
        ;; Award credits
        (map-set response-credits volunteer
            (+ current-credits (get reward-credits task-data))
        )
        
        ;; Update volunteer stats and reputation
        (map-set volunteers volunteer
            (merge volunteer-data {
                total-responses: (+ (get total-responses volunteer-data) u1),
                successful-responses: (+ (get successful-responses volunteer-data) u1),
                reputation-score: (+ (get reputation-score volunteer-data) u10)
            })
        )
        
        (ok true)
    )
)

;; Resolve an emergency
(define-public (resolve-emergency (emergency-id uint))
    (let
        (
            (emergency-data (unwrap! (map-get? emergencies emergency-id) err-not-found))
        )
        ;; Only creator or contract owner can resolve
        (asserts! (or (is-eq tx-sender (get created-by emergency-data)) (is-eq tx-sender contract-owner)) err-unauthorized)
        
        ;; Update emergency status
        (map-set emergencies emergency-id
            (merge emergency-data {
                is-active: false,
                resolved-at: (some stacks-block-height)
            })
        )
        
        (ok true)
    )
)

;; read only functions
;;

;; Get volunteer profile
(define-read-only (get-volunteer (volunteer principal))
    (ok (map-get? volunteers volunteer))
)

;; Get volunteer credits
(define-read-only (get-volunteer-credits (volunteer principal))
    (ok (default-to u0 (map-get? response-credits volunteer)))
)

;; Get emergency details
(define-read-only (get-emergency (emergency-id uint))
    (ok (map-get? emergencies emergency-id))
)

;; Get task details
(define-read-only (get-task (task-id uint))
    (ok (map-get? tasks task-id))
)

;; Get task assignment
(define-read-only (get-assignment (volunteer principal) (task-id uint))
    (ok (map-get? volunteer-assignments { volunteer: volunteer, task-id: task-id }))
)

;; Get total volunteers count
(define-read-only (get-total-volunteers)
    (ok (var-get total-volunteers))
)

;; Get current emergency nonce
(define-read-only (get-emergency-count)
    (ok (var-get emergency-nonce))
)

;; Get current task nonce
(define-read-only (get-task-count)
    (ok (var-get task-nonce))
)

;; Check if volunteer is eligible for task based on reputation
(define-read-only (is-volunteer-eligible (volunteer principal) (min-reputation uint))
    (match (map-get? volunteers volunteer)
        volunteer-data (ok (and 
            (get is-active volunteer-data)
            (>= (get reputation-score volunteer-data) min-reputation)
        ))
        (ok false)
    )
)

;; private functions
;;