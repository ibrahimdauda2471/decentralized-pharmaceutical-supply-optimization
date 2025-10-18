;; Drug Distribution Optimizer Contract
;; Optimizes pharmaceutical distribution and allocation, predicts demand patterns,
;; manages inventory levels, coordinates with distributors, and ensures drug availability

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-insufficient-stock (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-distributor (err u105))

;; Data Variables
(define-data-var next-drug-id uint u1)
(define-data-var next-distributor-id uint u1)
(define-data-var next-allocation-id uint u1)
(define-data-var emergency-threshold uint u10) ;; Minimum stock level for emergency alerts

;; Drug Information Structure
(define-map drugs
    { drug-id: uint }
    {
        name: (string-ascii 64),
        manufacturer: (string-ascii 64),
        current-stock: uint,
        reorder-point: uint,
        unit-cost: uint,
        expiry-date: uint,
        therapeutic-class: (string-ascii 32),
        is-active: bool
    }
)

;; Distributor Information
(define-map distributors
    { distributor-id: uint }
    {
        name: (string-ascii 64),
        location: (string-ascii 64),
        capacity: uint,
        reliability-score: uint,
        contact-info: (string-ascii 128),
        is-authorized: bool
    }
)

;; Distribution Centers Inventory
(define-map inventory
    { center-id: uint, drug-id: uint }
    {
        current-stock: uint,
        allocated-stock: uint,
        reserved-stock: uint,
        last-updated: uint
    }
)

;; Demand Forecasting Data
(define-map demand-forecast
    { drug-id: uint, period: uint }
    {
        predicted-demand: uint,
        confidence-level: uint,
        historical-usage: uint,
        seasonal-factor: uint
    }
)

;; Distribution Allocations
(define-map allocations
    { allocation-id: uint }
    {
        drug-id: uint,
        distributor-id: uint,
        quantity: uint,
        priority: uint, ;; 1=critical, 2=high, 3=normal
        status: (string-ascii 16), ;; "pending", "approved", "shipped", "delivered"
        allocation-date: uint,
        delivery-date: uint
    }
)

;; Route Optimization Data
(define-map routes
    { route-id: uint }
    {
        distributor-id: uint,
        destinations: (list 10 uint),
        estimated-cost: uint,
        estimated-time: uint,
        optimization-score: uint
    }
)

;; Authorized Personnel
(define-map authorized-users
    { user: principal }
    { role: (string-ascii 32) }
)

;; Private Functions

;; Check if user is authorized
(define-private (is-authorized (user principal))
    (or
        (is-eq user contract-owner)
        (is-some (map-get? authorized-users { user: user }))
    )
)

;; Calculate reorder quantity based on demand forecast
(define-private (calculate-reorder-quantity (drug-id uint) (current-stock uint))
    (let
        (
            (forecast (default-to { predicted-demand: u0, confidence-level: u0, historical-usage: u0, seasonal-factor: u100 }
                                (map-get? demand-forecast { drug-id: drug-id, period: stacks-block-height })))
        )
        (+ (get predicted-demand forecast) (* current-stock u2))
    )
)

;; Calculate distribution priority based on stock levels and demand
(define-private (calculate-priority (drug-id uint) (current-stock uint))
    (let
        (
            (drug-info (unwrap! (map-get? drugs { drug-id: drug-id }) u3))
            (reorder-point (get reorder-point drug-info))
        )
        (if (<= current-stock (var-get emergency-threshold))
            u1 ;; Critical priority
            (if (<= current-stock reorder-point)
                u2 ;; High priority
                u3 ;; Normal priority
            )
        )
    )
)

;; Public Functions

;; Initialize contract with owner authorization
(define-public (initialize)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-users { user: contract-owner } { role: "admin" })
        (ok true)
    )
)

;; Add authorized user
(define-public (add-authorized-user (user principal) (role (string-ascii 32)))
    (begin
        (asserts! (is-authorized tx-sender) err-unauthorized)
        (map-set authorized-users { user: user } { role: role })
        (ok true)
    )
)

;; Register new drug in the system
(define-public (register-drug 
    (name (string-ascii 64))
    (manufacturer (string-ascii 64))
    (initial-stock uint)
    (reorder-point uint)
    (unit-cost uint)
    (expiry-date uint)
    (therapeutic-class (string-ascii 32))
)
    (let
        (
            (drug-id (var-get next-drug-id))
        )
        (asserts! (is-authorized tx-sender) err-unauthorized)
        (asserts! (> initial-stock u0) err-invalid-amount)
        (map-set drugs
            { drug-id: drug-id }
            {
                name: name,
                manufacturer: manufacturer,
                current-stock: initial-stock,
                reorder-point: reorder-point,
                unit-cost: unit-cost,
                expiry-date: expiry-date,
                therapeutic-class: therapeutic-class,
                is-active: true
            }
        )
        (var-set next-drug-id (+ drug-id u1))
        (ok drug-id)
    )
)

;; Register distributor
(define-public (register-distributor
    (name (string-ascii 64))
    (location (string-ascii 64))
    (capacity uint)
    (contact-info (string-ascii 128))
)
    (let
        (
            (distributor-id (var-get next-distributor-id))
        )
        (asserts! (is-authorized tx-sender) err-unauthorized)
        (map-set distributors
            { distributor-id: distributor-id }
            {
                name: name,
                location: location,
                capacity: capacity,
                reliability-score: u100, ;; Default perfect score
                contact-info: contact-info,
                is-authorized: true
            }
        )
        (var-set next-distributor-id (+ distributor-id u1))
        (ok distributor-id)
    )
)

;; Update inventory for a distribution center
(define-public (update-inventory
    (center-id uint)
    (drug-id uint)
    (new-stock uint)
    (allocated-stock uint)
    (reserved-stock uint)
)
    (begin
        (asserts! (is-authorized tx-sender) err-unauthorized)
        (asserts! (is-some (map-get? drugs { drug-id: drug-id })) err-not-found)
        (map-set inventory
            { center-id: center-id, drug-id: drug-id }
            {
                current-stock: new-stock,
                allocated-stock: allocated-stock,
                reserved-stock: reserved-stock,
                last-updated: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Create demand forecast for a drug
(define-public (create-demand-forecast
    (drug-id uint)
    (period uint)
    (predicted-demand uint)
    (confidence-level uint)
    (historical-usage uint)
    (seasonal-factor uint)
)
    (begin
        (asserts! (is-authorized tx-sender) err-unauthorized)
        (asserts! (is-some (map-get? drugs { drug-id: drug-id })) err-not-found)
        (asserts! (<= confidence-level u100) err-invalid-amount)
        (map-set demand-forecast
            { drug-id: drug-id, period: period }
            {
                predicted-demand: predicted-demand,
                confidence-level: confidence-level,
                historical-usage: historical-usage,
                seasonal-factor: seasonal-factor
            }
        )
        (ok true)
    )
)

;; Allocate drugs to distributor
(define-public (allocate-drugs
    (drug-id uint)
    (distributor-id uint)
    (quantity uint)
    (delivery-date uint)
)
    (let
        (
            (allocation-id (var-get next-allocation-id))
            (drug-info (unwrap! (map-get? drugs { drug-id: drug-id }) err-not-found))
            (distributor-info (unwrap! (map-get? distributors { distributor-id: distributor-id }) err-invalid-distributor))
            (priority (calculate-priority drug-id (get current-stock drug-info)))
        )
        (asserts! (is-authorized tx-sender) err-unauthorized)
        (asserts! (get is-active drug-info) err-not-found)
        (asserts! (get is-authorized distributor-info) err-invalid-distributor)
        (asserts! (> quantity u0) err-invalid-amount)
        (asserts! (>= (get current-stock drug-info) quantity) err-insufficient-stock)
        
        ;; Create allocation record
        (map-set allocations
            { allocation-id: allocation-id }
            {
                drug-id: drug-id,
                distributor-id: distributor-id,
                quantity: quantity,
                priority: priority,
                status: "pending",
                allocation-date: stacks-block-height,
                delivery-date: delivery-date
            }
        )
        
        ;; Update drug stock
        (map-set drugs
            { drug-id: drug-id }
            (merge drug-info { current-stock: (- (get current-stock drug-info) quantity) })
        )
        
        (var-set next-allocation-id (+ allocation-id u1))
        (ok allocation-id)
    )
)

;; Update allocation status
(define-public (update-allocation-status (allocation-id uint) (new-status (string-ascii 16)))
    (let
        (
            (allocation (unwrap! (map-get? allocations { allocation-id: allocation-id }) err-not-found))
        )
        (asserts! (is-authorized tx-sender) err-unauthorized)
        (map-set allocations
            { allocation-id: allocation-id }
            (merge allocation { status: new-status })
        )
        (ok true)
    )
)

;; Get drug information
(define-read-only (get-drug-info (drug-id uint))
    (map-get? drugs { drug-id: drug-id })
)

;; Get distributor information
(define-read-only (get-distributor-info (distributor-id uint))
    (map-get? distributors { distributor-id: distributor-id })
)

;; Get inventory for a specific center and drug
(define-read-only (get-inventory (center-id uint) (drug-id uint))
    (map-get? inventory { center-id: center-id, drug-id: drug-id })
)

;; Get demand forecast
(define-read-only (get-demand-forecast (drug-id uint) (period uint))
    (map-get? demand-forecast { drug-id: drug-id, period: period })
)

;; Get allocation information
(define-read-only (get-allocation (allocation-id uint))
    (map-get? allocations { allocation-id: allocation-id })
)

;; Check if reorder is needed
(define-read-only (needs-reorder (drug-id uint))
    (match (map-get? drugs { drug-id: drug-id })
        drug-info
        (let
            (
                (current-stock (get current-stock drug-info))
                (reorder-point (get reorder-point drug-info))
            )
            (<= current-stock reorder-point)
        )
        false
    )
)

;; Get current stock level for a drug
(define-read-only (get-stock-level (drug-id uint))
    (match (map-get? drugs { drug-id: drug-id })
        drug-info (some (get current-stock drug-info))
        none
    )
)

