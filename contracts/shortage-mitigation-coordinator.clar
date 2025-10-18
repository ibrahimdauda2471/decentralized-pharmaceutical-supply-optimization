;; Shortage Mitigation Coordinator Contract
;; Coordinates drug shortage mitigation and alternative sourcing, tracks shortage alerts,
;; manages substitute medications, coordinates with healthcare providers, and ensures patient access

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-not-found (err u201))
(define-constant err-invalid-amount (err u202))
(define-constant err-unauthorized (err u203))
(define-constant err-invalid-status (err u204))
(define-constant err-invalid-priority (err u205))
(define-constant err-shortage-exists (err u206))

;; Priority Levels
(define-constant priority-critical u1)
(define-constant priority-high u2)
(define-constant priority-medium u3)
(define-constant priority-low u4)

;; Alert Status
(define-constant status-active "active")
(define-constant status-resolved "resolved")
(define-constant status-monitoring "monitoring")
(define-constant status-emergency "emergency")

;; Data Variables
(define-data-var next-alert-id uint u1)
(define-data-var next-alternative-id uint u1)
(define-data-var next-provider-id uint u1)
(define-data-var next-request-id uint u1)
(define-data-var critical-threshold uint u5) ;; Critical shortage threshold
(define-data-var emergency-response-active bool false)

;; Shortage Alerts
(define-map shortage-alerts
    { alert-id: uint }
    {
        drug-name: (string-ascii 64),
        therapeutic-class: (string-ascii 32),
        current-stock: uint,
        minimum-required: uint,
        shortage-level: uint, ;; Percentage shortage
        priority: uint,
        status: (string-ascii 16),
        affected-regions: (list 5 (string-ascii 32)),
        estimated-duration: uint,
        created-at: uint,
        last-updated: uint
    }
)

;; Alternative Medications
(define-map alternative-medications
    { alternative-id: uint }
    {
        original-drug: (string-ascii 64),
        substitute-drug: (string-ascii 64),
        therapeutic-equivalence: uint, ;; 1-100 scale
        dosage-conversion: (string-ascii 64),
        contraindications: (string-ascii 128),
        approval-status: (string-ascii 16),
        availability: uint,
        cost-difference: int ;; Positive if more expensive
    }
)

;; Healthcare Providers
(define-map healthcare-providers
    { provider-id: uint }
    {
        name: (string-ascii 64),
        facility-type: (string-ascii 32), ;; "hospital", "clinic", "pharmacy"
        location: (string-ascii 64),
        capacity: uint,
        specialization: (list 3 (string-ascii 32)),
        contact-info: (string-ascii 128),
        priority-level: uint,
        is-active: bool
    }
)

;; Emergency Supply Requests
(define-map supply-requests
    { request-id: uint }
    {
        provider-id: uint,
        drug-name: (string-ascii 64),
        quantity-needed: uint,
        urgency-level: uint,
        patient-count: uint,
        medical-justification: (string-ascii 256),
        status: (string-ascii 16),
        requested-at: uint,
        fulfilled-at: uint
    }
)

;; Shortage Monitoring Data
(define-map monitoring-data
    { drug-name: (string-ascii 64), period: uint }
    {
        consumption-rate: uint,
        supply-rate: uint,
        trend-indicator: int, ;; Positive for increasing shortage
        prediction-accuracy: uint,
        risk-score: uint
    }
)

;; Emergency Response Protocols
(define-map emergency-protocols
    { protocol-id: uint }
    {
        trigger-conditions: (string-ascii 128),
        response-actions: (string-ascii 256),
        responsible-parties: (list 5 principal),
        escalation-timeline: uint,
        communication-plan: (string-ascii 256)
    }
)

;; Authorized Personnel
(define-map authorized-coordinators
    { coordinator: principal }
    { 
        role: (string-ascii 32),
        clearance-level: uint,
        specialization: (string-ascii 64)
    }
)

;; Private Functions

;; Helper function to get minimum of two values
(define-private (min-value (a uint) (b uint))
    (if (<= a b) a b)
)

;; Check if user is authorized coordinator
(define-private (is-authorized-coordinator (user principal))
    (or
        (is-eq user contract-owner)
        (is-some (map-get? authorized-coordinators { coordinator: user }))
    )
)

;; Calculate shortage severity level
(define-private (calculate-shortage-level (current-stock uint) (minimum-required uint))
    (if (is-eq minimum-required u0)
        u100 ;; Complete shortage
        (let
            (
                (shortage-amount (if (> minimum-required current-stock)
                                   (- minimum-required current-stock)
                                   u0))
            )
            (if (is-eq shortage-amount u0)
                u0
                (* (/ shortage-amount minimum-required) u100)
            )
        )
    )
)

;; Determine priority based on shortage level and patient impact
(define-private (determine-priority (shortage-level uint) (patient-count uint))
    (if (or (>= shortage-level u80) (>= patient-count u100))
        priority-critical
        (if (or (>= shortage-level u60) (>= patient-count u50))
            priority-high
            (if (or (>= shortage-level u40) (>= patient-count u20))
                priority-medium
                priority-low
            )
        )
    )
)

;; Calculate risk score based on multiple factors
(define-private (calculate-risk-score 
    (shortage-level uint) 
    (consumption-rate uint) 
    (supply-rate uint)
)
    (let
        (
            (supply-deficit (if (> consumption-rate supply-rate)
                              (- consumption-rate supply-rate)
                              u0))
            (base-risk (* shortage-level u2))
        )
        (+ base-risk (min-value (* supply-deficit u10) u500))
    )
)

;; Public Functions

;; Initialize contract
(define-public (initialize)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-coordinators 
            { coordinator: contract-owner } 
            { role: "admin", clearance-level: u10, specialization: "general" }
        )
        (ok true)
    )
)

;; Add authorized coordinator
(define-public (add-coordinator 
    (coordinator principal) 
    (role (string-ascii 32))
    (clearance-level uint)
    (specialization (string-ascii 64))
)
    (begin
        (asserts! (is-authorized-coordinator tx-sender) err-unauthorized)
        (map-set authorized-coordinators
            { coordinator: coordinator }
            { 
                role: role,
                clearance-level: clearance-level,
                specialization: specialization
            }
        )
        (ok true)
    )
)

;; Create shortage alert
(define-public (create-shortage-alert
    (drug-name (string-ascii 64))
    (therapeutic-class (string-ascii 32))
    (current-stock uint)
    (minimum-required uint)
    (affected-regions (list 5 (string-ascii 32)))
    (estimated-duration uint)
)
    (let
        (
            (alert-id (var-get next-alert-id))
            (shortage-level (calculate-shortage-level current-stock minimum-required))
            (priority (determine-priority shortage-level minimum-required))
        )
        (asserts! (is-authorized-coordinator tx-sender) err-unauthorized)
        (asserts! (> minimum-required current-stock) err-invalid-amount)
        
        (map-set shortage-alerts
            { alert-id: alert-id }
            {
                drug-name: drug-name,
                therapeutic-class: therapeutic-class,
                current-stock: current-stock,
                minimum-required: minimum-required,
                shortage-level: shortage-level,
                priority: priority,
                status: (if (>= priority priority-critical) status-emergency status-active),
                affected-regions: affected-regions,
                estimated-duration: estimated-duration,
                created-at: stacks-block-height,
                last-updated: stacks-block-height
            }
        )
        
        ;; Activate emergency response if critical
        (if (is-eq priority priority-critical)
            (begin
                (var-set emergency-response-active true)
                true
            )
            false
        )
        
        (var-set next-alert-id (+ alert-id u1))
        (ok alert-id)
    )
)

;; Register alternative medication
(define-public (register-alternative
    (original-drug (string-ascii 64))
    (substitute-drug (string-ascii 64))
    (therapeutic-equivalence uint)
    (dosage-conversion (string-ascii 64))
    (contraindications (string-ascii 128))
    (availability uint)
    (cost-difference int)
)
    (let
        (
            (alternative-id (var-get next-alternative-id))
        )
        (asserts! (is-authorized-coordinator tx-sender) err-unauthorized)
        (asserts! (<= therapeutic-equivalence u100) err-invalid-amount)
        
        (map-set alternative-medications
            { alternative-id: alternative-id }
            {
                original-drug: original-drug,
                substitute-drug: substitute-drug,
                therapeutic-equivalence: therapeutic-equivalence,
                dosage-conversion: dosage-conversion,
                contraindications: contraindications,
                approval-status: "pending",
                availability: availability,
                cost-difference: cost-difference
            }
        )
        
        (var-set next-alternative-id (+ alternative-id u1))
        (ok alternative-id)
    )
)

;; Register healthcare provider
(define-public (register-provider
    (name (string-ascii 64))
    (facility-type (string-ascii 32))
    (location (string-ascii 64))
    (capacity uint)
    (specialization (list 3 (string-ascii 32)))
    (contact-info (string-ascii 128))
)
    (let
        (
            (provider-id (var-get next-provider-id))
        )
        (asserts! (is-authorized-coordinator tx-sender) err-unauthorized)
        
        (map-set healthcare-providers
            { provider-id: provider-id }
            {
                name: name,
                facility-type: facility-type,
                location: location,
                capacity: capacity,
                specialization: specialization,
                contact-info: contact-info,
                priority-level: priority-medium,
                is-active: true
            }
        )
        
        (var-set next-provider-id (+ provider-id u1))
        (ok provider-id)
    )
)

;; Submit emergency supply request
(define-public (submit-supply-request
    (provider-id uint)
    (drug-name (string-ascii 64))
    (quantity-needed uint)
    (urgency-level uint)
    (patient-count uint)
    (medical-justification (string-ascii 256))
)
    (let
        (
            (request-id (var-get next-request-id))
        )
        (asserts! (is-some (map-get? healthcare-providers { provider-id: provider-id })) err-not-found)
        (asserts! (> quantity-needed u0) err-invalid-amount)
        (asserts! (<= urgency-level priority-critical) err-invalid-priority)
        
        (map-set supply-requests
            { request-id: request-id }
            {
                provider-id: provider-id,
                drug-name: drug-name,
                quantity-needed: quantity-needed,
                urgency-level: urgency-level,
                patient-count: patient-count,
                medical-justification: medical-justification,
                status: "pending",
                requested-at: stacks-block-height,
                fulfilled-at: u0
            }
        )
        
        (var-set next-request-id (+ request-id u1))
        (ok request-id)
    )
)

;; Update shortage alert status
(define-public (update-alert-status (alert-id uint) (new-status (string-ascii 16)))
    (let
        (
            (alert (unwrap! (map-get? shortage-alerts { alert-id: alert-id }) err-not-found))
        )
        (asserts! (is-authorized-coordinator tx-sender) err-unauthorized)
        
        (map-set shortage-alerts
            { alert-id: alert-id }
            (merge alert { 
                status: new-status,
                last-updated: stacks-block-height
            })
        )
        
        ;; Deactivate emergency response if resolved
        (if (is-eq new-status status-resolved)
            (begin
                (var-set emergency-response-active false)
                true
            )
            false
        )
        
        (ok true)
    )
)

;; Update monitoring data
(define-public (update-monitoring
    (drug-name (string-ascii 64))
    (period uint)
    (consumption-rate uint)
    (supply-rate uint)
    (trend-indicator int)
)
    (let
        (
            (risk-score (calculate-risk-score 
                         (calculate-shortage-level supply-rate consumption-rate)
                         consumption-rate
                         supply-rate))
        )
        (asserts! (is-authorized-coordinator tx-sender) err-unauthorized)
        
        (map-set monitoring-data
            { drug-name: drug-name, period: period }
            {
                consumption-rate: consumption-rate,
                supply-rate: supply-rate,
                trend-indicator: trend-indicator,
                prediction-accuracy: u85, ;; Default accuracy
                risk-score: risk-score
            }
        )
        
        (ok true)
    )
)

;; Read-only Functions

;; Get shortage alert information
(define-read-only (get-shortage-alert (alert-id uint))
    (map-get? shortage-alerts { alert-id: alert-id })
)

;; Get alternative medication information
(define-read-only (get-alternative-medication (alternative-id uint))
    (map-get? alternative-medications { alternative-id: alternative-id })
)

;; Get healthcare provider information
(define-read-only (get-healthcare-provider (provider-id uint))
    (map-get? healthcare-providers { provider-id: provider-id })
)

;; Get supply request information
(define-read-only (get-supply-request (request-id uint))
    (map-get? supply-requests { request-id: request-id })
)

;; Get monitoring data
(define-read-only (get-monitoring-data (drug-name (string-ascii 64)) (period uint))
    (map-get? monitoring-data { drug-name: drug-name, period: period })
)

;; Check if emergency response is active
(define-read-only (is-emergency-active)
    (var-get emergency-response-active)
)

;; Get current alert count by status
(define-read-only (get-critical-alert-count)
    ;; This would require iteration in a real implementation
    ;; For now, return emergency status
    (if (var-get emergency-response-active) u1 u0)
)

;; Check if drug needs immediate attention
(define-read-only (needs-immediate-attention (drug-name (string-ascii 64)))
    ;; Simple check based on current monitoring
    (match (map-get? monitoring-data { drug-name: drug-name, period: stacks-block-height })
        monitoring-info
        (>= (get risk-score monitoring-info) u500)
        false
    )
)

