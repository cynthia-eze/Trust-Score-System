;; DECENTRALIZED REPUTATION NETWORK SMART CONTRACT
;; A comprehensive reputation management system that enables users to build trust scores
;; through peer-to-peer ratings with anti-spam protection, reputation thresholds,
;; and comprehensive audit trails for decentralized applications

;; ERROR CONSTANTS
(define-constant ERR-UNAUTHORIZED-ACCESS-DENIED (err u100))
(define-constant ERR-USER-PROFILE-NOT-FOUND (err u101))
(define-constant ERR-INVALID-REPUTATION-SCORE-VALUE (err u102))
(define-constant ERR-USER-ALREADY-REGISTERED-EXISTS (err u103))
(define-constant ERR-INVALID-THRESHOLD-VALUE-RANGE (err u104))
(define-constant ERR-INSUFFICIENT-REPUTATION-LEVEL-REQUIRED (err u105))
(define-constant ERR-INVALID-RATING-VALUE-RANGE (err u106))
(define-constant ERR-SELF-RATING-PROHIBITED-ACTION (err u107))
(define-constant ERR-DUPLICATE-RATING-DETECTED-EXISTS (err u108))
(define-constant ERR-CONTRACT-SUSPENDED-MAINTENANCE (err u109))
(define-constant ERR-INVALID-BULK-OPERATION-SIZE (err u110))
(define-constant ERR-INVALID-CATEGORY-LENGTH-RANGE (err u111))
(define-constant ERR-INVALID-REASON-LENGTH-RANGE (err u112))
(define-constant ERR-INVALID-PRINCIPAL-ADDRESS (err u113))

;; SYSTEM VALIDATION CONSTANTS
(define-constant MAXIMUM-REPUTATION-POINTS-LIMIT u1000)
(define-constant MINIMUM-REPUTATION-POINTS-LIMIT u0)
(define-constant DEFAULT-STARTING-REPUTATION-VALUE u100)
(define-constant LOWEST-ALLOWED-RATING-VALUE u1)
(define-constant HIGHEST-ALLOWED-RATING-VALUE u5)
(define-constant NEUTRAL-RATING-THRESHOLD-VALUE u3)
(define-constant POSITIVE-REPUTATION-BOOST-AMOUNT u10)
(define-constant NEGATIVE-REPUTATION-PENALTY-AMOUNT u5)
(define-constant MAXIMUM-BULK-OPERATIONS-LIMIT u20)
(define-constant MINIMUM-CATEGORY-LENGTH-REQUIRED u1)
(define-constant MAXIMUM-CATEGORY-LENGTH-ALLOWED u50)
(define-constant MINIMUM-REASON-LENGTH-REQUIRED u1)
(define-constant MAXIMUM-REASON-LENGTH-ALLOWED u100)

;; SYSTEM CONFIGURATION VARIABLES
(define-constant NETWORK-ADMINISTRATOR-ADDRESS tx-sender)
(define-data-var minimum-rating-threshold-requirement uint u50)
(define-data-var total-registered-users-count uint u0)
(define-data-var system-operational-status-flag bool true)
(define-data-var next-reputation-event-identifier uint u1)

;; CORE DATA STRUCTURE MAPS
;; Primary reputation tracking for all users
(define-map user-reputation-scores-registry principal uint)

;; Comprehensive user profile information storage
(define-map user-profile-information-registry principal {
    account-creation-timestamp-block: uint,
    last-activity-timestamp-block: uint,
    verification-status-flag: bool,
    user-category-classification: (string-ascii 50),
    total-ratings-received-count: uint,
    cumulative-rating-points-sum: uint
})

;; Detailed reputation event audit trail
(define-map reputation-event-audit-log uint {
    affected-user-principal: principal,
    event-timestamp-block: uint,
    previous-reputation-score: uint,
    updated-reputation-score: uint,
    change-description-reason: (string-ascii 100),
    initiating-user-principal: principal
})

;; Inter-user rating relationship tracking
(define-map user-rating-relationship-registry {rating-provider-principal: principal, rating-recipient-principal: principal} {
    rating-value-score: uint,
    rating-timestamp-block: uint
})

;; ADMINISTRATIVE MANAGEMENT FUNCTIONS
(define-public (configure-minimum-rating-threshold-setting (new-threshold-value-setting uint))
    (begin
        (asserts! (is-eq tx-sender NETWORK-ADMINISTRATOR-ADDRESS) ERR-UNAUTHORIZED-ACCESS-DENIED)
        (asserts! (var-get system-operational-status-flag) ERR-CONTRACT-SUSPENDED-MAINTENANCE)
        (asserts! (and (>= new-threshold-value-setting MINIMUM-REPUTATION-POINTS-LIMIT) 
                      (<= new-threshold-value-setting MAXIMUM-REPUTATION-POINTS-LIMIT)) ERR-INVALID-THRESHOLD-VALUE-RANGE)
        
        (var-set minimum-rating-threshold-requirement new-threshold-value-setting)
        (ok true)
    )
)

(define-public (toggle-system-operational-status-flag)
    (begin
        (asserts! (is-eq tx-sender NETWORK-ADMINISTRATOR-ADDRESS) ERR-UNAUTHORIZED-ACCESS-DENIED)
        (let ((current-operational-status (var-get system-operational-status-flag)))
            (var-set system-operational-status-flag (not current-operational-status))
            (ok (not current-operational-status))
        )
    )
)

(define-public (manually-adjust-user-reputation-score (target-user-principal principal) 
                                                      (new-reputation-value-setting uint) 
                                                      (adjustment-reason-description (string-ascii 100)))
    (begin
        (asserts! (is-eq tx-sender NETWORK-ADMINISTRATOR-ADDRESS) ERR-UNAUTHORIZED-ACCESS-DENIED)
        (asserts! (var-get system-operational-status-flag) ERR-CONTRACT-SUSPENDED-MAINTENANCE)
        (asserts! (and (>= new-reputation-value-setting MINIMUM-REPUTATION-POINTS-LIMIT) 
                      (<= new-reputation-value-setting MAXIMUM-REPUTATION-POINTS-LIMIT)) ERR-INVALID-REPUTATION-SCORE-VALUE)
        (asserts! (and (>= (len adjustment-reason-description) MINIMUM-REASON-LENGTH-REQUIRED)
                      (<= (len adjustment-reason-description) MAXIMUM-REASON-LENGTH-ALLOWED)) ERR-INVALID-REASON-LENGTH-RANGE)
        (asserts! (not (is-eq target-user-principal 'SP000000000000000000002Q6VF78)) ERR-INVALID-PRINCIPAL-ADDRESS)
        
        (let ((current-user-reputation-score (unwrap! (map-get? user-reputation-scores-registry target-user-principal) ERR-USER-PROFILE-NOT-FOUND))
              (reputation-event-identifier (var-get next-reputation-event-identifier)))
            
            ;; Update user reputation score
            (map-set user-reputation-scores-registry target-user-principal new-reputation-value-setting)
            
            ;; Update user profile last activity timestamp
            (try! (update-user-last-activity-timestamp target-user-principal))
            
            ;; Record reputation change event in audit log
            (record-reputation-event-in-audit-log reputation-event-identifier target-user-principal current-user-reputation-score 
                                                  new-reputation-value-setting adjustment-reason-description tx-sender)
            
            (ok new-reputation-value-setting)
        )
    )
)

(define-public (verify-user-account-status (user-to-verify-principal principal))
    (begin
        (asserts! (is-eq tx-sender NETWORK-ADMINISTRATOR-ADDRESS) ERR-UNAUTHORIZED-ACCESS-DENIED)
        (asserts! (var-get system-operational-status-flag) ERR-CONTRACT-SUSPENDED-MAINTENANCE)
        (asserts! (not (is-eq user-to-verify-principal 'SP000000000000000000002Q6VF78)) ERR-INVALID-PRINCIPAL-ADDRESS)
        
        (let ((user-profile-information (unwrap! (map-get? user-profile-information-registry user-to-verify-principal) ERR-USER-PROFILE-NOT-FOUND)))
            (map-set user-profile-information-registry user-to-verify-principal 
                    (merge user-profile-information {
                        verification-status-flag: true,
                        last-activity-timestamp-block: stacks-block-height
                    }))
            
            (ok true)
        )
    )
)

;; USER REGISTRATION AND MANAGEMENT FUNCTIONS
(define-public (register-new-user-account-profile (user-category-classification (string-ascii 50)))
    (let ((registering-user-principal tx-sender)
          (current-stacks-block-height stacks-block-height)
          (reputation-event-identifier (var-get next-reputation-event-identifier)))
        
        (asserts! (var-get system-operational-status-flag) ERR-CONTRACT-SUSPENDED-MAINTENANCE)
        (asserts! (is-none (map-get? user-reputation-scores-registry registering-user-principal)) ERR-USER-ALREADY-REGISTERED-EXISTS)
        (asserts! (and (>= (len user-category-classification) MINIMUM-CATEGORY-LENGTH-REQUIRED)
                      (<= (len user-category-classification) MAXIMUM-CATEGORY-LENGTH-ALLOWED)) ERR-INVALID-CATEGORY-LENGTH-RANGE)
        
        ;; Initialize user reputation score with default value
        (map-set user-reputation-scores-registry registering-user-principal DEFAULT-STARTING-REPUTATION-VALUE)
        
        ;; Create comprehensive user profile
        (map-set user-profile-information-registry registering-user-principal {
            account-creation-timestamp-block: current-stacks-block-height,
            last-activity-timestamp-block: current-stacks-block-height,
            verification-status-flag: false,
            user-category-classification: user-category-classification,
            total-ratings-received-count: u0,
            cumulative-rating-points-sum: u0
        })
        
        ;; Log initial reputation event for audit trail
        (record-reputation-event-in-audit-log reputation-event-identifier registering-user-principal u0 
                                              DEFAULT-STARTING-REPUTATION-VALUE "Account registration initialization" registering-user-principal)
        
        ;; Increment total registered users counter
        (var-set total-registered-users-count (+ (var-get total-registered-users-count) u1))
        
        (ok DEFAULT-STARTING-REPUTATION-VALUE)
    )
)

;; PEER-TO-PEER RATING SYSTEM FUNCTIONS
(define-public (submit-user-rating-evaluation (recipient-user-principal principal) (rating-score-value uint))
    (let ((rating-provider-principal tx-sender)
          (provider-reputation-score (unwrap! (map-get? user-reputation-scores-registry rating-provider-principal) ERR-USER-PROFILE-NOT-FOUND))
          (recipient-reputation-score (unwrap! (map-get? user-reputation-scores-registry recipient-user-principal) ERR-USER-PROFILE-NOT-FOUND))
          (recipient-profile-information (unwrap! (map-get? user-profile-information-registry recipient-user-principal) ERR-USER-PROFILE-NOT-FOUND))
          (existing-rating-relationship (map-get? user-rating-relationship-registry {rating-provider-principal: rating-provider-principal, rating-recipient-principal: recipient-user-principal}))
          (reputation-event-identifier (var-get next-reputation-event-identifier)))
        
        (asserts! (var-get system-operational-status-flag) ERR-CONTRACT-SUSPENDED-MAINTENANCE)
        (asserts! (not (is-eq rating-provider-principal recipient-user-principal)) ERR-SELF-RATING-PROHIBITED-ACTION)
        (asserts! (and (>= rating-score-value LOWEST-ALLOWED-RATING-VALUE) 
                      (<= rating-score-value HIGHEST-ALLOWED-RATING-VALUE)) ERR-INVALID-RATING-VALUE-RANGE)
        (asserts! (>= provider-reputation-score (var-get minimum-rating-threshold-requirement)) ERR-INSUFFICIENT-REPUTATION-LEVEL-REQUIRED)
        (asserts! (is-none existing-rating-relationship) ERR-DUPLICATE-RATING-DETECTED-EXISTS)
        
        ;; Record the new rating relationship
        (map-set user-rating-relationship-registry {rating-provider-principal: rating-provider-principal, rating-recipient-principal: recipient-user-principal} {
            rating-value-score: rating-score-value,
            rating-timestamp-block: stacks-block-height
        })
        
        ;; Calculate and apply reputation adjustment
        (let ((reputation-modification-amount (calculate-reputation-adjustment-amount rating-score-value))
              (new-reputation-value-calculated (apply-reputation-bounds-limits (+ recipient-reputation-score reputation-modification-amount)))
              (updated-total-ratings-count (+ (get total-ratings-received-count recipient-profile-information) u1))
              (updated-cumulative-points-sum (+ (get cumulative-rating-points-sum recipient-profile-information) rating-score-value)))
            
            ;; Update recipient's reputation score
            (map-set user-reputation-scores-registry recipient-user-principal new-reputation-value-calculated)
            
            ;; Update recipient's profile statistics
            (map-set user-profile-information-registry recipient-user-principal 
                    (merge recipient-profile-information {
                        last-activity-timestamp-block: stacks-block-height,
                        total-ratings-received-count: updated-total-ratings-count,
                        cumulative-rating-points-sum: updated-cumulative-points-sum
                    }))
            
            ;; Log reputation change event for audit trail
            (record-reputation-event-in-audit-log reputation-event-identifier recipient-user-principal recipient-reputation-score 
                                                  new-reputation-value-calculated "Peer rating evaluation received" rating-provider-principal)
            
            (ok new-reputation-value-calculated)
        )
    )
)

;; BULK OPERATIONS FUNCTIONS
(define-public (batch-verify-multiple-users (users-to-verify-list (list 20 principal)))
    (begin
        (asserts! (is-eq tx-sender NETWORK-ADMINISTRATOR-ADDRESS) ERR-UNAUTHORIZED-ACCESS-DENIED)
        (asserts! (var-get system-operational-status-flag) ERR-CONTRACT-SUSPENDED-MAINTENANCE)
        (asserts! (<= (len users-to-verify-list) MAXIMUM-BULK-OPERATIONS-LIMIT) ERR-INVALID-BULK-OPERATION-SIZE)
        
        (ok (map process-single-user-verification users-to-verify-list))
    )
)

;; READ-ONLY QUERY FUNCTIONS
(define-read-only (get-user-reputation-score-value (queried-user-principal principal))
    (map-get? user-reputation-scores-registry queried-user-principal)
)

(define-read-only (get-user-profile-information-details (queried-user-principal principal))
    (map-get? user-profile-information-registry queried-user-principal)
)

(define-read-only (get-comprehensive-user-statistics-summary (queried-user-principal principal))
    (let ((user-profile-information (map-get? user-profile-information-registry queried-user-principal)))
        (match user-profile-information
            profile-information-data (let ((total-ratings-received (get total-ratings-received-count profile-information-data))
                                          (cumulative-points-sum (get cumulative-rating-points-sum profile-information-data))
                                          (average-rating-calculated (if (> total-ratings-received u0) 
                                                                        (/ cumulative-points-sum total-ratings-received) u0)))
                (some {
                    total-ratings-received-count: total-ratings-received,
                    cumulative-rating-points-sum: cumulative-points-sum,
                    average-rating-score-calculated: average-rating-calculated,
                    current-reputation-score: (default-to u0 (map-get? user-reputation-scores-registry queried-user-principal))
                }))
            none
        )
    )
)

(define-read-only (get-rating-relationship-between-users (rating-provider-principal principal) (rating-recipient-principal principal))
    (map-get? user-rating-relationship-registry {rating-provider-principal: rating-provider-principal, rating-recipient-principal: rating-recipient-principal})
)

(define-read-only (get-reputation-event-audit-details (event-identifier uint))
    (map-get? reputation-event-audit-log event-identifier)
)

(define-read-only (get-system-configuration-settings)
    {
        minimum-rating-threshold-requirement: (var-get minimum-rating-threshold-requirement),
        total-registered-users-count: (var-get total-registered-users-count),
        system-operational-status-flag: (var-get system-operational-status-flag),
        network-administrator-address: NETWORK-ADMINISTRATOR-ADDRESS,
        next-event-identifier: (var-get next-reputation-event-identifier)
    }
)

(define-read-only (can-user-provide-rating-evaluation (potential-rater-principal principal) (potential-recipient-principal principal))
    (and 
        (var-get system-operational-status-flag)
        (is-some (map-get? user-reputation-scores-registry potential-rater-principal))
        (is-some (map-get? user-reputation-scores-registry potential-recipient-principal))
        (not (is-eq potential-rater-principal potential-recipient-principal))
        (>= (default-to u0 (map-get? user-reputation-scores-registry potential-rater-principal)) 
            (var-get minimum-rating-threshold-requirement))
        (is-none (map-get? user-rating-relationship-registry {rating-provider-principal: potential-rater-principal, rating-recipient-principal: potential-recipient-principal}))
    )
)

(define-read-only (check-reputation-threshold-compliance-status (user-to-check-principal principal))
    (let ((user-reputation-score (map-get? user-reputation-scores-registry user-to-check-principal)))
        (match user-reputation-score
            reputation-score-value (>= reputation-score-value (var-get minimum-rating-threshold-requirement))
            false
        )
    )
)

;; PRIVATE UTILITY HELPER FUNCTIONS
(define-private (calculate-reputation-adjustment-amount (received-rating-score uint))
    (if (>= received-rating-score u4)
        POSITIVE-REPUTATION-BOOST-AMOUNT
        (if (is-eq received-rating-score NEUTRAL-RATING-THRESHOLD-VALUE)
            u0
            (- u0 NEGATIVE-REPUTATION-PENALTY-AMOUNT)
        )
    )
)

(define-private (apply-reputation-bounds-limits (reputation-value-input uint))
    (if (> reputation-value-input MAXIMUM-REPUTATION-POINTS-LIMIT)
        MAXIMUM-REPUTATION-POINTS-LIMIT
        (if (< reputation-value-input MINIMUM-REPUTATION-POINTS-LIMIT)
            MINIMUM-REPUTATION-POINTS-LIMIT
            reputation-value-input
        )
    )
)

(define-private (update-user-last-activity-timestamp (user-account-principal principal))
    (let ((user-profile-information (unwrap! (map-get? user-profile-information-registry user-account-principal) ERR-USER-PROFILE-NOT-FOUND)))
        (map-set user-profile-information-registry user-account-principal 
                (merge user-profile-information {
                    last-activity-timestamp-block: stacks-block-height
                }))
        (ok true)
    )
)

(define-private (record-reputation-event-in-audit-log (event-identifier uint) 
                                                     (affected-user-principal principal) 
                                                     (old-reputation-score uint) 
                                                     (new-reputation-score uint) 
                                                     (event-description-reason (string-ascii 100)) 
                                                     (initiator-principal principal))
    (begin
        (map-set reputation-event-audit-log event-identifier {
            affected-user-principal: affected-user-principal,
            event-timestamp-block: stacks-block-height,
            previous-reputation-score: old-reputation-score,
            updated-reputation-score: new-reputation-score,
            change-description-reason: event-description-reason,
            initiating-user-principal: initiator-principal
        })
        (var-set next-reputation-event-identifier (+ event-identifier u1))
    )
)

(define-private (process-single-user-verification (user-to-verify-principal principal))
    (let ((user-profile-information (map-get? user-profile-information-registry user-to-verify-principal)))
        (match user-profile-information
            profile-information-data (begin
                (map-set user-profile-information-registry user-to-verify-principal 
                        (merge profile-information-data {
                            verification-status-flag: true,
                            last-activity-timestamp-block: stacks-block-height
                        }))
                true
            )
            false
        )
    )
)