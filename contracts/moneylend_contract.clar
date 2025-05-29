;; moneylend_contract
;; A comprehensive DeFi money lending protocol for Stacks blockchain
;; Supports lending, borrowing, collateral management, and liquidations

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_FUNDS (err u101))
(define-constant ERR_LOAN_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_REPAID (err u103))
(define-constant ERR_INSUFFICIENT_COLLATERAL (err u104))
(define-constant ERR_LOAN_NOT_DUE (err u105))
(define-constant ERR_INVALID_AMOUNT (err u106))
(define-constant ERR_POOL_INSUFFICIENT (err u107))
(define-constant ERR_LIQUIDATION_NOT_ALLOWED (err u108))
(define-constant ERR_COLLATERAL_RATIO_TOO_LOW (err u109))

;; Loan parameters
(define-constant MIN_LOAN_AMOUNT u1000000) ;; 1 STX minimum
(define-constant MAX_LTV u75) ;; 75% loan-to-value ratio
(define-constant LIQUIDATION_THRESHOLD u85) ;; 85% liquidation threshold
(define-constant BASE_INTEREST_RATE u5) ;; 5% annual base rate
(define-constant LIQUIDATION_PENALTY u10) ;; 10% penalty

;; data maps and vars
(define-data-var next-loan-id uint u1)
(define-data-var total-pool-balance uint u0)
(define-data-var total-borrowed uint u0)
(define-data-var protocol-fees uint u0)

;; User balances in lending pool
(define-map user-deposits principal uint)

;; Loan structure
(define-map loans uint {
    borrower: principal,
    amount: uint,
    collateral: uint,
    interest-rate: uint,
    start-block: uint,
    due-block: uint,
    is-active: bool,
    is-liquidated: bool
})

;; User loan IDs
(define-map user-loans principal (list 50 uint))

;; Interest rate model variables
(define-data-var utilization-rate uint u0)
(define-data-var current-interest-rate uint BASE_INTEREST_RATE)

;; private functions

;; Calculate interest based on blocks elapsed
(define-private (calculate-interest (principal-amount uint) (interest-rate uint) (blocks-elapsed uint))
    (let (
        (annual-blocks u52560) ;; Approximate blocks per year
        (interest-per-block (/ (* principal-amount interest-rate) (* annual-blocks u100)))
    )
    (* interest-per-block blocks-elapsed))
)

;; Calculate current debt including interest
(define-private (calculate-current-debt (loan-id uint))
    (match (map-get? loans loan-id)
        loan-data 
        (let (
            (blocks-elapsed (- block-height (get start-block loan-data)))
            (interest (calculate-interest (get amount loan-data) (get interest-rate loan-data) blocks-elapsed))
        )
        (+ (get amount loan-data) interest))
        u0
    )
)

;; Check if loan can be liquidated
(define-private (is-liquidatable (loan-id uint))
    (match (map-get? loans loan-id)
        loan-data
        (let (
            (current-debt (calculate-current-debt loan-id))
            (collateral-value (get collateral loan-data))
            (ltv-ratio (/ (* current-debt u100) collateral-value))
        )
        (and 
            (get is-active loan-data)
            (>= ltv-ratio LIQUIDATION_THRESHOLD)
        ))
        false
    )
)

;; Update utilization rate and interest rates
(define-private (update-rates)
    (let (
        (pool-balance (var-get total-pool-balance))
        (borrowed (var-get total-borrowed))
        (total-supply (+ pool-balance borrowed))
    )
    (if (> total-supply u0)
        (let (
            (util-rate (/ (* borrowed u100) total-supply))
            (new-rate (+ BASE_INTEREST_RATE (/ (* util-rate u2) u10))) ;; Dynamic rate based on utilization
        )
        (var-set utilization-rate util-rate)
        (var-set current-interest-rate new-rate))
        (begin
            (var-set utilization-rate u0)
            (var-set current-interest-rate BASE_INTEREST_RATE)
        )
    ))
)

;; Add loan ID to user's loan list
(define-private (add-user-loan (user principal) (loan-id uint))
    (let (
        (current-loans (default-to (list) (map-get? user-loans user)))
    )
    (map-set user-loans user (unwrap! (as-max-len? (append current-loans loan-id) u50) false)))
)


