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

;; public functions

;; Deposit STX to lending pool
(define-public (deposit (amount uint))
    (begin
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (let (
            (current-balance (default-to u0 (map-get? user-deposits tx-sender)))
            (new-balance (+ current-balance amount))
        )
        (map-set user-deposits tx-sender new-balance)
        (var-set total-pool-balance (+ (var-get total-pool-balance) amount))
        (update-rates)
        (ok new-balance))
    )
)

;; Withdraw STX from lending pool
(define-public (withdraw (amount uint))
    (let (
        (user-balance (default-to u0 (map-get? user-deposits tx-sender)))
        (pool-balance (var-get total-pool-balance))
    )
    (asserts! (>= user-balance amount) ERR_INSUFFICIENT_FUNDS)
    (asserts! (>= pool-balance amount) ERR_POOL_INSUFFICIENT)
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (map-set user-deposits tx-sender (- user-balance amount))
    (var-set total-pool-balance (- pool-balance amount))
    (update-rates)
    (ok (- user-balance amount)))
)

;; Borrow STX with collateral
(define-public (borrow (amount uint) (collateral uint))
    (let (
        (loan-id (var-get next-loan-id))
        (pool-balance (var-get total-pool-balance))
        (ltv-ratio (/ (* amount u100) collateral))
        (current-rate (var-get current-interest-rate))
        (due-block (+ block-height u52560)) ;; 1 year from now
    )
    (asserts! (>= amount MIN_LOAN_AMOUNT) ERR_INVALID_AMOUNT)
    (asserts! (<= ltv-ratio MAX_LTV) ERR_COLLATERAL_RATIO_TOO_LOW)
    (asserts! (>= pool-balance amount) ERR_POOL_INSUFFICIENT)
    (asserts! (> collateral u0) ERR_INSUFFICIENT_COLLATERAL)
    
    ;; Transfer collateral from borrower
    (try! (stx-transfer? collateral tx-sender (as-contract tx-sender)))
    
    ;; Transfer loan amount to borrower
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    
    ;; Create loan record
    (map-set loans loan-id {
        borrower: tx-sender,
        amount: amount,
        collateral: collateral,
        interest-rate: current-rate,
        start-block: block-height,
        due-block: due-block,
        is-active: true,
        is-liquidated: false
    })
    
    ;; Update global state
    (add-user-loan tx-sender loan-id)
    (var-set next-loan-id (+ loan-id u1))
    (var-set total-pool-balance (- pool-balance amount))
    (var-set total-borrowed (+ (var-get total-borrowed) amount))
    (update-rates)
    (ok loan-id))
)

;; Repay loan
(define-public (repay-loan (loan-id uint))
    (match (map-get? loans loan-id)
        loan-data
        (let (
            (borrower (get borrower loan-data))
            (current-debt (calculate-current-debt loan-id))
            (collateral (get collateral loan-data))
        )
        (asserts! (is-eq tx-sender borrower) ERR_UNAUTHORIZED)
        (asserts! (get is-active loan-data) ERR_ALREADY_REPAID)
        
        ;; Transfer repayment amount
        (try! (stx-transfer? current-debt tx-sender (as-contract tx-sender)))
        
        ;; Return collateral
        (try! (as-contract (stx-transfer? collateral tx-sender borrower)))
        
        ;; Update loan status
        (map-set loans loan-id (merge loan-data { is-active: false }))
        
        ;; Update global state
        (let (
            (principal-amount (get amount loan-data))
            (interest-earned (- current-debt principal-amount))
        )
        (var-set total-pool-balance (+ (var-get total-pool-balance) current-debt))
        (var-set total-borrowed (- (var-get total-borrowed) principal-amount))
        (var-set protocol-fees (+ (var-get protocol-fees) (/ interest-earned u10))) ;; 10% of interest as protocol fee
        (update-rates)
        (ok current-debt)))
        ERR_LOAN_NOT_FOUND
    )
)

;; Liquidate undercollateralized loan
(define-public (liquidate-loan (loan-id uint))
    (match (map-get? loans loan-id)
        loan-data
        (let (
            (borrower (get borrower loan-data))
            (current-debt (calculate-current-debt loan-id))
            (collateral (get collateral loan-data))
            (penalty-amount (/ (* collateral LIQUIDATION_PENALTY) u100))
            (liquidator-reward (- collateral penalty-amount))
        )
        (asserts! (get is-active loan-data) ERR_ALREADY_REPAID)
        (asserts! (is-liquidatable loan-id) ERR_LIQUIDATION_NOT_ALLOWED)
        
        ;; Transfer liquidator reward
        (try! (as-contract (stx-transfer? liquidator-reward tx-sender tx-sender)))
        
        ;; Keep penalty as protocol fee
        (var-set protocol-fees (+ (var-get protocol-fees) penalty-amount))
        
        ;; Mark loan as liquidated
        (map-set loans loan-id (merge loan-data { 
            is-active: false, 
            is-liquidated: true 
        }))
        
        ;; Update global state
        (let ((principal-amount (get amount loan-data)))
        (var-set total-borrowed (- (var-get total-borrowed) principal-amount))
        (update-rates)
        (ok liquidator-reward)))
        ERR_LOAN_NOT_FOUND
    )
)

;; Get loan details
(define-read-only (get-loan (loan-id uint))
    (map-get? loans loan-id)
)

;; Get user's deposit balance
(define-read-only (get-user-balance (user principal))
    (default-to u0 (map-get? user-deposits user))
)

;; Get current debt for a loan
(define-read-only (get-current-debt (loan-id uint))
    (calculate-current-debt loan-id)
)

;; Get protocol statistics
(define-read-only (get-protocol-stats)
    {
        total-pool-balance: (var-get total-pool-balance),
        total-borrowed: (var-get total-borrowed),
        utilization-rate: (var-get utilization-rate),
        current-interest-rate: (var-get current-interest-rate),
        protocol-fees: (var-get protocol-fees)
    }
)

;; Get user's loans
(define-read-only (get-user-loans (user principal))
    (default-to (list) (map-get? user-loans user))
)

;; Check if loan is liquidatable
(define-read-only (can-liquidate (loan-id uint))
    (is-liquidatable loan-id)
)

;; Emergency functions (only contract owner)
(define-public (emergency-pause)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        ;; Could implement pause functionality here
        (ok true)
    )
)

;; Withdraw protocol fees (only contract owner)
(define-public (withdraw-fees (amount uint))
    (let ((fees (var-get protocol-fees)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= amount fees) ERR_INSUFFICIENT_FUNDS)
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
    (var-set protocol-fees (- fees amount))
    (ok amount))
)


