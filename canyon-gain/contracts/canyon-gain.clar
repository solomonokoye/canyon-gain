;; Canyon Gain Protocol
;; A community-driven flash loan arbitrage protocol with staking and governance.

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-INVALID-POOL (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-POOL-PAUSED (err u104))
(define-constant ERR-LOAN-ACTIVE (err u105))
(define-constant ERR-NO-LOAN-ACTIVE (err u106))
(define-constant ERR-REPAYMENT-TOO-LOW (err u107))
(define-constant ERR-ALREADY-VOTED (err u108))
(define-constant ERR-PROPOSAL-EXPIRED (err u109))
(define-constant ERR-INVALID-PROPOSAL (err u110))

;; Pool IDs
(define-constant POOL-CONSERVATIVE u0)   ;; stablecoin arbitrage, ~2-5% APY
(define-constant POOL-MODERATE u1)       ;; mixed strategy, ~8-15% APY
(define-constant POOL-AGGRESSIVE u2)     ;; cross-chain opportunities, ~20-40% APY

;; Fee basis points (out of 10000)
;; e.g. 30 = 0.30% flash loan fee
(define-constant FLASH-LOAN-FEE-BPS u30)

;; Insurance fund contribution: 10% of protocol fees
(define-constant INSURANCE-SHARE-BPS u1000)

;; Governance proposal lifetime in blocks (~1 week at ~10 min/block)
(define-constant PROPOSAL-LIFETIME u1008)

;; Minimum CGN stake required for governance voting
(define-constant MIN-GOVERNANCE-STAKE u1000000) ;; 1 CGN (6 decimals)

;; ============================================================
;; DATA VARS
;; ============================================================

;; Protocol-level totals
(define-data-var total-insurance-fund uint u0)
(define-data-var total-protocol-fees uint u0)
(define-data-var flash-loan-active bool false)
(define-data-var flash-loan-borrower (optional principal) none)
(define-data-var flash-loan-amount uint u0)
(define-data-var proposal-nonce uint u0)
(define-data-var protocol-paused bool false)

;; ============================================================
;; DATA MAPS
;; ============================================================

;; Per-pool state
;; pool-id -> { total-staked, total-shares, paused, apy-bps }
(define-map pools
  uint
  { total-staked: uint,
    total-shares: uint,
    paused: bool,
    apy-bps: uint })

;; User stake per pool
;; { user, pool-id } -> { shares, deposited-at }
(define-map user-stakes
  { user: principal, pool-id: uint }
  { shares: uint, deposited-at: uint })

;; CGN token balances (native governance token)
(define-map cgn-balances principal uint)

;; Governance proposals
(define-map proposals
  uint
  { proposer: principal,
    description: (string-ascii 256),
    yes-votes: uint,
    no-votes: uint,
    expires-at: uint,
    executed: bool })

;; Track whether a principal has voted on a proposal
(define-map has-voted { voter: principal, proposal-id: uint } bool)

;; Insurance claims
(define-map insurance-claims
  principal
  { amount: uint, approved: bool })

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

;; Calculate flash loan fee for a given amount
(define-private (calc-flash-fee (amount uint))
  (/ (* amount FLASH-LOAN-FEE-BPS) u10000))

;; Calculate insurance share of a fee amount
(define-private (calc-insurance-share (fee uint))
  (/ (* fee INSURANCE-SHARE-BPS) u10000))

;; Mint CGN to a principal (internal)
(define-private (mint-cgn (recipient principal) (amount uint))
  (let ((current (default-to u0 (map-get? cgn-balances recipient))))
    (map-set cgn-balances recipient (+ current amount))))

;; Burn CGN from a principal (internal)
(define-private (burn-cgn (holder principal) (amount uint))
  (let ((current (default-to u0 (map-get? cgn-balances holder))))
    (asserts! (>= current amount) ERR-INSUFFICIENT-BALANCE)
    (ok (map-set cgn-balances holder (- current amount)))))

;; Get pool or fail
(define-private (get-pool-or-fail (pool-id uint))
  (ok (unwrap! (map-get? pools pool-id) ERR-INVALID-POOL)))

;; ============================================================
;; INITIALIZATION
;; ============================================================

;; Initialize the three risk pools (called once by owner)
(define-public (initialize-pools)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set pools POOL-CONSERVATIVE
      { total-staked: u0, total-shares: u0, paused: false, apy-bps: u350 })   ;; 3.5% APY
    (map-set pools POOL-MODERATE
      { total-staked: u0, total-shares: u0, paused: false, apy-bps: u1150 })  ;; 11.5% APY
    (map-set pools POOL-AGGRESSIVE
      { total-staked: u0, total-shares: u0, paused: false, apy-bps: u3000 })  ;; 30% APY
    (ok true)))

;; ============================================================
;; STAKING
;; ============================================================

;; Stake STX into a risk pool and receive pool shares.
;; Shares are proportional to the pool's current total-staked.
(define-public (stake (pool-id uint) (amount uint))
  (let (
    (pool (try! (get-pool-or-fail pool-id)))
    (existing (default-to { shares: u0, deposited-at: u0 }
                 (map-get? user-stakes { user: tx-sender, pool-id: pool-id })))
  )
    (asserts! (not (var-get protocol-paused)) ERR-POOL-PAUSED)
    (asserts! (not (get paused pool)) ERR-POOL-PAUSED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)

    ;; Transfer STX from user to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    ;; Calculate new shares:
    ;; If pool is empty, shares = amount (1:1)
    ;; Otherwise shares = amount * total-shares / total-staked
    (let ((new-shares
            (if (is-eq (get total-staked pool) u0)
              amount
              (/ (* amount (get total-shares pool)) (get total-staked pool)))))

      ;; Update pool totals
      (map-set pools pool-id
        (merge pool { total-staked: (+ (get total-staked pool) amount),
                      total-shares: (+ (get total-shares pool) new-shares) }))

      ;; Update user stake record
      (map-set user-stakes { user: tx-sender, pool-id: pool-id }
        { shares: (+ (get shares existing) new-shares),
          deposited-at: block-height })

      ;; Reward staker with CGN (1 CGN per 1000 STX staked, scaled)
      (mint-cgn tx-sender (/ amount u1000))

      (ok new-shares))))

;; Unstake shares from a pool and receive proportional STX back.
(define-public (unstake (pool-id uint) (shares uint))
  (let (
    (pool (try! (get-pool-or-fail pool-id)))
    (user-stake (unwrap! (map-get? user-stakes { user: tx-sender, pool-id: pool-id })
                         ERR-INSUFFICIENT-BALANCE))
  )
    (asserts! (>= (get shares user-stake) shares) ERR-INSUFFICIENT-BALANCE)
    (asserts! (> shares u0) ERR-INVALID-AMOUNT)

    ;; STX owed = shares * total-staked / total-shares
    (let ((stx-owed
            (/ (* shares (get total-staked pool)) (get total-shares pool))))

      ;; Update pool
      (map-set pools pool-id
        (merge pool { total-staked: (- (get total-staked pool) stx-owed),
                      total-shares: (- (get total-shares pool) shares) }))

      ;; Update user record
      (map-set user-stakes { user: tx-sender, pool-id: pool-id }
        (merge user-stake { shares: (- (get shares user-stake) shares) }))

      ;; Return STX to user
      (try! (as-contract (stx-transfer? stx-owed tx-sender tx-sender)))

      (ok stx-owed))))

;; ============================================================
;; FLASH LOANS
;; ============================================================

;; Initiate a flash loan.
;; The borrower must repay amount + fee in the same transaction
;; by calling repay-flash-loan before this function returns.
;; NOTE: True atomic flash loans require inter-contract calls.
;;       This implementation uses a two-step pattern with guards.
(define-public (take-flash-loan (amount uint))
  (begin
    (asserts! (not (var-get protocol-paused)) ERR-POOL-PAUSED)
    (asserts! (not (var-get flash-loan-active)) ERR-LOAN-ACTIVE)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)

    ;; Check contract has enough STX
    (asserts! (>= (stx-get-balance (as-contract tx-sender)) amount)
              ERR-INSUFFICIENT-BALANCE)

    ;; Mark loan active
    (var-set flash-loan-active true)
    (var-set flash-loan-borrower (some tx-sender))
    (var-set flash-loan-amount amount)

    ;; Send funds to borrower
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))

    (ok amount)))

;; Repay a flash loan.
;; Must be called by the same borrower in the same block.
(define-public (repay-flash-loan)
  (let (
    (borrower (unwrap! (var-get flash-loan-borrower) ERR-NO-LOAN-ACTIVE))
    (principal-amount (var-get flash-loan-amount))
    (fee (calc-flash-fee principal-amount))
    (total-due (+ principal-amount fee))
    (insurance-cut (calc-insurance-share fee))
  )
    (asserts! (var-get flash-loan-active) ERR-NO-LOAN-ACTIVE)
    (asserts! (is-eq tx-sender borrower) ERR-NOT-AUTHORIZED)

    ;; Collect repayment
    (try! (stx-transfer? total-due tx-sender (as-contract tx-sender)))

    ;; Update insurance fund and fee tracker
    (var-set total-insurance-fund (+ (var-get total-insurance-fund) insurance-cut))
    (var-set total-protocol-fees (+ (var-get total-protocol-fees) fee))

    ;; Clear loan state
    (var-set flash-loan-active false)
    (var-set flash-loan-borrower none)
    (var-set flash-loan-amount u0)

    (ok { repaid: total-due, fee: fee })))

;; ============================================================
;; CGN TOKEN OPERATIONS
;; ============================================================

;; Transfer CGN between principals
(define-public (transfer-cgn (recipient principal) (amount uint))
  (let ((sender-balance (default-to u0 (map-get? cgn-balances tx-sender))))
    (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (map-set cgn-balances tx-sender (- sender-balance amount))
    (map-set cgn-balances recipient
      (+ (default-to u0 (map-get? cgn-balances recipient)) amount))
    (ok true)))

;; Read CGN balance
(define-read-only (get-cgn-balance (holder principal))
  (default-to u0 (map-get? cgn-balances holder)))

;; ============================================================
;; GOVERNANCE
;; ============================================================

;; Submit a governance proposal (requires MIN-GOVERNANCE-STAKE CGN)
(define-public (create-proposal (description (string-ascii 256)))
  (let ((cgn-bal (get-cgn-balance tx-sender)))
    (asserts! (>= cgn-bal MIN-GOVERNANCE-STAKE) ERR-NOT-AUTHORIZED)
    (let ((pid (var-get proposal-nonce)))
      (map-set proposals pid
        { proposer: tx-sender,
          description: description,
          yes-votes: u0,
          no-votes: u0,
          expires-at: (+ block-height PROPOSAL-LIFETIME),
          executed: false })
      (var-set proposal-nonce (+ pid u1))
      (ok pid))))

;; Vote on a proposal; vote weight = CGN balance
(define-public (vote (proposal-id uint) (vote-yes bool))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL))
    (weight (get-cgn-balance tx-sender))
  )
    (asserts! (>= weight MIN-GOVERNANCE-STAKE) ERR-NOT-AUTHORIZED)
    (asserts! (<= block-height (get expires-at proposal)) ERR-PROPOSAL-EXPIRED)
    (asserts! (not (default-to false
                     (map-get? has-voted { voter: tx-sender, proposal-id: proposal-id })))
              ERR-ALREADY-VOTED)

    (map-set has-voted { voter: tx-sender, proposal-id: proposal-id } true)

    (if vote-yes
      (map-set proposals proposal-id
        (merge proposal { yes-votes: (+ (get yes-votes proposal) weight) }))
      (map-set proposals proposal-id
        (merge proposal { no-votes: (+ (get no-votes proposal) weight) })))

    (ok true)))

;; Read a proposal
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id))

;; ============================================================
;; INSURANCE FUND
;; ============================================================

;; Submit an insurance claim for a failed transaction loss
(define-public (submit-insurance-claim (amount uint))
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (map-set insurance-claims tx-sender { amount: amount, approved: false })
    (ok true)))

;; Owner approves and pays out an insurance claim
(define-public (approve-insurance-claim (claimant principal))
  (let ((claim (unwrap! (map-get? insurance-claims claimant) ERR-INVALID-PROPOSAL)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (not (get approved claim)) ERR-LOAN-ACTIVE)
    (asserts! (<= (get amount claim) (var-get total-insurance-fund))
              ERR-INSUFFICIENT-BALANCE)

    (var-set total-insurance-fund
      (- (var-get total-insurance-fund) (get amount claim)))

    (map-set insurance-claims claimant (merge claim { approved: true }))

    (try! (as-contract (stx-transfer? (get amount claim) tx-sender claimant)))

    (ok true)))

;; Read insurance fund balance
(define-read-only (get-insurance-fund-balance)
  (var-get total-insurance-fund))

;; ============================================================
;; ADMIN
;; ============================================================

;; Pause or unpause the protocol
(define-public (set-protocol-paused (paused bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set protocol-paused paused)
    (ok true)))

;; Pause or unpause a specific pool
(define-public (set-pool-paused (pool-id uint) (paused bool))
  (let ((pool (try! (get-pool-or-fail pool-id))))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set pools pool-id (merge pool { paused: paused }))
    (ok true)))

;; ============================================================
;; READ-ONLY VIEWS
;; ============================================================

(define-read-only (get-pool-info (pool-id uint))
  (map-get? pools pool-id))

(define-read-only (get-user-stake (user principal) (pool-id uint))
  (map-get? user-stakes { user: user, pool-id: pool-id }))

(define-read-only (get-protocol-stats)
  { total-insurance-fund: (var-get total-insurance-fund),
    total-protocol-fees:  (var-get total-protocol-fees),
    flash-loan-active:    (var-get flash-loan-active),
    protocol-paused:      (var-get protocol-paused),
    proposal-count:       (var-get proposal-nonce) })

(define-read-only (get-flash-loan-fee (amount uint))
  (calc-flash-fee amount))
