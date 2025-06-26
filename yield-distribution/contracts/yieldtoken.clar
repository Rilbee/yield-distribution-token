;; Yield Distribution Token

;; Constants
(define-constant contract-admin tx-sender)
(define-constant err-admin-only (err u100))
(define-constant err-insufficient-funds (err u102))
(define-constant err-unauthorized-sender (err u103))
(define-constant err-no-yield-available (err u108))
(define-constant err-transaction-failed (err u110))
(define-constant err-record-update-failed (err u111))
(define-constant err-balance-modification-failed (err u112))
(define-constant err-invalid-value (err u113))

;; Data Variables
(define-data-var token-title (string-ascii 32) "Yield Distribution Token")
(define-data-var token-ticker (string-ascii 10) "YDT")
(define-data-var token-precision uint u6)
(define-data-var circulating-supply uint u0)
(define-data-var yield-per-token uint u0)

;; Maps
(define-map token-holdings principal uint)
(define-map yield-allocated principal uint)

;; Private Functions
(define-private (is-contract-admin)
    (is-eq tx-sender contract-admin))

(define-private (verify-sender (user principal))
    (if (and (is-eq tx-sender user) (is-some (map-get? token-holdings user)))
        (ok true)
        err-unauthorized-sender))

;; Balance Management
(define-private (increase-holdings (wallet principal) (tokens uint))
    (let ((updated-balance (+ (default-to u0 (map-get? token-holdings wallet)) tokens)))
        (if (map-set token-holdings wallet updated-balance)
            (ok true)
            err-balance-modification-failed)))

(define-private (decrease-holdings (wallet principal) (tokens uint))
    (let ((current-holdings (default-to u0 (map-get? token-holdings wallet))))
        (asserts! (>= current-holdings tokens) err-insufficient-funds)
        (if (map-set token-holdings wallet (- current-holdings tokens))
            (ok true)
            err-balance-modification-failed)))

;; Yield Functions
(define-private (compute-pending-yield (user principal))
    (let ((holdings (default-to u0 (map-get? token-holdings user)))
          (allocated (default-to u0 (map-get? yield-allocated user))))
        (- (* holdings (var-get yield-per-token)) allocated)))

(define-private (refresh-yield-allocation (user principal))
    (let ((new-allocation (* (default-to u0 (map-get? token-holdings user)) 
                            (var-get yield-per-token))))
        (if (map-set yield-allocated user new-allocation)
            (ok true)
            err-record-update-failed)))

;; Public Functions
(define-public (transfer-tokens (tokens uint) 
                               (from principal) 
                               (to principal) 
                               (note (optional (buff 34))))
    (begin
        (asserts! (> tokens u0) err-invalid-value)
        (asserts! (is-some (map-get? token-holdings from)) err-unauthorized-sender)
        (asserts! (is-some (map-get? token-holdings to)) err-unauthorized-sender)
        (try! (verify-sender from))
        (try! (refresh-yield-allocation from))
        (try! (refresh-yield-allocation to))
        (try! (decrease-holdings from tokens))
        (try! (increase-holdings to tokens))
        (ok true)))

(define-public (create-tokens (tokens uint) (beneficiary principal))
    (begin
        (asserts! (is-contract-admin) err-admin-only)
        (asserts! (> tokens u0) err-invalid-value)
        (asserts! (is-some (map-get? token-holdings beneficiary)) err-unauthorized-sender)
        (try! (refresh-yield-allocation beneficiary))
        (var-set circulating-supply (+ (var-get circulating-supply) tokens))
        (try! (increase-holdings beneficiary tokens))
        (ok true)))

(define-public (distribute-yield)
    (let ((yield-payment (stx-get-balance tx-sender)))
        (begin
            (asserts! (is-contract-admin) err-admin-only)
            (asserts! (> (var-get circulating-supply) u0) err-transaction-failed)
            (var-set yield-per-token 
                (+ (var-get yield-per-token)
                   (/ (* yield-payment u1000000) (var-get circulating-supply))))
            (try! (stx-transfer? yield-payment tx-sender (as-contract tx-sender)))
            (ok true))))

(define-public (withdraw-yield)
    (let ((payment (compute-pending-yield tx-sender)))
        (begin
            (asserts! (> payment u0) err-no-yield-available)
            (try! (refresh-yield-allocation tx-sender))
            (try! (as-contract (stx-transfer? payment tx-sender tx-sender)))
            (ok true))))

;; Read-only Functions
(define-read-only (get-token-name)
    (ok (var-get token-title)))

(define-read-only (get-token-symbol)
    (ok (var-get token-ticker)))

(define-read-only (get-token-decimals)
    (ok (var-get token-precision)))

(define-read-only (get-token-balance (holder principal))
    (ok (default-to u0 (map-get? token-holdings holder))))

(define-read-only (get-circulating-supply)
    (ok (var-get circulating-supply)))

(define-read-only (get-pending-yield (user principal))
    (ok (compute-pending-yield user)))