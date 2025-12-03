;;;;
;;;; This example program uses the classic bank account management example
;;;; to demonstrate how multiple monads can be combined together to manage
;;;; complex effects.
;;;;
;;;; In this program, you can create and close bank accounts with different
;;;; names and balances. You can transfer money between different accounts.
;;;; There is a configurable minimum balance that must be floated in an
;;;; account at all times. Overdraft protection can either be on or off,
;;;; blocking withdrawals below the minimum balance; otherwise, an account
;;;; in the minimum balance cannot be used except in a deposit.
;;;;
;;;; To manage all of the state, context, and possible failures, we combine
;;;; the state monad (to manage our state), the environment monad (to manage
;;;; our static configuration), and the Result monad (to stop running the
;;;; program whenever we encounter an error).
;;;;
;;;; To completely understand the workings of this example program, it will
;;;; be helpful to already have a basic grasp of how the State, Environment,
;;;; and Result monads work.
;;;;
;;;; Example usage:
;;;;
;;;; CL-USER> (in-package :small-coalton-programs/bank)
;;;; #<COMMON-LISP:PACKAGE "SMALL-COALTON-PROGRAMS/BANK">
;;;; SMALL-COALTON-PROGRAMS/BANK> (test-bank-simulation)
;;;; Name:     Checking
;;;; Balance:  85
;;;;
;;;; Name:     Savings
;;;; Balance:  70
;;;;
;;;; --------
;;;; Name:     Reserves
;;;; Balance:  25
;;;;
;;;; Name:     Savings
;;;; Balance:  155
;;;;
;;;; --------
;;;; Result: #.(ERR "Account not found: Checking")
;;;; COALTON::UNIT/UNIT
;;;;

(cl:defpackage :small-coalton-programs/bank
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/result
   #:coalton-library/monad/stateT
   #:coalton-library/monad/identity
   #:coalton-library/monad/environment
   #:coalton-library/monad/resultt
   #:coalton-library/experimental/do-control-core)
  (:local-nicknames
   (#:s #:coalton-library/string)
   (#:m #:coalton-library/ordmap))
  (:export
   :create-account
   :deposit
   :withdraw
   :print-reportM
   :transfer
   :close-account
   :run-bank-simulation))

(cl:in-package :small-coalton-programs/bank)

;;
;; Utility Functions
;;

(coalton-toplevel
  (declare to-string (Into :a String => :a -> String))
  (define to-string into))

;;;
;;; Bank Example
;;;

;;; First, we set up the types that will store all of the data. We alias
;;; a few primitive types to make type signatures more expressive and set
;;; up structs to store our program's Configuration and the data for an
;;; individual account.

(coalton-toplevel
  (define-type-alias AccountName String)
  (define-type-alias Balance Integer)
  (define-type-alias Amount Integer)

  (define-struct Configuration
    (minimum-balance      "Minimum balance that must be floated by an account."                          Balance)
    (overdraft-protection "If True, prevents an account from being withdrawn below the minimum balance." Boolean))

  ;; Workaround for:
  ;; https://github.com/coalton-lang/coalton/issues/1656
  (declare minimum-balance_ (Configuration -> Balance))
  (define (minimum-balance_ conf)
    (.minimum-balance conf))

  ;; Workaround for:
  ;; https://github.com/coalton-lang/coalton/issues/1656
  (declare overdraft-protection_ (Configuration -> Boolean))
  (define (overdraft-protection_ conf)
    (.overdraft-protection conf))

  (declare without-overdraft-protection (Configuration -> Configuration))
  (define (without-overdraft-protection conf)
    (Configuration (.minimum-balance conf) False))

  (define-struct Account
    (name    AccountName)
    (balance Balance))

  (declare add-balance (Amount -> Account -> Account))
  (define (add-balance amount acc)
    (Account (.name acc) (+ (.balance acc) amount)))

  (declare subtract-balance (Amount -> Account -> Account))
  (define (subtract-balance amount acc)
    (Account (.name acc) (- (.balance acc) amount))))

;;; Second, we set up the type that is going to represent the top-level state
;;; of our whole program. We decided that our configuration will be loaded
;;; statically, so it won't need to be part of our state. We'll store a map
;;; from AccountName -> Account as our program's state, and call that `BankState`.
;;; For now, this is still just an immutable type! We'll get into actual state
;;; management later.

(coalton-toplevel
  (define-type-alias BankState (m:OrdMap AccountName Account))

  (declare print-report (BankState -> Unit))
  (define (print-report accounts)
    (for (Account name balance) in (m:values accounts)
      (lisp :a (name balance)
        (cl:format cl:t "Name:~10T~a~%Balance:~10T~a~%~%" name balance)))
    (lisp :a ()
      (cl:format cl:t "--------~%"))
    Unit))

;;; Third, we'll set up an error type to store all of the ways that our program
;;; could go wrong. `BankResult` is a handy alias for `(Result BankError :value)`.
;;; So, for example, a `(BankResult Integer)` type is either a failure (`BankError`)
;;; or a success (`Integer`).

(coalton-toplevel
  (define-type BankError
    (AccountAlreadyExists  AccountName)
    (InvalidDeposit        Amount)
    (InvalidWithdrawal     Amount)
    (InvalidAccountBalance AccountName Balance)
    (RecursiveTransfer     AccountName)
    (AccountNotFound       AccountName)
    (Unknown               String))

  (define-instance (Into BankError String)
    (define (into err)
      (match err
        ((AccountAlreadyExists name)
         (s:concat "An account already exists with name: " name))
        ((InvalidDeposit _) "Cannot deposit a negative amount")
        ((InvalidWithdrawal _) "Cannot withdraw a negative amount")
        ((InvalidAccountBalance name _)
         (s:concat "Account balance below the minimum balance: "
                   name))
        ((RecursiveTransfer name)
         (s:concat "Cannot transfer between the same account: " name))
        ((AccountNotFound name) (s:concat "Account not found: " name))
        ((Unknown s) (s:concat "Unknown Error: " s)))))

  (define-type-alias BankResult
    (Result BankError)))

;;; Fourth, we set up our application's monad. We know that we need to store state, so
;;; we want to have a State (`ST`) monad. But we also want to store our static configuration,
;;; so we want to use the Environment (`EnvT`) monad. We can get both of these features in one
;;; type by wrapping `ST` in an `EnvT`, which we do in this type alias. `BankM`, or "BankMonad",
;;; will be where we run all of our complex domain logic that needs to be able to read and write
;;; to our `BankState` and read our `Configuration`.
;;;
;;; By using `EnvT Configuration`, we're telling it which type to use for the Environment
;;; functions. By using `ST BankState`, we're telling it which type to use for the State functions.
;;; Combined, our `BankM` will be able to function as both a ST and an EnvT monad.
;;;
;;; Finally, the type alias leaves off the value parameter. So a `BankM :val` represents a
;;; computation in our overall bank context (state + configuration) that returns a type of :val.

(coalton-toplevel
  (define-type-alias BankM
    ;; NOTE: Should be changed back from (StateT Identity) -> ST when the inference bugs
    ;; keeping ST from getting wrapped in MonadState are fixed.
    ;; See, e.g., https://github.com/coalton-lang/coalton/issues/1656.
    (EnvT Configuration (StateT BankState Identity)))

  (declare run-bankM (BankM :val -> Configuration -> BankState -> Tuple BankState :val))
  (define (run-bankM bankm conf initial-state)
    "Takes BANKM, a BankM computation to run, CONF, an initial configuration, and an initial
state. Runs the computation, and returns a tuple of the final state and the return value of
the computation."
    (run-identity (run-stateT (run-envT bankm conf) initial-state))))

;;; Fifth, we define some helper functions.

(coalton-toplevel
  (declare get-account (AccountName -> BankState -> BankResult Account))
  (define (get-account account-name accounts)
    (opt->result (AccountNotFound account-name) (m:lookup accounts account-name)))

  (declare get-accountM (AccountName -> BankM (BankResult Account)))
  (define (get-accountM account-name)
    (map (get-account account-name) get))

  (declare check-account-is-valid (Account -> BankM (BankResult Account)))
  (define (check-account-is-valid account)
    (do
     (minimum-balance <- (asks minimum-balance_))
     (if (>= (.balance account) minimum-balance)
         (pure (Ok account))
         (pure (Err (InvalidAccountBalance (.name account) (.balance account)))))))

  (declare set-account (Account -> BankM (BankResult Account)))
  (define (set-account acc)
    "Update/insert an account and return it for convenience."
    (do
     ;; NOTE: Even though modify isn't returning a value in the do block, it's
     ;; still performing a "side effect" by modifying the state in our BankM monad.
     ;; Here, it's changing the BankState (which is a Map from String -> Account)
     ;; by inserting an account with its name as the key.
     (modify (fn (mp)
               (m:insert mp (.name acc) acc)))
     (pure (Ok acc)))))

;;; Finally, we'll create all of the functions that our "user" can use to
;;; manipulate their accounts directly. Note that, at the very top level of our
;;; program, we want to make sure that errors are handled properly. So all of these
;;; functions will return a type of `BankM (BankResult :val)`.

(coalton-toplevel

  ;;; create-account has a simple error handling flow, where we want to perform
  ;;; a series of BankM computations and bail out as soon as we get a BankError.
  ;;; We can do this by wrapping each of our BankM steps in a ResultT and then
  ;;; calling `run-resultT` on the whole thing, which will produce exactly the
  ;;; `BankM (BankResult Account)` type that we want. The macro `do-resultT`
  ;;; does this behind the scenes, eliminating a lot of boilerplate `ResultT`
  ;;; calls.
  ;;;
  ;;; Most of our top-level functions can follow this pattern.

  (declare create-account (AccountName -> Balance -> BankM (BankResult Account)))
  (define (create-account name initial-balance)
    "Adds an account to the BankState and return the created account."
    (do
      (accounts <- get)
      (do-resultT
        (match (get-account name accounts)
          ((Err _) (pure (Ok Unit)))
          ((Ok _) (pure (Err (AccountAlreadyExists name)))))
        (let unvalidated-account = (Account name initial-balance))
        (account <- (check-account-is-valid unvalidated-account))
        (set-account account))))

  (declare deposit (AccountName -> Amount -> BankM (BankResult Account)))
  (define (deposit account-name amount)
    "Deposit AMOUNT into account with ACCOUNT-NAME and return the Account for convenience."
    (do-resultT
      (err-ifM (< amount 0) (InvalidDeposit amount))
      (acc <- (get-accountM account-name))
      (set-account (add-balance amount acc))))

  (declare print-reportM (BankM (BankResult Unit)))
  (define print-reportM
    (do
     (accounts <- get)
     (pure (Ok (print-report accounts)))))

  (declare withdraw (AccountName -> Amount -> BankM (BankResult Account)))
  (define (withdraw account-name amount)
    "Withdraw AMOUNT from account with ACCOUNT-NAME, returning the Account for convenience."
    (do
      (protection? <- (asks overdraft-protection_))
      (minimum <- (asks minimum-balance_))
      (do-resultT
        (err-ifM (< amount 0) (InvalidWithdrawal amount))
        (acc <- (get-accountM account-name))
        (map-errM
         (fn (er)
           (Unknown
            (s:concat "Cannot withdraw from an invalid account: "
                      (into er))))
         (check-account-is-valid acc))
        (let new-account = (subtract-balance amount acc))
        (if (and protection?
                 (< (.balance new-account) minimum))
            (pure (Err (InvalidWithdrawal amount)))
            (set-account new-account)))))

  ;;; Unlike most of our top-level functions, `transfer` has more complex error handling.
  ;;; Instead of just running everything through a `ResultT`, we manually handle the
  ;;; different errors that can occur and return either an `Err` or `Ok` value.

  (declare transfer (AccountName -> AccountName -> Balance -> BankM (BankResult Unit)))
  (define (transfer from-acc-name to-acc-name amount)
    (if (== from-acc-name to-acc-name)
      (pure (Err (RecursiveTransfer from-acc-name)))
      (matchM (withdraw from-acc-name amount)
        ((Err er)
         (pure (Err er)))
        ((Ok _)
         (matchM (deposit to-acc-name amount)
           ;; If the deposit failed, put the money back into the from account!
           ((Err er)
            (do-resultT
              (deposit from-acc-name amount)
              (pure (Err er))))
           ((Ok _)
            (pure (Ok Unit))))))))

  (declare close-account (AccountName -> AccountName -> BankM (BankResult Unit)))
  (define (close-account acc-to-close-name deposit-acc-name)
    (do-resultT
      (acc-to-close <- (get-accountM acc-to-close-name))
      (local
       without-overdraft-protection
       (transfer acc-to-close-name deposit-acc-name (.balance acc-to-close)))
      (okM (modify (fn (mp) (m:remove mp acc-to-close-name)))))))

;;; Finally, we run our bank simulation! We use the `do-resultT` macro, which
;;; wraps a sequence of `ResultT` computations in a single do block and runs them.
;;;
;;; Try changing the commands in the test simulation or the default configuration in
;;; `run-bank-simulation`, and seeing how that affects the outcome.

(coalton-toplevel
  (declare run-bank-simulation (BankM (BankResult :val) -> Tuple BankState (BankResult :val)))
  (define (run-bank-simulation bank-computation)
    (run-bankM bank-computation
               ;; This is the configuration and initial state that is passed into the program.
               (Configuration 10 True)
               m:empty)))

(cl:defun test-bank-simulation ()
  (coalton
   (progn
     (let (Tuple accounts res) =
       (run-bank-simulation
        (do-resultT
          ;; Bob and Alice each open a new account
          (create-account "Alice" 100)
          (create-account "Bob" 100)
          ;; Bob loses a bet to Alice
          (transfer "Bob" "Alice" 10)
          print-reportM ;; Report: Alice->110, Bob->90
          ;; Bob's friend Steve opens an account
          (create-account "Steve" 200)
          ;; Alice uses the money she won from Bob
          (withdraw "Alice" 10)
          ;; Bob closes his account and gives his money to Steve
          (close-account "Bob" "Steve")
          print-reportM ;; Report: Alice->100, Steve->290

          ;; Alice attempts to withdraw 200, but this fails!
          ;; Because we're in a resultT, this will immediately
          ;; end the computation with an error.
          ;;
          ;; If this was not a resultT, if it was just a normal
          ;; do block over the BankM, then this command wouldn't
          ;; execute, but it would return an Err result. If we
          ;; didn't manually handel it, then the remaining commands
          ;; would continue to run.
          (withdraw "Alice" 200)

          ;; None of these commands will execute:
          (create-account "Natasha" 50)
          (transfer "Steve" "Natasha" 10)
          print-reportM
          )))
     (traceobject "Result" (map-err to-string res)))))
