(coalton-library/utils:defstdlib-package #:coalton-library/monad/state
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes)
  (:export
   #:ST
   #:MonadState
   #:put
   #:get
   #:modify
   #:run))

(in-package #:coalton-library/monad/state)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;;
  ;; State Monad
  ;;

  (repr :transparent)
  (define-type (ST :state :value)
    "A computation of a value which may affect the state.
Represented as a closure from initial state to updated state and value."
    (ST (:state -> (Tuple :state :value))))

  (declare put-st (:state -> ST :state Unit))
  (define (put-st state)
    (ST (fn (_) (Tuple state Unit))))

  (declare get-st (ST :state :state))
  (define get-st
    (ST (fn (state) (Tuple state state))))

  (declare run (ST :state :a -> :state -> Tuple :state :a))
  (define (run sc)
    "Runs a StatefulComputation to produce a final updated state and value given an initial state"
    (match sc
      ((ST fstate)
       fstate)))

  ;;
  ;; MonadState Typeclass
  ;;

  (define-class (Monad :m => MonadState :state :m (:m -> :state))
    "A monad capable of representing stateful computations."
    (put
     "A StatefulComputation with state set to be given state. The returned value is Unit."
     (:state -> :m Unit))
    (get
     "A StatefulComputation which returns the current state as the value."
     (:m :state)))

  (declare modify (MonadState :state :m => (:state -> :state) -> :m Unit))
  (define (modify statef)
    "Modify the state in a StatefulComputation, discarding the old state."
    (do
     (state <- get)
     (put (statef state))))

  ;;
  ;; State Monad instances
  ;;
  (define-instance (Functor (ST :state))
    (define (map fa->b sca)
      (ST
       (fn (state)
         (match (run sca state)
           ((Tuple state2 a)
            (Tuple state2 (fa->b a))))))))

  (define-instance (Applicative (ST :state))
    (define (pure x)
      (ST (fn (state) (Tuple state x))))
    ;; liftA2 uses the resulting state from a to compute the state from b
    (define (liftA2 fab sca scb)
      (ST
       (fn (state1)
         ;; Apply the initial state to sca
         (match (run sca state1)
           ((Tuple state2 a)
            ;; Apply the state from sca to scb
            (match (run scb state2)
              ((Tuple state3 b)
               (Tuple state3 (fab a b))))))))))

  (define-instance (Monad (ST :state))
    (define (>>= sca fa->scb)
      (ST
       (fn (state1)
         (match (run sca state1)
           ((Tuple state2 a)
            ;; Use the a to compute the mb,
            ;; and apply the state from ma to the mb
            (run (fa->scb a) state2)))))))

  (define-instance (MonadState :state (ST :state))
    (define put put-st)
    (define get get-st)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MONAD/STATE")
