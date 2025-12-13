(coalton-library/utils:defstdlib-package #:coalton-library/monad/state
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-compatibility)
  (:local-nicknames
   (#:compat #:coalton-compatibility))
  (:export
   #:ST
   #:put
   #:get
   #:modify
   #:modify-get
   #:swap
   #:modify-swap
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

  (inline)
  (declare put (:state -> ST :state Unit))
  (define (put state)
    "A StatefulComputation with state set to be the given state. The returned value is Unit."
    (ST (fn (_) (Tuple state Unit))))

  (inline)
  (declare get (ST :state :state))
  (define get
    "A StatefulComputation which returns the current state as the value."
    (ST (fn (state) (Tuple state state))))

  (inline)
  (declare run (ST :state :a -> :state -> Tuple :state :a))
  (define (run sc)
    "Runs a StatefulComputation to produce a final updated state and value given an initial state"
    (match sc
      ((ST fstate)
       fstate)))

  (inline)
  (declare modify ((:state -> :state) -> ST :state Unit))
  (define (modify fs->s)
    "Modify the state in a StatefulComputation, discarding the old state."
    (ST (fn (state)
          (Tuple (fs->s state) Unit))))

  (inline)
  (declare modify-get ((:state -> :state) -> ST :state :state))
  (define (modify-get fs->s)
    "Modify the state in a StatefulComputation, discarding the old state. Return the new state."
    (ST (fn (state)
          (let ((new-state (fs->s state)))
            (Tuple new-state new-state)))))

  (inline)
  (declare swap (:state -> ST :state :state))
  (define (swap state)
    "A StatefulComputation with state set to be the given state. The old state is returned."
    (ST (fn (old-state) (Tuple state old-state))))

  (inline)
  (declare modify-swap ((:state -> :state) -> ST :state :state))
  (define (modify-swap fs->s)
    "Modify the state in a StatefulComputation, returning the old state."
    (ST (fn (old-state)
          (Tuple (fs->s old-state) old-state))))

  ;;
  ;; State Monad instances
  ;;

  (define-instance (Functor (ST :state))
    (inline)
    (define (map fa->b sca)
      (ST
       (fn (state)
         (match (run sca state)
           ((Tuple state2 a)
            (Tuple state2 (fa->b a))))))))

  (define-instance (Applicative (ST :state))
    (inline)
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
    (inline)
    (define (>>= sca fa->scb)
      (ST
       (fn (state1)
         (match (run sca state1)
           ((Tuple state2 a)
            ;; Use the a to compute the mb,
            ;; and apply the state from ma to the mb
            (run (fa->scb a) state2))))))))

(compat:try-lock-package "COALTON-LIBRARY/MONAD/STATE")
