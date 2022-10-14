(coalton-library/utils:defstdlib-package #:coalton-library/monad/state
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes)
  (:export
   #:ST
   #:put
   #:get
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

  (declare put (:state -> ST :state Unit))
  (define (put state)
    "A StatefulComputation with state set to be given state. The returned value is Unit."
    (ST (fn (_) (Tuple state Unit))))

  (declare get (ST :state :state))
  (define get
    "A StatefulComputation which returns the current state as the value."
    (ST (fn (state) (Tuple state state))))

  (declare run (ST :state :a -> :state -> Tuple :state :a))
  (define (run sc)
    "Runs a StatefulComputation to produce a final updated state and value given an initial state"
    (match sc
      ((ST fstate)
       fstate)))
  
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
	    ;; Appply the state from sca to scb
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
	    (run (fa->scb a) state2))))))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MONAD/STATE")
