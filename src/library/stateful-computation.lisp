(in-package #:coalton-library)

(coalton-toplevel
  ;;
  ;; StatefulComputation
  ;;

  ;; Definition is in types.lisp

  (declare stateful-computation-put (:state -> (StatefulComputation :state Unit)))
  (define (stateful-computation-put state)
    "A StatefulComputation with state set to be given state. The returned value is Unit."
    (StatefulComputation (fn (_) (Tuple state Unit))))

  (declare stateful-computation-get (StatefulComputation :state :state))
  (define stateful-computation-get
    "A StatefulComputation which returns the current state as the value."
    (StatefulComputation (fn (state) (Tuple state state))))

  (declare run-stateful-computation ((StatefulComputation :state :a) -> :state -> (Tuple :state :a)))
  (define (run-stateful-computation sc)
    "Runs a StatefulComputation to produce a final updated state and value given an initial state"
    (match sc
      ((StatefulComputation fstate)
       fstate)))
  
  ;;
  ;; StatefulComputation instances
  ;;
  (define-instance (Functor (StatefulComputation :state))
    (define (map fa->b sca)
      (StatefulComputation
       (fn (state)
	 (match (run-stateful-computation sca state)
	   ((Tuple state2 a)
	    (Tuple state2 (fa->b a))))))))

  (define-instance (Applicative (StatefulComputation :state))
    (define (pure x)
      (StatefulComputation (fn (state) (Tuple state x))))
    ;; liftA2 uses the resulting state from StateComputation a to compute the state from StateComputation b
    (define (liftA2 fab sca scb)
      (StatefulComputation
       (fn (state1)
	 ;; Apply the initial state to sca
	 (match (run-stateful-computation sca state1)
	   ((Tuple state2 a)
	    ;; Appply the state from sca to scb
	    (match (run-stateful-computation scb state2)
	      ((Tuple state3 b)
	       (Tuple state3 (fab a b))))))))))

  (define-instance (Monad (StatefulComputation :state))
    (define (>>= sca fa->scb)
      (StatefulComputation
       (fn (state1)
	 (match (run-stateful-computation sca state1)
	   ((Tuple state2 a)
	    ;; Use the a to compute the scb,
	    ;; and apply the state from sca to the scb
	    (run-stateful-computation (fa->scb a) state2))))))))
