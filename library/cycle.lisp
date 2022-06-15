(coalton-library/utils:defstdlib-package #:coalton-library/cycle
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/builtin)
  (:export
   #:Cycle
   #:make-uninit
   #:initialize
   #:acyclic
   #:make-cycle))
(cl:in-package #:coalton-library/cycle)

(cl:defstruct (%cycle (:constructor %make-cycle))
  (initp cl:nil :type cl:boolean)
  inner)
#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type %cycle))

(coalton-toplevel
  (repr :native %cycle)
  (define-type (Cycle :t)
    "Wrapper type for cyclical data.

A circular list may be encoded as (Cycle (List :elt)).")

  (declare cycle-init? ((Cycle :t) -> Boolean))
  (define (cycle-init? cyc)
    (lisp Boolean (cyc)
      (%cycle-initp cyc)))

  (declare cycle-inner ((Cycle :t) -> (Optional :t)))
  (define (cycle-inner cyc)
    (if (cycle-init? cyc)
        (Some (lisp :t (cyc)
                (%cycle-inner cyc)))
        None))

  (declare make-uninit (Unit -> (Cycle :t)))
  (define (make-uninit)
    (lisp (Cycle :t) ()
      (%make-cycle)))

  (declare initialize ((Cycle :t) -> :t -> (Cycle :t)))
  (define (initialize cyc inner)
    (lisp :any (inner cyc)
      (cl:setf (%cycle-inner cyc) inner
               (%cycle-initp cyc) cl:t))
    cyc)
  
  (declare acyclic (:t -> (Cycle :t)))
  (define (acyclic data)
    (lisp (Cycle :t) (data)
      (%make-cycle :initp cl:t
                   :inner data)))

  (declare make-cycle (((Cycle :t) -> :t) -> (Cycle :t)))
  (define (make-cycle ctor)
    (let cyc = (make-uninit))
    (let data = (ctor cyc))
    (initialize cyc data))

  (define-instance (Functor Cycle)
    (define (map f cyc)
      (match (cycle-inner cyc)
        ((None) (make-uninit))
        ((Some inner) (acyclic (f inner))))))

  (define-instance (Unwrappable Cycle)
    (define (unwrap-or-else err cyc)
      (unwrap-or-else err (cycle-inner cyc)))))
