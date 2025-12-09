(coalton-library/utils:defstdlib-package #:coalton-library/monad/classes
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-compatibility)
  (:local-nicknames
   (#:compat #:coalton-compatibility))
  (:export
   #:LiftTo
   #:lift-to

   #:MonadEnvironment
   #:ask
   #:local
   #:asks

   #:MonadState
   #:get
   #:put
   #:modify
   ))

(in-package #:coalton-library/monad/classes)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel

  (define-class ((Monad :m) (Monad :r) => LiftTo :m :r)
     "A monad, :m, which can be lifted to :r. Typically because :m is a MonadTransformer or :m and :r are the same."
    (lift-to (:m :a -> :r :a)))

  (define-instance (Monad :m => LiftTo :m :m)
    (inline)
    (define (lift-to x) x))

  (define-instance ((Monad :m) (Monad (:t :m)) (MonadTransformer :t) => LiftTo :m (:t :m))
    (inline)
    (define lift-to lift))

  (define-class (Monad :m => MonadEnvironment :env :m (:m -> :env))
    "A monad capable of a function in a computation environment."
    (ask
     "Retrieves the computation environment."
     (:m :env))
    (local
     "Run a computation in a modified environment."
     ((:env -> :env) -> :m :a -> :m :a))
    (asks
     "Retrieve an aspect of the computation environment."
     ((:env -> :a) -> :m :a)))

  (define-class (Monad :m => MonadState :s :m (:m -> :s))
    "A monad capable of tracking state in a computation."
    (get
     "Retrieve the computation state."
     (:m :s))
    (put
     "Set the state to a given value."
     (:s -> :m Unit))
    (modify
     "Modify the computation state, discarding the old state."
     ((:s -> :s) -> :m Unit))))

(compat:try-lock-package "COALTON-LIBRARY/MONAD/CLASSES")
