(coalton-library/utils:defstdlib-package #:coalton-library/monad/statet
  (:use
   #:coalton
   #:coalton-library/functions
   #:coalton-library/classes
   #:coalton-library/monad/classes)
  (:local-nicknames
   (:tp #:coalton-library/tuple))
  (:export
   #:StateT
   #:run-stateT
   #:run-stateT_
   #:map-stateT

   #:lift-stateT

   #:put-stateT
   #:get-stateT
   #:modify-stateT

   ;; Re-export MonadState
   #:MonadState
   #:get
   #:put
   #:modify))

(in-package #:coalton-library/monad/statet)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel

  ;;
  ;; StateT Definition
  ;;

  (repr :transparent)
  (define-type (StateT :s :m :a)
    "A monadic computation that tracks state of type :s."
    (StateT (:s -> :m (Tuple :s :a))))

  (inline)
  (declare put-stateT (Applicative :m => :s -> StateT :s :m Unit))
  (define (put-stateT state)
    "A stateful computation with state set to the given state. The returned value is Unit."
    (StateT (const (pure (Tuple state Unit)))))

  (inline)
  (declare get-stateT (Applicative :m => StateT :s :m :s))
  (define get-stateT
    "A stateful computation which returns the current state as the value."
    (StateT (fn (s)
              (pure (Tuple s s)))))

  (inline)
  (declare run-stateT (Applicative :m => StateT :s :m :a -> :s -> :m (Tuple :s :a)))
  (define (run-stateT (StateT fs->msa) s)
    (fs->msa s))

  (inline)
  (declare run-stateT_ (Applicative :m => StateT :s :m :a -> :s -> :m :a))
  (define (run-stateT_ st-op s)
    "Run ST-OP, discarding the state and returning the result."
    (map tp:snd (run-stateT st-op s)))

  (inline)
  (declare modify-stateT (Applicative :m => (:s -> :s) -> StateT :s :m Unit))
  (define (modify-stateT fs->s)
    "Modify the computation state, discarding the old state."
    (StateT
     (fn (s)
       (pure (Tuple (fs->s s) Unit)))))

  ;;
  ;; StateT Instances
  ;;

  (inline)
  (declare map-stateT ((:m (Tuple :s :a) -> :n (Tuple :s :b)) -> StateT :s :m :a -> StateT :s :n :b))
  (define (map-stateT fma->nb (StateT fs->msa))
    "Map the return value, the final state, and the execution context."
    (StateT (fn (s)
              (fma->nb (fs->msa s)))))

  (inline)
  (declare lift-stateT (Functor :m => :m :a -> StateT :s :m :a))
  (define (lift-stateT m)
    "Lift a stateless computation into a stateful context."
    (StateT (fn (s)
              (map (fn (a)
                     (Tuple s a))
                   m))))

  (define-instance (Functor :m => Functor (StateT :s :m))
    (inline)
    (define (map fa->b (StateT fs->msa))
      (StateT (fn (s)
                (map (fn ((Tuple s2 a))
                       (Tuple s2 (fa->b a)))
                     (fs->msa s))))))

  (define-instance (Applicative :m => Applicative (StateT :s :m))
    (inline)
    (define (pure a)
      (lift-stateT (pure a)))
    (inline)
    (define (liftA2 fa->b->c (StateT fs->msa) (StateT fs->msb))
      (StateT (fn (s)
                (liftA2 (fn ((Tuple _ a) (Tuple _ b))
                          (Tuple s (fa->b->c a b)))
                        (fs->msa s)
                        (fs->msb s))))))

  (define-instance (Monad :m => Monad (StateT :s :m))
    (inline)
    (define (>>= (StateT fs->msa) fa->st-msb)
      (StateT (fn (s)
                (>>= (fs->msa s)
                     (fn ((Tuple s2 a))
                       (match (fa->st-msb a)
                         ((StateT fs->msb)
                          (fs->msb s2)))))))))

  (define-instance (MonadTransformer (StateT :s))
    (inline)
    (define lift lift-stateT))

  (define-instance (Monad :m => MonadState :s (StateT :s :m))
    (define get get-stateT)
    (define put put-stateT)
    (define modify modify-stateT)))

;;;
;;; StateT Instances for other Transformers
;;;

(coalton-toplevel
  (define-instance (MonadEnvironment :e :m => (MonadEnvironment :e (StateT :s :m)))
    (define ask (lift ask))
    (define asks (compose lift asks))
    (define local (compose map-stateT local))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MONAD/STATET")
