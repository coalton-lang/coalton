(coalton-library/utils:defstdlib-package #:coalton-library/monad/environment
  (:use
   #:coalton
   #:coalton-library/functions
   #:coalton-library/classes
   #:coalton-library/monad/identity)
  (:import-from
   #:coalton-library/monad/state
   #:MonadState put get)
  (:export
   #:EnvT
   #:local-envT
   #:ask-envT
   #:asks-envT
   #:run-envT
   #:Env
   #:run-env
   #:MonadEnvironment
   #:local
   #:ask
   #:asks
   #:map-envT
   #:lift-envT))

(in-package #:coalton-library/monad/environment)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;;
  ;; Environment Monad
  ;;

  (repr :transparent)
  (define-type (EnvT :env :m :value)
    "A monadic computation that runs inside an :env environment.
Equivalent to Haskell's ReaderT monad https://hackage.haskell.org/package/transformers-0.6.1.2/docs/Control-Monad-Trans-Reader.html"
    (EnvT (:env -> :m :value)))

  (define-type-alias (Env :env :value) (EnvT :env Identity :value))

  (declare local-envT ((:env -> :env) -> EnvT :env :m :value -> EnvT :env :m :value))
  (define (local-envT fenv (EnvT fenv->a))
    "Run a computation in a modified environment."
    (EnvT (compose fenv->a fenv)))

  (declare ask-envT (Monad :m => EnvT :env :m :env))
  (define ask-envT
    "Retrieve the computation environment."
    (EnvT (compose pure id)))

  (declare asks-envT (Applicative :m => (:env -> :a) -> EnvT :env :m :a))
  (define (asks-envT fenv->a)
    "Retrieve an aspect of the computation environment."
    (EnvT (compose pure fenv->a)))

  (declare run-envT (EnvT :env :m :value -> :env -> :m :value))
  (define (run-envT (EnvT fenv->val) env)
    "Run a EnvT inside an environment."
    (fenv->val env))

  (declare run-env (Env :env :value -> :env -> :value))
  (define (run-env env-computation env)
    "Run a Env inside an environment."
    (run-identity (run-envT env-computation env)))

  ;;
  ;; MonadEnvironment Typeclass
  ;;

  (define-class (Monad :m => MonadEnvironment :env :m (:m -> :env))
    "A monad capable of a function in a computation environment."
    (ask
     "Retrieves the computation environment."
     (:m :env))
    (local
     "Run a computation in a modified environment."
     ((:env -> :env) -> :m :env -> :m :env))
    (asks
     "Retrieve an aspect of the computation environment."
     ((:env -> :a) -> :m :a))))

;;
;; Environment Monad Instances
;;

(coalton-toplevel
  (declare map-envT ((:m :a -> :n :b) -> EnvT :env :m :a -> EnvT :env :n :b))
  (define (map-envT fma->nb (EnvT fenv->ma))
    (EnvT (compose fma->nb fenv->ma)))

  (declare lift-envT (:m :a -> EnvT :env :m :a))
  (define lift-envT (compose EnvT const)))

(coalton-toplevel
  (define-instance (Functor :m => Functor (EnvT :env :m))
    (define map (compose map-envT map)))

  (define-instance (Applicative :m => Applicative (EnvT :env :m))
    (define pure (compose lift-envT pure))
    (define (liftA2 fc->d->e (EnvT fenv->mc) (EnvT fenv->md))
      (EnvT (fn (env)
                (liftA2 fc->d->e (fenv->mc env) (fenv->md env))))))

  (define-instance (Monad :m => Monad (EnvT :env :m))
    (define (>>= (EnvT fenv->ma) fa->envmb)
      (EnvT
        (fn (env)
          (>>= (fenv->ma env)
               (fn (a)
                 (match (fa->envmb a)
                   ((EnvT fenv->mb)
                    (fenv->mb env)))))))))

  (define-instance (MonadTransformer (EnvT :env))
    (define lift lift-envT)))

(coalton-toplevel
  (define-instance (MonadState :s :m => MonadState :s (EnvT :env :m))
    (define put (compose lift put))
    (define get (lift get))))

(coalton-toplevel
  (define-instance (Monad :m => MonadEnvironment :env (EnvT :env :m))
    (define ask ask-envT)
    (define local local-envT)
    (define asks asks-envT)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MONAD/ENVIRONMENT")
