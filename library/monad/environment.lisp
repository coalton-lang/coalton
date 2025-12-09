(coalton-library/utils:defstdlib-package #:coalton-library/monad/environment
  (:use
   #:coalton
   #:coalton-library/functions
   #:coalton-library/classes
   #:coalton-library/monad/classes
   #:coalton-library/monad/identity
   #:coalton-compatibility)
  (:local-nicknames
   (#:compat #:coalton-compatibility))
  (:export
   #:EnvT
   #:local-envT
   #:ask-envT
   #:asks-envT
   #:run-envT
   #:map-envT
   #:lift-envT

   #:Env
   #:run-env
   #:ask-env
   #:asks-env
   #:local-env

   ;; Re-export MonadEnvironment
   #:MonadEnvironment
   #:ask
   #:local
   #:asks))

(in-package #:coalton-library/monad/environment)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;;
  ;; Environment Monad
  ;;

  ;; NOTE: In Haskell, the base Reader monad is just defined as
  ;; (ReaderT :env Identity). GHC does a lot of work behind the
  ;; scenes to fuse lambdas together at compile time to prevent
  ;; needless thunk allocation, so they can get away with this.
  ;; In Coalton, which lacks these optimizations, a concrete
  ;; base Env implementation will be more efficient than just
  ;; using (EnvT :env Identity).
  (repr :transparent)
  (define-type (Env :env :value)
    "A computation that runs inside an :env environment."
    (Env (:env -> :value)))

  (inline)
  (declare run-env (Env :env :value -> :env -> :value))
  (define (run-env (Env env-computation) env)
    "Run a Env inside an environment."
    (env-computation env))

  (inline)
  (declare local-env ((:env -> :env) -> Env :env :value -> Env :env :value))
  (define (local-env fenv menv)
    (Env (fn (env)
           (run-env menv (fenv env)))))

  (inline)
  (declare ask-env (Env :env :env))
  (define ask-env
    "Retrieve the computation environment."
    (Env id))

  (inline)
  (declare asks-env ((:env -> :a) -> Env :env :a))
  (define (asks-env fenv->a)
    "Retrieve an aspect of the computation environment."
    (Env (fn (env)
           (fenv->a env))))

  ;;
  ;; Enviornment Monad Instances
  ;;

  (define-instance (Functor (Env :env))
    (inline)
    (define (map fa->b menv)
      (Env (compose fa->b (run-env menv)))))

  (define-instance (Applicative (Env :env))
    (inline)
    (define (pure a)
      (Env (const a)))
    (inline)
    (define (liftA2 fa->b->c menv-a menv-b)
      (Env (fn (env)
             (fa->b->c (run-env menv-a env) (run-env menv-b env))))))

  (define-instance (Monad (Env :env))
    (inline)
    (define (>>= menv-a fa->env-b)
      (Env (fn (env)
             (run-env
              (fa->env-b (run-env menv-a env))
              env)))))

  (define-instance (MonadEnvironment :env (Env :env))
    (define ask ask-env)
    (define local local-env)
    (define asks asks-env)))


;;
;; EnvironmentT Monad
;;

(coalton-toplevel

  (repr :transparent)
  (define-type (EnvT :env :m :value)
    "A monadic computation that runs inside an :env environment.
Equivalent to Haskell's ReaderT monad https://hackage.haskell.org/package/transformers-0.6.1.2/docs/Control-Monad-Trans-Reader.html"
    (EnvT (:env -> :m :value)))

  (inline)
  (declare local-envT ((:env -> :env) -> EnvT :env :m :value -> EnvT :env :m :value))
  (define (local-envT fenv (EnvT fenv->a))
    "Run a computation in a modified environment."
    (EnvT (fn (env)
            (fenv->a (fenv env)))))

  (inline)
  (declare ask-envT (Monad :m => EnvT :env :m :env))
  (define ask-envT
    "Retrieve the computation environment."
    (EnvT pure))

  (inline)
  (declare asks-envT (Applicative :m => (:env -> :a) -> EnvT :env :m :a))
  (define (asks-envT fenv->a)
    "Retrieve an aspect of the computation environment."
    (EnvT (fn (env)
            (pure (fenv->a env)))))

  (inline)
  (declare run-envT (EnvT :env :m :value -> :env -> :m :value))
  (define (run-envT (EnvT fenv->val) env)
    "Run a EnvT inside an environment."
    (fenv->val env)))

;;
;; Environment Monad Instances
;;

(coalton-toplevel
  (inline)
  (declare map-envT ((:m :a -> :n :b) -> EnvT :env :m :a -> EnvT :env :n :b))
  (define (map-envT fma->nb (EnvT fenv->ma))
    (EnvT (fn (env)
            (fma->nb (fenv->ma env)))))

  (inline)
  (declare lift-envT (:m :a -> EnvT :env :m :a))
  (define (lift-envT m)
    (EnvT (const m))))

(coalton-toplevel
  (define-instance (Functor :m => Functor (EnvT :env :m))
    (inline)
    (define (map fa->b env-a)
      (map-envT (map fa->b) env-a)))

  (define-instance (Applicative :m => Applicative (EnvT :env :m))
    (inline)
    (define (pure a)
      (lift-envT (pure a)))
    (inline)
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
    (inline)
    (define lift lift-envT)))

(coalton-toplevel
  (define-instance (Monad :m => MonadEnvironment :env (EnvT :env :m))
    (define ask ask-envT)
    (define local local-envT)
    (define asks asks-envT)))

;;;
;;; EnvT Instances for other Transformers
;;;

(coalton-toplevel
  (define-instance (MonadState :s :m => (MonadState :s (EnvT :e :m)))
    (define get (lift get))
    (define put (compose lift put))
    (define modify (compose lift modify))))

(compat:try-lock-package "COALTON-LIBRARY/MONAD/ENVIRONMENT")
