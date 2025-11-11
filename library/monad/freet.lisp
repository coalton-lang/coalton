(coalton-library/utils::defstdlib-package #:coalton-library/monad/freet
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/functions)
  (:import-from
   #:coalton-library/monad/free
   #:MonadFree
   #:wrap)
  (:export
   #:FreeF
   #:Val
   #:FreeT
   #:unwrap-freeT
   #:run-freeT
   #:fold-freeT))

(in-package #:coalton-library/monad/freet)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;
;; Port of the free monad transformer (not the church-encoded monad transformer)
;; from the Haskell `free` library.
;;
;; See:
;; https://hackage.haskell.org/package/free
;; https://www.haskellforall.com/2012/07/free-monad-transformers.html
;;

(coalton-toplevel
  ;;
  ;; FreeF, the free functor
  ;;

  (define-type (FreeF :f :a :b)
    (FreeF (:f :b))
    (Val :a))

  (define-instance (Functor :f => Functor (FreeF :f :a))
    ;; (b -> c) -> FreeF f a b -> FreeF f a c
    (define (map b->c f)
      (match f
        ((Val a) (Val a))
        ((FreeF funct) (FreeF (map b->c funct))))))

  (define-instance (Traversable :f => Traversable (FreeF :f :a))
    (define (traverse b->tc free-f)
      (match free-f
        ((Val a) (pure (Val a)))
        ((FreeF funct) (map FreeF (traverse b->tc funct))))))

  (define-instance (Functor :f => Bifunctor (FreeF :f))
    (define (bimap a->c b->d free-f)
      (match free-f
        ((Val a) (Val (a->c a)))
        ((FreeF fb) (FreeF (map b->d fb))))))

  ;; Note: Constitutes an instance of Bitraverse, which Coalton doesn't have (yet)
  (declare bitraverse ((Traversable :f) (Applicative :g) =>
                       (:a -> :g :c) -> (:b -> :g :d) -> FreeF :f :a :b -> :g (FreeF :f :c :d)))
  (define (bitraverse a->fc b->fd free-f)
    (match free-f
      ((Val a) (map Val (a->fc a)))
      ((FreeF funct-b) (map FreeF (traverse b->fd funct-b))))))

(coalton-toplevel
  ;;
  ;; FreeT Type
  ;;

  (define-type (FreeT :f :m :a)
    "`Free :f :m :a` gives you a Monad Transformer instance for any `Functor :f` and `Monad :m`."
    (FreeT (:m (FreeF :f :a (FreeT :f :m :a)))))

  (inline)
  (declare unwrap-freeT (FreeT :f :m :a -> :m (FreeF :f :a (FreeT :f :m :a))))
  (define (unwrap-freeT (FreeT m))
    "Unwrap one layer of the the free monad transformer, returning a value of the base
monad containing a FreeF (which can either contain VAL, a pure value, or FREEF, another
wrapped layer of the free monad transformer)."
    m)

  (declare run-freeT (Monad :m
                      => (:f (FreeT :f :m :a) -> FreeT :f :m :a)
                      -> FreeT :f :m :a
                      -> :m :a))
  (define (run-freeT transf op)
    "Run a free monad transformer with a function that unwraps a single layer of the
functor `f` at a time."
    (do
     (step <- (unwrap-freeT op))
     (match step
       ((Val a)
        (pure a))
       ((FreeF next)
        (run-freeT transf (transf next))))))

  (define-instance ((Functor :f) (Monad :m) => MonadFree :f (FreeT :f :m))
    (define (wrap fm)
      (FreeT (pure (FreeF fm)))))

  ;;
  ;; Core Functor/Applicative/Monad Instances
  ;;

  (define-instance ((Functor :f) (Functor :m) => Functor (FreeT :f :m))
    (define (map a->b (FreeT m))
      (FreeT (map (fn (x)
                    (match x
                      ((Val a) (Val (a->b a)))
                      ((FreeF funct) (FreeF (map (map a->b) funct)))))
                  m))))

  (declare pure-freeT (Monad :m => :a -> FreeT :f :m :a))
  (define (pure-freeT a)
    (FreeT (pure (Val a))))

  (declare bind ((Monad :m) (Functor :f) => FreeT :f :m :a -> (:a -> FreeT :f :m :b) -> FreeT :f :m :b))
  (define (bind (FreeT ma) a->freet-mb)
    (FreeT
     (>>= ma (fn (v)
               (match v
                 ((Val a)
                  (unwrap-freeT (a->freet-mb a)))
                 ((FreeF funct-a)
                  (pure (FreeF (map ((flip bind) a->freet-mb) funct-a)))))))))

  (declare apply-freeT ((Functor :f) (Monad :m) => FreeT :f :m (:a -> :b) -> FreeT :f :m :a -> FreeT :f :m :b))
  (define (apply-freeT m n)
    (bind
     m
     (fn (f)
       (bind
        n
        (fn (x)
          (pure-freeT (f x)))))))

  (declare lifta2-freeT ((Functor :f) (Monad :m) => (:a -> :b -> :c) -> FreeT :f :m :a
                                                    -> FreeT :f :m :b -> FreeT :f :m :c))
  (define (lifta2-freeT a->b->c fa fb)
    (apply-freeT (map a->b->c fa) fb))

  (define-instance ((Functor :f) (Monad :m) => Applicative (FreeT :f :m))
    (define pure pure-freeT)
    (define lifta2 lifta2-freeT))

  (define-instance ((Functor :f) (Monad :m) => Monad (FreeT :f :m))
    (define >>= bind))

  (define-instance (Functor :f => MonadTransformer (FreeT :f))
    (define (lift m)
      (FreeT (map Val m))))

  ;;
  ;; "Other" Instances
  ;;

  (define-instance ((Monad :m) (Traversable :m) (Traversable :f) => Traversable (FreeT :f :m))
    (define (traverse a->gb (FreeT ma))
      (map FreeT (traverse (bitraverse a->gb (traverse a->gb)) ma))))

  ;;
  ;; FreeT functions
  ;;

  (declare fold-freeT ((MonadTransformer :t) (Monad (:t :m)) (Monad :m) =>
                       (:f (FreeT :f :m :a) -> :t :m (FreeT :f :m :a)) -> FreeT :f :m :a -> :t :m :a))
  (define (fold-freeT f (FreeT m))
    (>>=
     (lift m)
     (fn (free-t)
       (match free-t
         ((Val a) (pure a))
         ((FreeF funct-a)
          (>>= (f funct-a)
               (fold-freeT f))))))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MONAD/FREET")
