(coalton-library/utils::defstdlib-package #:coalton-library/monad/freet
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/functions))

(in-package #:coalton-library/monad/freet)

(named-readtables:in-readtable coalton:coalton)

;;
;; Port of the monad transformer (not the church-encoded monad transformer)
;; from the Haskell `free` library.
;;
;; See:
;; https://hackage.haskell.org/package/free
;; https://www.haskellforall.com/2012/07/free-monad-transformers.html
;;

;; Note: The best design here would be to wrap FreeT and Free in an mtl-style typeclass.
;; When I tried this with the ST monad, it broke some type inference in a way that makes
;; me nervous to do this here. Instead, I'll give it the same API as Free, so it should be
;; easy to swap out the concrete API with a typeclass one at a future date.

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
        ((FreeF funct) (FreeF (map b->c funct)))))))

(coalton-toplevel

  ;;
  ;; FreeT Type
  ;;

  (define-type (FreeT :f :m :a)
    "`Free :f :m :a` gives you a Monad Transformer instance for any `Functor :f` and `Monad :m`."
    (FreeT (:m (FreeF :f :a (FreeT :f :m :a)))))

  (declare run-freeT (FreeT :f :m :a -> :m (FreeF :f :a (FreeT :f :m :a))))
  (define (run-freeT (FreeT m)) m))

(coalton-toplevel
  ;;
  ;; Instances
  ;;

  (define-instance ((Functor :f) (Functor :m) => Functor (FreeT :f :m))
    (define (map a->b (FreeT m))
      (FreeT (map (fn (x)
                    (match x
                      ((Val a) (Val (a->b a)))
                      ((FreeF funct) (FreeF (map (map a->b) funct)))))
                  m))))

  (declare rtrn (Monad :m => :a -> FreeT :f :m :a))
  (define (rtrn a)
    (FreeT (pure (Val a))))

  (declare bind ((Monad :m) (Functor :f) => FreeT :f :m :a -> (:a -> FreeT :f :m :b) -> FreeT :f :m :b))
  (define (bind (FreeT ma) a->freet-mb)
    (FreeT
     (>>= ma (fn (v)
               (match v
                 ((Val a)
                  (run-freeT (a->freet-mb a)))
                 ((FreeF funct-a)
                  (pure (FreeF (map ((flip bind) a->freet-mb) funct-a)))))))))

  (declare freet-apply ((Functor :f) (Monad :m) => FreeT :f :m (:a -> :b) -> FreeT :f :m :a -> FreeT :f :m :b))
  (define (freet-apply m n)
    (bind
     m
     (fn (f)
       (bind
        n
        (fn (x)
          (rtrn (f x)))))))

  (declare freet-lifta2 ((Functor :f) (Monad :m) => (:a -> :b -> :c) -> FreeT :f :m :a
                                                    -> FreeT :f :m :b -> FreeT :f :m :c))
  (define (freet-lifta2 a->b->c fa fb)
    (freet-apply (map a->b->c fa) fb))

  (define-instance ((Functor :f) (Monad :m) => Applicative (FreeT :f :m))
    (define pure rtrn)
    (define lifta2 freet-lifta2))

  (define-instance ((Functor :f) (Monad :m) => Monad (FreeT :f :m))
    (define >>= bind)))
