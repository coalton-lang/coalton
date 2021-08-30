(in-package #:coalton-user)

(coalton-toplevel

  (declare error (String -> :a))
  (define (error str)
    (lisp :a (cl:error str)))
  
  ;;
  ;; Function combinators
  ;;

  (declare fix (((:a -> :b) -> (:a -> :b)) -> (:a -> :b)))
  (define (fix f n)
    (f (fix f) n))

  (declare id (:a -> :a))
  (define (id x) x)

  (declare const (:a -> :b -> :a))
  (define (const a b)
    a)

  (declare flip ((:a -> :b -> :c) -> :b -> :a -> :c))
  (define (flip f x y)
    (f y x))

  ;;
  ;; Monadic operators
  ;;

  (declare traverse (Applicative :m => ((:a -> (:m :b)) -> (List :a) -> (:m (List :b)))))
  (define (traverse f xs)
    (let ((cons-f (fn (x ys)
                    (liftA2 Cons (f x) ys))))
      (fold cons-f (pure Nil) xs)))

  (declare mapM (Monad :m => ((:a -> (:m :b)) -> (List :a) -> (:m (List :b)))))
  (define (mapM f xs)
    (traverse f xs))

  (declare liftM (Monad :m => ((:a1 -> :r) -> (:m :a1) -> (:m :r))))
  (define (liftM f m1)
    (>>= m1
         (fn (x1)
           (pure (f x1)))))

  (declare liftM2 (Monad :m => ((:a1 -> :a2 -> :r) -> (:m :a1) -> (:m :a2) -> (:m :r))))
  (define (liftM2 f m1 m2)
    (>>= m1
         (fn (x1)
           (>>= m2
                (fn (x2)
                  (pure (f x1 x2)))))))

  (declare sequence (Applicative :f => ((List (:f :a)) -> (:f (List :a)))))
  (define (sequence xs)
    (traverse id xs))

  (declare mconcat (Monoid :a => ((List :a) -> :a)))
  (define (mconcat xs)
    (fold <> mempty xs))

  (declare asum (Alternative :f => ((List (:f :a)) -> (:f :a))))
  (define (asum xs) (foldr alt empty xs)))
