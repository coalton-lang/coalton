(in-package #:coalton-library)

(coalton-toplevel


  (declare trace (String -> Unit))
  (define (trace str)
    "Print a line to *STANDARD-OUTPUT*"
    (progn
      (lisp :a (str) (cl:format cl:t"~A~%" str))
      Unit))

  (declare traceObject (String -> :a -> Unit))
  (define (traceObject str item)
    "Print a line to *STANDARD-OUTPUT* in the form \"{STR}: {ITEM}\""
    (progn
      (lisp :a (str item) (cl:format cl:t "~A: ~A~%" str item))
      Unit))

  ;;
  ;; Function combinators
  ;;

  (declare fix (((:a -> :b) -> (:a -> :b)) -> (:a -> :b)))
  (define (fix f n)
    "The factorial function can be written
    ```
    (define fact
      (fix
        (fn (f n)
          (if (== n 0)
            1
            (* n (f (- n 1)))))))
    ```"
    (f (fix f) n))

  (declare id (:a -> :a))
  (define (id x)
    "A function that always returns its argument"
    x)

  (declare const (:a -> :b -> :a))
  (define (const a b)
    "A function that always returns its first argument"
    a)

  (declare flip ((:a -> :b -> :c) -> :b -> :a -> :c))
  (define (flip f x y)
    "FLIP reverses the arguments to F"
    (f y x))

  ;; We don't write (COMPOSE F G X) even though it's OK so that the
  ;; most common case of using compose---as a binary function---is
  ;; considered to be "saturated".
  (declare compose ((:b -> :c) -> (:a -> :b) -> (:a -> :c)))
  (define (compose f g)
    (fn (x)
      (f (g x))))

  (define (conjoin f g x)
    "Compute the conjunction of two unary Boolean functions."
    (and (f x) (g x)))

  (define (disjoin f g x)
    "Compute the disjunction of two unary Boolean functions."
    (or (f x) (g x)))

  (define (complement f x)
    "Compute the complement of a unary Boolean function."
    (not (f x)))


  ;;
  ;; Monadic operators
  ;;

  (declare traverse (Applicative :m => ((:a -> (:m :b)) -> (List :a) -> (:m (List :b)))))
  (define (traverse f xs)
    "Map the elements of XS with F then collect the results."
    (let ((cons-f (fn (x ys)
                    (liftA2 Cons (f x) ys))))
      (fold cons-f (pure Nil) xs)))

  (declare sequence (Applicative :f => ((List (:f :a)) -> (:f (List :a)))))
  (define (sequence xs)
    (traverse id xs))

  (declare msum (Monoid :a => ((List :a) -> :a)))
  (define (msum xs)
    "Fold over a list using <>"
    (foldr <> mempty xs))

  (declare asum (Alternative :f => ((List (:f :a)) -> (:f :a))))
  (define (asum xs)
    "Fold over a list using alt"
    (fold alt empty xs)))
