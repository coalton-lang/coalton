(coalton-library/utils:defstdlib-package #:coalton-library/functions
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes)
  (:export
   #:trace
   #:traceObject
   #:unsafe-pointer-eq?
   #:fix
   #:id
   #:const
   #:flip
   #:reduce
   #:compose
   #:conjoin
   #:disjoin
   #:complement
   #:uncurry
   #:msum
   #:asum
   #:/=))

(in-package #:coalton-library/functions)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (declare trace (String -> Unit))
  (define (trace str)
    "Print a line to `*STANDARD-OUTPUT*`"
    (progn
      (lisp :a (str) (cl:format cl:t"~A~%" str))
      Unit))

  (declare traceObject (String -> :a -> Unit))
  (define (traceObject str item)
    "Print a line to `*STANDARD-OUTPUT*` in the form \"{STR}: {ITEM}\""
    (progn
      (lisp :a (str item) (cl:format cl:t "~A: ~A~%" str item))
      Unit))

  (declare unsafe-pointer-eq? (:a -> :a -> Boolean))
  (define (unsafe-pointer-eq? a b)
    (lisp Boolean (a b)
      (to-boolean (cl:eq a b))))

  ;;
  ;; Function combinators
  ;;

  (declare fix (((:a -> :b) -> (:a -> :b)) -> (:a -> :b)))
  (define (fix f n)
    "Compute the fixed point of a unary function. This is equivalent to the Y-combinator of the lambda calculus. This combinator allows recursion without specific assignment of names. For example, the factorial function can be written

    (define fact
      (fix
        (fn (f n)
          (if (== n 0)
            1
            (* n (f (- n 1)))))))"
    (f (fix f) n))

  (declare id (:a -> :a))
  (define (id x)
    "A function that always returns its argument."
    x)

  (declare const (:a -> :b -> :a))
  (define (const a b)
    "A function that always returns its first argument."
    a)

  (declare flip ((:a -> :b -> :c) -> :b -> :a -> :c))
  (define (flip f x y)
    "Returns a function that takes its arguments in reverse order."
    (f y x))

  (declare reduce (Foldable :f => (:a -> :b -> :b) -> :b -> (:f :a) -> :b))
  (define (reduce f y xs)
    "The same as `fold` but with the argument order swapped to match `cl:reduce`"
    (fold (flip f) y xs))


  ;; We don't write (COMPOSE F G X) even though it's OK so that the
  ;; most common case of using compose---as a binary function---is
  ;; considered to be "saturated".
  (declare compose ((:b -> :c) -> (:a -> :b) -> (:a -> :c)))
  (define (compose f g)
    "Produces a function equivalent to applying G then F in succession."
    ;; Note: ((compose f g) x) behaves like (f (g x))
    (fn (x)
      (f (g x))))

  (declare conjoin ((:a -> Boolean) -> (:a -> Boolean) -> :a -> Boolean))
  (define (conjoin f g x)
    "Compute the conjunction of two unary Boolean functions."
    (and (f x) (g x)))

  (declare disjoin ((:a -> Boolean) -> (:a -> Boolean) -> :a -> Boolean))
  (define (disjoin f g x)
    "Compute the disjunction of two unary Boolean functions."
    (or (f x) (g x)))

  (declare complement ((:a -> Boolean) -> :a -> Boolean))
  (define (complement f x)
    "Compute the complement of a unary Boolean function."
    (not (f x)))

  (declare uncurry ((:left -> :right -> :result) -> Tuple :left :right -> :result))
  (define (uncurry func tpl)
    (match tpl
      ((Tuple left right)
       (func left right))))

  ;;
  ;; Monadic operators
  ;;

  (declare msum ((Monoid :a) (Foldable :t) => :t :a -> :a))
  (define (msum xs)
    "Fold over a list using <>"
    (foldr <> mempty xs))

  (declare asum ((Alternative :f) (Foldable :t) => :t (:f :a) -> :f :a))
  (define (asum xs)
    "Fold over a list using alt"
    (foldr alt empty xs))

  (declare /= (Eq :a => :a -> :a -> Boolean))
  (define (/= a b)
    "Is A not equal to B?"
    (boolean-not (== a b))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/FUNCTIONS")
