(coalton-library/utils:defstdlib-package #:coalton-library/functions
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes)
  (:export
   #:trace
   #:traceObject
   #:print
   #:unsafe-pointer-eq?
   #:fix
   #:id
   #:const
   #:flip
   #:reduce
   #:compose
   #:conjoin
   #:disjoin
   #:conjoin*
   #:disjoin*
   #:complement
   #:curry
   #:uncurry
   #:pair-with
   #:msum
   #:asum
   #:/=
   #:bracket))

(in-package #:coalton-library/functions)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (declare trace (String -> Unit))
  (define (trace str)
    "Deprecated: Use `show`.

Print a line to `cl:*standard-output*`."
    (progn
      (lisp :a (str) (cl:format cl:t "~A~%" str))
      Unit))

  (declare traceObject (String -> :a -> Unit))
  (define (traceObject str item)
    "Deprecated: Use `show` and `Reveal`.

Print a line to `cl:*standard-output*` in the form \"{STR}: {ITEM}\"."
    (progn
      (lisp :a (str item) (cl:format cl:t "~A: ~A~%" str item))
      Unit))

  (declare print ((Into :a String) => :a -> Unit))
  (define (print item)
    "Print the String representation of `item` to `cl:*standard-output*`."
    (trace (into item)))

  (inline)
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

  (inline)
  (declare id (:a -> :a))
  (define (id x)
    "A function that always returns its argument."
    x)

  (inline)
  (declare const (:a -> :b -> :a))
  (define (const a _b)
    "A function that always returns its first argument."
    a)

  (inline)
  (declare flip ((:a -> :b -> :c) -> :b -> :a -> :c))
  (define (flip f x y)
    "Returns a function that takes its arguments in reverse order."
    (f y x))

  (declare reduce (Foldable :f => (:a -> :b -> :b) -> :b -> (:f :a) -> :b))
  (define (reduce f y xs)
    "The same as `fold` but with the argument order swapped to match `cl:reduce`"
    (fold (flip f) y xs))

  (inline)
  (declare compose ((:b -> :c) -> (:a -> :b) -> :a -> :c))
  (define (compose f g x)
    "Equivalent to `(f (g x))`."
    (f (g x))))

(coalton-toplevel
 
 (declare conjoin ((:a -> Boolean) -> (:a -> Boolean) -> :a -> Boolean))
 (define (conjoin f g x)
   "Compute the conjunction of two unary Boolean functions."
   (and (f x) (g x)))

 (declare disjoin ((:a -> Boolean) -> (:a -> Boolean) -> :a -> Boolean))
 (define (disjoin f g x)
   "Compute the disjunction of two unary Boolean functions."
   (or (f x) (g x))))

(defmacro conjoin* (cl:&rest predicates)
  "Compute the conjuction of `predicates`.

For example, the following expressions are equivalent.

`(conjoin* f g h)`

`(fn (x) (and (f x) (g x) (h x)))`"
  (cl:let ((x (cl:gensym "X-")))
    `(fn (,x) (and ,@(cl:loop :for predicate :in predicates
                              :collect `(,predicate ,x))))))

(defmacro disjoin* (cl:&rest predicates)
  "Compute the disjunction of `predicates`.

For example, the following expressions are equivalent.

`(disjoin* f g h)`

`(fn (x) (or (f x) (g x) (h x)))`"
  (cl:let ((x (cl:gensym "X-")))
    `(fn (,x) (or ,@(cl:loop :for predicate :in predicates
                             :collect `(,predicate ,x))))))

(coalton-toplevel

 (inline)
 (declare complement ((:a -> Boolean) -> :a -> Boolean))
 (define (complement f x)
   "Compute the complement of a unary Boolean function."
   (not (f x)))

 (declare curry ((Tuple :left :right -> :result) -> :left -> :right -> :result))
 (define (curry func left right)
   "Take a function whose input is a tuple and enable curried application of the left and right parameters, equivalent to `(func (Tuple left right))`."
   (func (Tuple left right)))

 (declare uncurry ((:left -> :right -> :result) -> Tuple :left :right -> :result))
 (define (uncurry func tpl)
   "Take a function with two currying parameters and enable their input as a single `Tuple`."
   (match tpl
          ((Tuple left right)
           (func left right))))

 (declare pair-with ((:left -> :right) -> :left -> Tuple :left :right))
 (define (pair-with func left)
   "Create a `Tuple` of the form `(Tuple left (func left))`."
   (Tuple left (func left)))

   ;;
   ;; Monadic operators
   ;;



  (declare msum ((Monoid :a) (Foldable :t) => :t :a -> :a))
  (define (msum xs)
    "Fold over a list using `<>`."
    (foldr <> mempty xs))

  (declare asum ((Alternative :f) (Foldable :t) => :t (:f :a) -> :f :a))
  (define (asum xs)
    "Fold over a list using `alt`."
    (foldr alt empty xs))

  (inline)
  (declare /= (Eq :a => :a -> :a -> Boolean))
  (define (/= a b)
    "Is `a` not equal to `b`?"
    (boolean-not (== a b)))

   ;;
   ;; Instances
   ;;



  (define-instance (Functor (Arrow :a))
    (define map compose))

  (define-instance (Applicative (Arrow :a))
    (define (pure x) (fn (_) x))
    (define (liftA2 f g h) (fn (x) (f (g x) (h x)))))

  (define-instance (Monad (Arrow :a))
    (define (>>= f g) (fn (x) (g (f x) x)))))

;;;
;;; Bracket pattern
;;;

(cl:defmacro %unwind-protect (obj exit thunk)
  "A wrapper on `cl:unwind-protect.`"
  (cl:let ((output (cl:gensym "OUTPUT")))
    `(cl:let (,output)
       (cl:unwind-protect (cl:setq ,output (call-coalton-function ,thunk ,obj))
          (call-coalton-function ,exit ,obj))
       ,output)))

(coalton-toplevel

  (declare bracket (Monad :m
                          => :m :a
                          -> (:a -> :m :b)
                          -> (:a -> :m :c)
                          -> :m :c))
  (define (bracket init exit body)
    "Bracket takes an initial state, performs a body of operations, and then forces a safe exit.

This wraps `cl:unwind-protect`.

Modeled after Haskell: https://wiki.haskell.org/Bracket_pattern"
    (do
     (obj <- init)
     (lisp (:m :c) (obj exit body)
       (%unwind-protect obj exit body)))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/FUNCTIONS")
