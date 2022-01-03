(in-package #:coalton-library)

(coalton-toplevel
  (define (undefined x)
    "A function which can be used in place of any value, throwing an error at runtime."
    (lisp :a () (cl:error "Undefined")))

  (declare error (String -> :a))
  (define (error str)
    "Signal an error by calling `CL:ERROR`."
    (lisp :a (str) (cl:error str)))

  ;;
  ;; Function combinators
  ;;

  (declare fix (((:a -> :b) -> (:a -> :b)) -> (:a -> :b)))
  (define (fix f n)
    "Compute the fixed point of a unary function. This is equivalent to the Y-combinator of the lambda calculus. 


    This combinator allows recursion without specific assignment of names. For example, the factorial function can be written


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
    "A function that always returns its argument."
    x)

  (declare const (:a -> :b -> :a))
  (define (const a b)
    "A function that always returns its first argument."
    a)

  (declare flip ((:a -> :b -> :c) -> :b -> :a -> :c))
  (define (flip f x y)
    "Returns a function that takes its arguments in reverse order."
    (f y x)))
