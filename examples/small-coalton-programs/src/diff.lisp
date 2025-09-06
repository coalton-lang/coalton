;;;; Example from the README.

(defpackage #:differentiation
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:sym #:coalton-library/symbol))
  (:export #:Expr #:EConst #:EVar #:E+ #:E*)
  (:export #:diff #:t #:d/dt))

(in-package #:differentiation)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;; Define a new parametric algebraic data type for simple
  ;; mathematical expressions.
  (define-type (Expr :t)
    "A symbolic expression of basic arithmetic."
    (EConst :t)
    (EVar   sym:Symbol)
    (E+     (Expr :t) (Expr :t))
    (E*     (Expr :t) (Expr :t)))

  ;; The classic `diff` function, in Coalton.
  (declare diff (Num :t => sym:Symbol -> Expr :t -> Expr :t))
  (define (diff x f)
    "Compute the derivative of `f` with respect to `x`."
    (match f
      ((EConst _)                       ; c' = 0
       (EConst 0))
      ((EVar s)                         ; x' = 1
       (if (== s x) (EConst 1) (EConst 0)))
      ((E+ a b)                         ; (a+b)' = a' + b'
       (E+ (diff x a) (diff x b)))
      ((E* a b)                         ; (ab)' = a'b + ab'
       (E+ (E* (diff x a) b)
           (E* a          (diff x b))))))

  ;; We can use `t` just fine since Coalton doesn't import `cl:t`.
  (define t (sym:make-symbol "t"))

  (declare d/dt (Num :t => Expr :t -> Expr :t))
  (define d/dt
    "The time derivative operator."
    (diff t))

  (define (square x)
    (E* x x)))
