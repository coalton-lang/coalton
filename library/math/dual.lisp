;;;; dual.lisp
;;;;
;;;; An implementation of Dual numbers for the computing derivatives
;;;; of compositions of built-in Coalton functions.

(coalton-library/utils:defstdlib-package #:coalton-library/math/dual
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/functions
   #:coalton-library/math/arith
   #:coalton-library/math/elementary
   #:coalton-library/math/integral
   #:coalton-library/hash)
  (:export
   #:Dual
   #:primal-part
   #:dual-part)
  (:documentation "
Dual numbers are a hypercomplex number system [1]. A dual number has
the form $a + b\\varepsilon$ where $a$ and $b$ are real numbers and
$\\varepsilon$ is a symbol that satisfies $\\varepsilon^2=0$ and
$\\varepsilon\\neq 0$. The value $a$ is often called the *primal part*
and the value $b$ is often called the *dual part*. One application of
dual numbers is automatic differentiation; an example taken from [2]
is as follows.

Consider the function $f(x) = 3x+2$ and you want to calculate $f(4)$
and $f^{\\prime}(4)$. By the usual rules of differentiation, we know
$f^{\\prime}(x) = 3$ and thus $(f(4), f^{\\prime}(4)) = (14, 3)$. We
seek to recover this with dual numbers.

With dual numbers, we can calculate

$$f(a) + f^{\\prime}(a)\\varepsilon$$

by taking a real-valued function $f$ and evaluating as if it were a
dual-valued function at the point $a + \\varepsilon$. Thus, for the
defined $f$, we have:

$$
\\begin{align*}
f(4 + \\varepsilon)
&= 3(4 + \\varepsilon) + 2 \\\\\\\\
&= 3\\cdot 4 + 3\\varepsilon + 2 \\\\\\\\
&= 14 + 3\\varepsilon.
\\end{align*}
$$

In this result, the primal $14$ is the value of $f(4)$ and the dual is
the value of of $f^{\\prime}(4)$.

Haskell has an automatic differentiation library and you can find it
here [3].

Limitations:

We have decided to implement Ord, Eq, and Hash to look at only the
primal part of numbers. This is so the Dual type can be used primarily
for the purpose of automatic differentiation of existing code, and not
for general abstract mathematics. If you need these type classes
acting in the usual way (i.e., on both primal and dual parts), then we
recommend making your own data type which wraps a dual number.

References:

- [1] https://en.wikipedia.org/wiki/Dual_number
- [2] https://blog.demofox.org/2014/12/30/dual-numbers-automatic-differentiation/
- [3] https://hackage.haskell.org/package/ad"))

(in-package #:coalton-library/math/dual)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel

  (define-struct (Dual :t)
    "Representation of a dual number in the form $a + b\\varepsilon$ where
$a$ and $b$ are real numbers and $\\varepsilon$ satisfies
$\\varepsilon^2 = 0$ and $\\varepsilon \\neq 0$.

Note: `Eq`, and `Ord` and `Hash` only make use of the primal
component."
    (primal-part "The primal part." :t)
    (dual-part "The dual part." :t))

  (declare primal-part (Dual :t -> :t))
  (define (primal-part (Dual p _))
    "The primal (i.e., real) part of a dual number."
    p)

  (declare dual-part (Dual :t -> :t))
  (define (dual-part (Dual _ d))
    "The dual (i.e., derivative) part of a dual number."
    d)

  (define (sq x)
    (* x x))

  (define-instance (Eq :t => Eq (Dual :t))
    "Note: Eq only compares the primal component."
    (define (== (Dual a _) (Dual p _))
      (== a p)))

  (define-instance (Num :t => Num (Dual :t))
    (define (+ (Dual p1 d1) (Dual p2 d2))
      (Dual (+ p1 p2) (+ d1 d2)))

    (define (- (Dual p1 d1) (Dual p2 d2))
      (Dual (- p1 p2) (- d1 d2)))

    (define (* (Dual p1 d1) (Dual p2 d2))
      (Dual (* p1 p2)
            (+ (* p1 d2) (* d1 p2))))

    ;; N.B., A real number `z` converts to a dual number in the
    ;; following way. However, if we are calculating derivatives, we
    ;; instead evaluate a function at `z+ε`.
    (define (fromInt z)
      (Dual (fromInt z) 0)))

  (define-instance (Reciprocable :t => Reciprocable (Dual :t))
    (define (/ (Dual p1 d1) (Dual p2 d2))
      (Dual (/ p1 p2)
            (/ (- (* d1 p2)
                  (* p1 d2))
  	       (sq p2))))

    (define (reciprocal (Dual p1 d1))
      (Dual (reciprocal p1)
            (/ (negate d1) (sq p1)))))

  (define-instance ((Num :t) (Trigonometric :t) (Reciprocable :t) (Radical :t) => (Trigonometric (Dual :t)))
    (define (sin (Dual p1 d1))
      (Dual (sin p1)
            (* d1 (cos p1))))

    (define (cos (Dual p1 d1))
      (Dual (cos p1)
            (negate (* d1 (sin p1)))))

    (define (tan (Dual p1 d1))
      (Dual (tan p1)
            (/ d1 (sq (cos p1)))))

    (define (asin (Dual p1 d1))
      (Dual (asin p1)
            (/ d1 (sqrt (- 1 (sq p1))))))

    (define (acos (Dual p1 d1))
      (Dual (acos p1)
            (negate (/ d1 (sqrt (- 1 (sq p1)))))))

    (define (atan (Dual p1 d1))
      (Dual (atan p1)
            (/ d1 (+ 1 (sq p1)))))
    
    (define pi (Dual pi 0)))

  (define-instance ((Num :t) (Exponentiable :t) (Reciprocable :t) => (Exponentiable (Dual :t)))
    (define (exp (Dual p1 d1))
      (Dual (exp p1)
            (* d1 (exp p1))))

    (define (ln (Dual p1 d1))
      (Dual (ln p1)
            (/ d1 p1)))

    (define (pow dual1 dual2)
      (exp (* dual2 (ln dual1))))

    (define (log dual1 dual2)
      (/ (ln dual2) (ln dual1)))

    (define ee (Dual ee 0)))

  (define-instance ((Num :t) (Radical :t) (Reciprocable :t) (Exponentiable :t) => (Radical (Dual :t)))
    (define (nth-root n (Dual p1 d1))
      ;; root(x,n)' = (x^(1/n))'
      ;;            = (1/n)x^(1/n - 1)
      ;;            = (1/n)x^((1-n)/n)
      ;;            = 1/[n * x^((n-1)/n)]
      (let ((n* (fromInt n)))
        (Dual (nth-root n p1)
  	      (/ d1 (* n* (pow p1 (/ (- n* 1) n*)))))))

    (define (sqrt (Dual p1 d1))
      (Dual (sqrt p1)
            (/ d1 (* 2 (sqrt p1))))))

  (define-instance ((Ord :t) => Ord (Dual :t))
    "Note: Ord only compares the primal component."
    (define (<=> (Dual p1 _) (Dual p2 _))
      (<=> p1 p2)))

  (define-instance ((Hash :t) => (Hash (Dual :t)))
    "Note: Hash only considers the primal component in order to be consistent with Eq."
    (define (hash (Dual p1 _))
      (hash p1))))


#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/DUAL")
