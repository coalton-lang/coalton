;;;; dual.lisp

"
dual numbers are a hypercomplex number system [1]. A dual number has the form
a + bε where a and b are real numbers and ε is a symbol that satisfies ε^2=0
and ε!=0. One application of dual numbers is automatic differentiation; an example
taken from [2] is as follows:

    consider you have the given expression f(x) = 3x+2 and you want to calculate
    f(4) and f'(4).

    You then convert 4 into a dual number which gives:

        4+1ε

    then the computation is as follows:

        (4*1ε) * (3 + 0ε) =
        12 * 0ε + 3ε + 0ε^2 =
        12 + 3ε

    lastly, you need to add the constant 2 of the expression

        (12 + 3ε) + (2 + 0ε) =
        14 + 3ε

   in this result, the primal 14 is the value of f(4) and the dual is the value of
   of f'(4).

   Haskell has an automatic differentiation library and you can find it here
   https://hackage.haskell.org/package/ad.

   limitations:

     some limitations is that `Ord` and `Eq` and `Hash` only work on primals.
     but the advantages of this approach is that code will be auto differentiated
     for free.

   references:

   [1] https://en.wikipedia.org/wiki/Dual_number
   [2] https://blog.demofox.org/2014/12/30/dual-numbers-automatic-differentiation/"

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
   #:dual-part))

(in-package #:coalton-library/math/dual)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel

  (define-type (Dual :t)
    "Representation of a Dual number in the form (a + bε) where a and b are    real numbers and ε satisfie ε**2 = 0 and ε != 0.  Note: `Eq`, and `Ord`    and `Hash` only work on the primal component."
    (Dual :t :t))
   
  (declare primal-part (Dual :t -> :t))
  (define (primal-part (Dual p _))
    p)

  (declare dual-part (Dual :t -> :t))
  (define (dual-part (Dual _ d))
    d)

  (define (sq x)
    (* x x))
   
  (define-instance (Eq :t => Eq (Dual :t))
    (define (== (Dual a _) (Dual p _))
      (== a p)))
  
  (define-instance (Num :t => Num (Dual :t)) 
    (define (+ (Dual p1 d1) (Dual p2 d2))
      (Dual (+ p1 p2) (+ d1 d2)))
     
    (define (- (Dual p1 d1) (Dual p2 d2))
      (Dual (- p1 p2) (- d1 d2)))

    (define (* (Dual p1 d1) (Dual p2 d2))
      (Dual (* p1 p2)
            (+ (* p1  d2) (* d1 p2))))
     
    (define (fromInt z)
      (Dual (fromInt z) 0)))
   
  (define-instance (Reciprocable :t => Reciprocable (Dual :t))
    (define (/ (Dual p1 d1) (Dual p2 d2))
      (Dual (/ p1 p2)
            (/ (- (* d1 p2)
                  (* p1 d2))
  	       (* p2 p2))))
     
    (define (reciprocal (Dual p1 d1))
      (Dual (reciprocal p1)
            (/ (negate d1) (* p1 p1)))))

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
            (/ d1 (+ 1 (sq p1))))))

  (define-instance ((Num :t) (Exponentiable :t) (Reciprocable :t) => (Exponentiable (Dual :t)))
    (define (exp (Dual p1 d1))
      (Dual (exp p1)
            (* d1 (exp p1))))

    (define (pow dual1 dual2)
      (exp (* dual2 (ln dual1))))
     
    (define (ln (Dual p1 d1))
      (Dual (ln p1)
            (/ d1 p1)))
     
    (define (log dual1 dual2)
      (/ (ln dual2) (ln dual1))))
   
  (define-instance ((Num :t) (Radical :t) (Reciprocable :t) (Exponentiable :t) => (Radical (Dual :t)))
    (define (nth-root n (Dual p1 d1))
      (let ((n* (fromInt n)))
        (Dual (nth-root n p1)
  	      (pow (* p1 (/ d1 n*)) (- (reciprocal n*) 1)))))
     
    (define (sqrt (Dual p1 d1))
      (Dual (sqrt p1)
            (/ p1 (* 2 (sqrt p1))))))

  (define-instance ((Ord :t) (Ord :t) => Ord (Dual :t))
    (define (<=> (Dual p1 _) (Dual p2 _))
      (<=> p1 p2)))
      
     

  (define-instance ((Hash :t) (Hash :t) => (Hash (Dual :t)))
    (define (hash (Dual p1 _))
      (hash p1))))
      

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/DUAL")


