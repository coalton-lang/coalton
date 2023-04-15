;;;; dual.lisp
;;;;
;;;; dual numbers to enable automatic differentiation

(coalton-library/utils:defstdlib-package #:coalton-library/math/dual
    (:use
     #:coalton
     #:coalton-library/builtin
     #:coalton-library/classes
     #:coalton-library/functions
     #:coalton-library/math/arith
     #:coalton-library/math/elementary
     #:coalton-library/math/integral)
  (:export
   #:Dual
   #:primal-part
   #:dual-part))

(in-package #:coalton-library/math/dual)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-type (Dual :t)
    "Representation of a Dual number in the form (a + bE)."
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
    
    (define (== (Dual a b) (Dual p q))
      (and (== a p)
           (== b q))))

  (define-instance (Num :t => Num (Dual :t))
    
    (define (+ (Dual p1 d1) (Dual p2 d2))
      (Dual (+ p1 p2)
            (+ d1 d2)))
    
    (define (- (Dual p1 d1) (Dual p2 d2))
      (Dual (- p1 p2)
            (- d1 d2)))

    (define (* (Dual p1 d1) (Dual p2 d2))
      (Dual (* p1 p2)
            (+ (* p1  d2) (* d1 p2))))
    
    (define (fromInt z)
      (Dual (fromInt z) (fromInt 0))))

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
            (* (* -1 d1) (sin p1))))
    
    (define (tan (Dual p1 d1))
      (Dual (tan p1)
            (/ d1 (sq (cos p1)))))
    
    (define (asin (Dual p1 d1))
      (Dual (asin p1)
            (* d1
               (reciprocal (sqrt (- 1 (sq p1)))))))
    
    (define (acos (Dual p1 d1))
      (Dual (acos p1)
            (* d1
               (/ -1 (sqrt (- 1 (sq p1)))))))
    
    (define (atan (Dual p1 d1))
      (Dual (atan p1)
            (/ d1
               (+ 1 (sq p1))))))

  (define-instance ((Num :t) (Exponentiable :t) (Reciprocable :t) => (Exponentiable (Dual :t)))
    
    (define (exp (Dual p1 d1))
      (Dual (exp p1)
            (* d1 (exp p1))))

    (define (pow dual1 dual2)
      (exp (* dual2 (ln dual1))))
    
    (define (ln (Dual p1 d1))
      (Dual (ln p1)
            (* d1 (reciprocal p1))))
    
    (define (log dual1 dual2)
      (/ (ln dual2) (ln dual1))))

  (define-instance ((Num :t) (Radical :t) (Reciprocable :t) (Exponentiable :t) => (Radical (Dual :t)))
    
    (define (nth-root n (Dual p1 d1))
      (let ((n* (fromInt n)))
        (Dual (pow  p1 (/ 1 n*))
              (pow (* p1 (/ d1 n*)) (- (reciprocal n*) 1)))))
    
    (define (sqrt (Dual p1 d1))
      (Dual (sqrt p1)
            (/ (* (/ 1 2) p1)
               (sqrt p1))))))


#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/DUAL")


