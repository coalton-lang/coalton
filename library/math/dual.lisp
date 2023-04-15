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
     #:coalton-library/math/elementary)
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
  (define (primal-part (Dual r _)) r)

  (declare dual-part (Dual :t -> :t))
  (define (dual-part (Dual _ d))
    d)

  ;; base-part to get the base of a given log
  (declare base-part (Dual :t -> :t))
  (define (base-part (Dual b _))
    b)

  (declare exponent-part (Dual :t -> :t))
  (define (exponent-part (Dual expt _))
    expt)
  
  (define-instance (Eq :t => Eq (Dual :t))
    
    (define (== (Dual a b) (Dual p q))
      "Check that the given Dual numbers are the same."
      (and (== a p)
	   (== b q))))

  (define-instance (Num :t => Num (Dual :t))
    
    (define (+ (Dual p1 d1) (Dual p2 d2))
      "Addition of Dual numbers."
      (Dual (+ p1 p2)
	    (+ d1 d2)))
    
    (define (- (Dual p1 d1) (Dual p2 d2))
      "Subtraction of Dual numbers."
      (Dual (- p1 p2)
	    (- d1 d2)))

    (define (* (Dual p1 d1) (Dual p2 d2))
      "Multiplication of Dual numbers."
      (Dual (* p1 p2)
	    (+ (* p1  d2)
	       (* d1 p2))))
    
    (define (fromInt z)
      (Dual (fromInt z) (fromInt 0))))

  (define-instance (Reciprocable :t => Reciprocable (Dual :t))
    
    (define (/ (Dual p1 d1) (Dual p2 d2))
      "Division of Dual numbers."
      (Dual (/ p1 p2)
	    (/ (- (* d1 p2)
		  (* p1 d2))
	       (* p2 p2))))
    
    (define (reciprocal (Dual p1 d1))
      "Reciprocal of given Dual number."
      (Dual (/ 1 p1)
	    (/ d1
	       (* p1 p1)))))

  (define-instance ((Num :t) (Trigonometric :t) (Reciprocable :t) (Radical :t) (Exponentiable :t) => (Trigonometric (Dual :t)))
    
    (define (sin (Dual p1 d1))
      "Sin of given Dual number."
      (Dual (sin p1)
	    (* d1
	       (cos p1))))
    
    (define (cos (Dual p1 d1))
      "Cos of a given Dual Number."
      (Dual (cos p1)
	    (* (* -1 d1) (sin p1))))
    
    (define (tan (Dual p1 d1))
      "Tan of the given Dual number."
      (Dual (tan p1)
	    (/ d1 (pow (cos p1) 2))))
    
    (define (asin (Dual p1 d1))
      "Asin of the given Dual number."
      (Dual (asin p1)
	    (* d1
	       (/ 1 (sqrt (- 1 (pow p1 2)))))))
    
    (define (acos (Dual p1 d1))
      "Acos of the given Dual number."
      (Dual (acos p1)
	    (* d1
	       (/ -1 (sqrt (- 1 (pow p1 2)))))))
    
    (define (atan (Dual p1 d1))
      "Atan of the given Dual number."
      (Dual (atan p1)
	    (/ d1
	       (+ 1 (pow p1 2))))))

  (define-instance ((Num :t) (Exponentiable :t) (Reciprocable :t) => (Exponentiable (Dual :t)))
    
    (define (exp (Dual p1 d1))
      "Exp of the given Dual number."
      (Dual (exp p1)
	    (* (exp p1)
	       d1)))

    (define (pow (Dual p1 d1) (Dual n dummy))
      "Pow of the given Dual number."
      (Dual (pow p1 n)
	    (* d1
	       (* n
		  (pow p1 (- n 1))))))
    
    (define (ln (Dual p1 d1))
      "ln of the the given Dual number."
      (Dual (ln p1)
	    (* (/ 1 p1)
	       d1)))
    
    (define (log (Dual base dummy) (Dual p1 d1))
      "Log of the given Dual number."
      (Dual (log base p1)
	    (/ p1 d1))))

  (define-instance ((Num :t) (Radical :t) (Reciprocable :t) => (Radical (Dual :t)))
    
    (define (nth-root n (Dual p1 d1))
      "N-th root of the given Dual."
      (Dual (nth-root n p1)
	    (/ (* (/ 1 2) p1)
	       (nth-root n p1))))
    
    (define (sqrt (Dual p1 d1))
      "Sqrt of the given Dual number."
      (Dual (sqrt p1)
	    (/ (* (/ 1 2) p1)
	       (sqrt p1))))))


#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/DUAL")


