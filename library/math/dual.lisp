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

(coalton-toplevel

 (define-type (Dual :t)
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
     (define (== a b)
	 (lisp Boolean (a b)
	       (cl:equal a b))))

 (define-instance (Num :t => Num (Dual :t))
     (define (+ dual1 dual2)
	 (Dual (+ (primal-part dual1)
		  (primal-part dual2))
	       (+ (dual-part dual1)
		  (dual-part dual2))))
   (define (- dual1 dual2)
       (Dual (- (primal-part dual1)
		(primal-part dual2))
	     (- (dual-part dual1)
		(dual-part dual2))))
   (define (* dual1 dual2)
       (Dual (* (primal-part dual1) (primal-part dual2))
	     (+ (* (primal-part dual1) (dual-part dual2))
		(* (dual-part dual1)
		   (primal-part dual2)))))
   (define (fromInt z)
       (Dual (fromInt z) (fromInt 0))))

 (define-instance (Reciprocable :t => Reciprocable (Dual :t))
     (define (/ dual1 dual2)
	 (Dual (/ (primal-part dual1) (primal-part dual2))
	       (/ (- (* (dual-part dual1) (primal-part dual2))
		     (* (primal-part dual1) (dual-part dual2)))
		  (* (primal-part dual2) (primal-part dual2)))))
   (define (reciprocal dual1)
       (Dual (/ 1 (primal-part dual1))
	     (/ (dual-part dual1)
		(* (primal-part dual1) (primal-part dual1))))))

 (define-instance ((Num :t) (Trigonometric :t) (Reciprocable :t) (Radical :t) (Exponentiable :t) => (Trigonometric (Dual :t)))
     (define (sin dual1)
	(Dual (sin (primal-part dual1))
	      (* (dual-part dual1)
		 (cos (primal-part dual1)))))
   (define (cos dual1)
       (Dual (cos (primal-part dual1))
	     (* (* -1 (dual-part dual1)) (sin (primal-part dual1)))))
   (define (tan dual1)
       (Dual (tan (primal-part dual1))
	     (/ (dual-part dual1) (pow (cos (primal-part dual1)) 2))))
   (define (asin dual1)
       (Dual (asin (primal-part dual1))
	     (* (dual-part dual1)
		(/ 1 (sqrt (- 1 (pow (primal-part dual1) 2)))))))
   
   (define (acos dual1)
       (Dual (acos (primal-part dual1))
	     (* (dual-part dual1)
		(/ -1 (sqrt (- 1 (pow (primal-part dual1) 2)))))))
   (define (atan dual1)
       (Dual (atan (primal-part dual1))
	     (/ (dual-part dual1)
		(+ 1 (pow (primal-part dual1) 2))))))

 (define-instance ((Num :t) (Exponentiable :t) (Reciprocable :t) => (Exponentiable (Dual :t)))
     (define (exp dual1)
	 (Dual (exp (primal-part dual1))
	       (* (exp (primal-part dual1))
		  (dual-part dual1))))

   (define (pow dual1 n)
       (Dual (pow (primal-part dual1) (exponent-part n))
	     (* (dual-part dual1)
		(* (exponent-part n)
		   (pow (primal-part dual1) (- (exponent-part n) 1)))))) 
     (define (ln dual1)
	 (Dual (ln (primal-part dual1))
	       (* (/ 1 (primal-part dual1))
		  (dual-part dual1))))
     (define (log base dual1)
	 (Dual (log (base-part base) (primal-part dual1))
	       (/ (primal-part dual1) (dual-part dual1)))))

 (define-instance ((Num :t) (Radical :t) (Reciprocable :t) => (Radical (Dual :t)))
     (define (nth-root n dual1)
	 (Dual (nth-root n (primal-part dual1))
	       (/ (* (/ 1 2) (primal-part dual1))
		  (nth-root n (primal-part dual1)))))
   (define (sqrt dual1)
       (Dual (sqrt (primal-part dual1))
	     (/ (* (/ 1 2) (primal-part dual1))
		(sqrt (primal-part dual1)))))))
     
   
   


#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/DUAL")


