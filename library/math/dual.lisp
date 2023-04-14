;;;; dual.lisp
;;;;
;;;; dual numbers to enable automatic differentiation

(coalton-library/utils:defstdlib-package #:coalton-library/math/dual
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/functions
   #:coalton-library/math/arith)
  (:export
   #:Dual
   #:primal-part
   #:dual-part
   #:*Dual*
   #:*Real*))

(in-package #:coalton-library/math/dual)

(coalton-toplevel

 (define-type *Real*
     (*Real* Integer))

 (define-type *Dual*
     (*Dual* Integer))
 
 (define-type (Dual :t)
     (Dual :t :t))
 
 (declare primal-part (Dual :t -> :t))
 (define (primal-part (Dual r _)) r)

 (declare dual-part (Dual :t -> :t))
 (define (dual-part (Dual _ d))
     d)

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
       (Dual (fromInt z) (fromInt 0)))))
 
#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/DUAL")


