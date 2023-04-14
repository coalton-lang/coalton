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
   #:dual-real
   #:dual-dual
   #:sub-duals
   #:add-duals
   #:mul-duals))

(in-package #:coalton-library/math/dual)

(coalton-toplevel

 (define-type *Real*
     (*Real* Integer))

 (define-type *Dual*
     (*Dual* Integer))
 
 (define-type Dual
     (Dual *Real* *Dual*))
 
 (declare dual-real (Dual -> Integer))
 (define (dual-real d1)
     (match d1
	    ((Dual (*Real* x) (*Dual* y)) x)))
 
 (declare dual-dual (Dual -> Integer))
 (define (dual-dual d1)
     (match d1
	    ((Dual (*Real* x) (*Dual* y)) y)))

 (declare add-duals (Dual -> Dual -> Dual))
 (define (add-duals dual1 dual2)
     (Dual (*Real* (+ (dual-real dual1)
		      (dual-real dual2)))
	   (*Dual* (+ (dual-dual dual1)
		      (dual-dual dual2)))))

 (declare sub-duals (Dual -> Dual -> Dual))
 (define (sub-duals dual1 dual2)
     (Dual (*Real* (- (dual-real dual1)
		      (dual-real dual2)))
	   (*Dual* (- (dual-dual dual1)
		      (dual-dual dual2)))))
 (declare mul-duals (Dual -> Dual -> Dual))
 (define (mul-duals dual1 dual2)
     (Dual (*Real* (* (dual-real dual1) (dual-real dual2)))
	   (*Dual* (+ (* (dual-real dual1) (dual-dual dual2))
		      (* (dual-dual dual1)
			 (dual-real dual2)))))))
 
#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/DUAL")

