;;;; computable-reals.lisp
;;;;
;;;; Reals library using computable-reals (https://github.com/stylewarning/computable-reals)

(defpackage #:coalton-library/computable-reals
  (:use #:coalton
        #:coalton-prelude
        #:coalton-library/math)
  (:local-nicknames (#:cr #:computable-reals)
                    (#:complex #:coalton-library/math/complex)
                    (#:math #:coalton-library/math)
                    (#:cell #:coalton-library/cell))
  (:export
   #:Creal
   #:set-comparison-threshold!
   #:comparison-threshold
   #:approx
   #:rational-approx
   #:rationalize
   #:cr-print))

(in-package #:coalton-library/computable-reals)

(named-readtables:in-readtable coalton:coalton)

;; Setting a default comparison threshold for Eq and Ord
(cl:defvar *creal-comparison-threshold* 106)

;;;
;;; Defining the Creal type as a wrapper around computable-reals
;;;


(coalton-toplevel
  
 (repr :native (cl:or cr:creal cl:rational))
 (define-type Creal)

 (define (set-comparison-threshold! k)
   "Sets the global `Creal` comparison threshold to k bits after the 'decimal' point.

See `comparison-threshold` for more details."
   (lisp UFix (k)
         (cl:setq *creal-comparison-threshold* k))
   Unit)

 (define (comparison-threshold)
   "Returns the current `Creal` comparison threshold measured as a number of bits after the 'decimal' point.

This threshold is used to ensure `Eq` and `Ord` instances terminate. (In general computable real arithmetic is undecidable.) Note that if the production of a `Creal` depends on comparison, *there is no guarantee that the `Creal` will be accurate to any precision*."
   (lisp UFix ()
         *creal-comparison-threshold*)))

;;;
;;; Instances
;;;

(coalton-toplevel
  
  (define-instance (Eq Creal)
    (define (== a b)
      (lisp Boolean (a b)
        (cl:zerop (cr:approx-r (cr:-r a b)
                               *creal-comparison-threshold*))))))

(coalton-toplevel
  
  (define-instance (Num Creal)
    (define (+ a b)
      (lisp Creal (a b)
        (cr:+r a b)))
    (define (- a b)
      (lisp Creal (a b)
        (cr:-r a b)))
    (define (* a b)
      (lisp Creal (a b)
        (cr:*r a b)))
    (define (fromint x)
      (lisp Creal (x)
        x))))

(coalton-toplevel
  
  (define-instance (Ord Creal)
    (define (<=> a b)
      (let diff-approxd = (lisp Integer (a b)
                            (cr:approx-r (cr:-r a b)
                                         *creal-comparison-threshold*)))
      (cond
        ((math:zero? diff-approxd)
         EQ)
        ((math:positive? diff-approxd)
         GT)
        ((math:negative? diff-approxd)
         LT)))))

(coalton-toplevel
  
  (define-instance (Reciprocable Creal)
    (define (/ a b)
      (lisp Creal (a b)
        (cr:/r a b)))
    (define (math:reciprocal n)
      (lisp Creal (n)
        (cr:/r n)))))

(coalton-toplevel
  
  (define-instance (math:Exponentiable Creal)

    (define (math:exp x)
      (lisp Creal (x)
        (cr:exp-r x)))

    (define (math:pow x y)
      (lisp Creal (x y)
        (cr:expt-r x y)))

    (define (math:ln x)
      (lisp Creal (x)
        (cr:log-r x)))

    (define (math:log b x)
      (lisp Creal (x b)
        (cr:log-r x b)))

    (define ee (exp 1))))

(coalton-toplevel
  
  (define-instance (math:Radical Creal)

    (define (math:nth-root n x)
      (lisp Creal (n x)
        (cr:expt-r x (cl:/ n))))

    (define (math:sqrt x)
      (lisp Creal (x)
        (cr:sqrt-r x)))))

(coalton-toplevel
  
  (define-instance (math:Trigonometric Creal)

    (define (math:sin x)
      (lisp Creal (x)
        (cr:sin-r x)))

    (define (math:cos x)
      (lisp Creal (x)
        (cr:cos-r x)))

    (define (math:tan x)
      (lisp Creal (x)
        (cr:tan-r x)))

    (define (math:asin x)
      (math:atan (/ x (sqrt (- 1 (* x x))))))

    (define (math:acos x)
      (math:atan (/ (sqrt (- 1 (* x x))) x)))

    (define (math:atan x)
      (lisp Creal (x)
        (cr:atan-r x)))

    (define pi
      (lisp Creal ()
        cr:+pi-r+))))


(coalton-toplevel

  (define-instance (Complex Creal)
    (define (complex a b)
      (complex::%Complex a b))

    (define (real-part z)
      (match z
        ((complex::%Complex a _) a)))

    (define (imag-part z)
      (match z
        ((complex::%Complex _ b) b))))

  (define-instance ((Complex :a) (Into :a Creal) => (Into (Complex :a) (Complex Creal)))
    (define (Into x)
      (Complex (Into (real-part x))
               (Into (imag-part x))))))

(coalton-toplevel

  (define-instance (math:Polar Creal)
    (define (phase z)
      (math:atan2 (imag-part z) (real-part z)))
    (define (polar z)
      (Tuple (magnitude z) (phase z)))))

(coalton-toplevel

  (define-instance (Elementary Creal)))

;;;
;;; Into's
;;;

(coalton-toplevel

  (define-instance (Into Single-Float Creal)
    (define (Into x)
      (Lisp CReal (x)
        (cl:rational x))))
  
  (define-instance (Into Double-Float Creal)
    (define (Into x)
      (Lisp CReal (x)
        (cl:rational x))))
  
  (define-instance (Into Fraction CReal)
    (define (Into x)
      (Lisp CReal (x)
        x)))
  
  (define-instance (Into Integer CReal)
    (define (Into x)
      (Lisp CReal (x)
        x)))

  (define-instance (Into UFix CReal)
    (define (Into x)
      (Lisp CReal (x)
        x)))

  (define-instance (Into IFix CReal)
    (define (Into x)
      (Lisp CReal (x)
        x)))

  (define-instance (Into U64 CReal)
    (define (Into x)
      (Lisp CReal (x)
        x)))

  (define-instance (Into I64 CReal)
    (define (Into x)
      (Lisp CReal (x)
        x)))
  
  (define-instance (Into U32 CReal)
    (define (Into x)
      (Lisp CReal (x)
        x)))

  (define-instance (Into I32 CReal)
    (define (Into x)
      (Lisp CReal (x)
        x)))
  
  (define-instance (Into U16 CReal)
    (define (Into x)
      (Lisp CReal (x)
        x)))

  (define-instance (Into I16 CReal)
    (define (Into x)
      (Lisp CReal (x)
        x)))
  
  (define-instance (Into U8 CReal)
    (define (Into x)
      (Lisp CReal (x)
        x)))

  (define-instance (Into I8 CReal)
    (define (Into x)
      (Lisp CReal (x)
        x))))

;;;
;;; Other computable-reals utilities
;;;

(coalton-toplevel

  (declare approx (CReal -> UFix -> Integer))
  (define (approx x k)
    "Computes an approximation of the bits of a given `Creal`. Specifically, given an object of type `Creal` `X` and a non-negative integer `K`, return an integer `A` with

    `|A*2^(-k) - X| <= 2^(-K)`.

See `rational` or `rationalize` to produce a rational approximation of `Creal`."
    (lisp Integer (x k)
      (cr:approx-r x k)))

  (declare rational-approx (CReal -> UFix -> Fraction))
  (define (rational-approx x k)
    "Produce a rational approximation of `X` called `R` such that

    `|R - X| < 2^(-K)`."
    (lisp Fraction (x k)
      (cr:rational-approx-r x k)))

  (declare rationalize (CReal -> UFix -> Fraction))
  (define (rationalize x k)
    "Produce a rational approximation of `X` called `R` such that

    `|R - X| < 2^(-K)`,

   taking into account the maximum precision specified by `K` to return
   the simplest possible such approximation."
    (lisp Fraction (x k)
      (cr:rationalize-r x k)))

  ;; this is just for testing purposes. Intentionally not exported.
  (define (raw-approx x)
    "Returns an approximation for `Creal`s."
    (lisp Integer (x)
      (cr:raw-approx-r x)))

  (declare cr-print (CReal -> UFix -> Boolean))
  (define (cr-print x k)
    "Prints a real `R` up to `K` bits of precision."
    (lisp Boolean (x k)
      (cr:print-r x k))))

(coalton-toplevel

  (define-instance (math:Real CReal)
    (define (real-approx prec x)
      (rational-approx x prec))))
