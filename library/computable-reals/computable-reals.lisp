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
   #:CReal
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
;;; Defining the CReal type as a wrapper around computable-reals
;;;


(coalton-toplevel
  
 (repr :native (cl:or cr:creal cl:rational))
 (define-type CReal)

 (define (set-comparison-threshold! k)
   "Sets the global `CReal` comparison threshold to k bits after the 'decimal' point.

See `comparison-threshold` for more details."
   (lisp UFix (k)
         (cl:setq *creal-comparison-threshold* k))
   Unit)

 (define (comparison-threshold)
   "Returns the current `CReal` comparison threshold measured as a number
of bits after the 'decimal' point.

This threshold is used to ensure `Eq` and `Ord` instances
terminate. (In general, computable real arithmetic is undecidable.)
Note that if the production of a `CReal` depends on comparison, *there
is no guarantee that the `CReal` will be accurate to any precision*."
   (lisp UFix ()
     *creal-comparison-threshold*)))

;;;
;;; Instances
;;;

(coalton-toplevel
  
  (define-instance (Eq CReal)
    (define (== a b)
      (lisp Boolean (a b)
        (cl:zerop (cr:approx-r (cr:-r a b)
                               *creal-comparison-threshold*))))))

(coalton-toplevel
  
  (define-instance (Num CReal)
    (define (+ a b)
      (lisp CReal (a b)
        (cr:+r a b)))
    (define (- a b)
      (lisp CReal (a b)
        (cr:-r a b)))
    (define (* a b)
      (lisp CReal (a b)
        (cr:*r a b)))
    (define (fromint x)
      (lisp CReal (x)
        x))))

(coalton-toplevel
  
  (define-instance (Ord CReal)
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
  
  (define-instance (Reciprocable CReal)
    (define (/ a b)
      (lisp CReal (a b)
        (cr:/r a b)))
    (define (math:reciprocal n)
      (lisp CReal (n)
        (cr:/r n)))))

(coalton-toplevel

  (define-instance (Dividable Integer CReal)
    (define (general/ a b)
      (/ (fromint a) (fromint b)))))

(coalton-toplevel
  
  (define-instance (math:Exponentiable CReal)

    (define (math:exp x)
      (lisp CReal (x)
        (cr:exp-r x)))

    (define (math:pow x y)
      (lisp CReal (x y)
        (cr:expt-r x y)))

    (define (math:ln x)
      (lisp CReal (x)
        (cr:log-r x)))

    (define (math:log b x)
      (lisp CReal (x b)
        (cr:log-r x b)))

    (define ee (exp 1))))

(coalton-toplevel
  
  (define-instance (math:Radical CReal)

    (define (math:nth-root n x)
      (lisp CReal (n x)
        (cr:expt-r x (cl:/ n))))

    (define (math:sqrt x)
      (lisp CReal (x)
        (cr:sqrt-r x)))))

(coalton-toplevel
  
  (define-instance (math:Trigonometric CReal)

    (define (math:sin x)
      (lisp CReal (x)
        (cr:sin-r x)))

    (define (math:cos x)
      (lisp CReal (x)
        (cr:cos-r x)))

    (define (math:tan x)
      (lisp CReal (x)
        (cr:tan-r x)))

    (define (math:asin x)
      (math:atan (/ x (sqrt (- 1 (* x x))))))

    (define (math:acos x)
      (math:atan (/ (sqrt (- 1 (* x x))) x)))

    (define (math:atan x)
      (lisp CReal (x)
        (cr:atan-r x)))

    (define pi
      (lisp CReal ()
        cr:+pi-r+))))


(coalton-toplevel

  (define-instance (complex:ComplexComponent CReal)
    (define (complex a b)
      (complex::%Complex a b))

    (define (real-part z)
      (match z
        ((complex::%Complex a _) a)))

    (define (imag-part z)
      (match z
        ((complex::%Complex _ b) b))))

  (define-instance ((complex:ComplexComponent :a) (Into :a CReal) => (Into (Complex :a) (Complex CReal)))
    (define (Into x)
      (Complex (Into (real-part x))
               (Into (imag-part x))))))

(coalton-toplevel

  (define-instance (math:Polar CReal)
    (define (phase z)
      (math:atan2 (imag-part z) (real-part z)))
    (define (polar z)
      (Tuple (magnitude z) (phase z)))))

(coalton-toplevel

  (define-instance (Elementary CReal)))

;;;
;;; Into's
;;;

(coalton-toplevel

  (define-instance (Into F32 CReal)
    (define (Into x)
      (Lisp CReal (x)
        (cl:rational x))))
  
  (define-instance (Into F64 CReal)
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
    "Computes an approximation of the bits of a given
`CReal`. Specifically, given an object of type `CReal` `x` and a
non-negative integer `k`, return an integer $a$ with

$$
\\vert a\\cdot 2^{-\\mathtt{k}} - \\mathtt{x}\\vert \leq 2^{-\\mathtt{k}}.
$$

See `rational` or `rationalize` to produce a rational approximation of
`CReal`."
    (lisp Integer (x k)
      (cr:approx-r x k)))

  (declare rational-approx (CReal -> UFix -> Fraction))
  (define (rational-approx x k)
    "Produce a rational approximation of `x` called $r$ such that

$$
\\vert r - \\mathtt{x} \\vert < 2^{-\\mathtt{k}}.
$$"
    (lisp Fraction (x k)
      (cr:rational-approx-r x k)))

  (declare rationalize (CReal -> UFix -> Fraction))
  (define (rationalize x k)
    "Produce a rational approximation of `x` called $r$ such that

$$
\\vert r - \\mathtt{x} \\vert < 2^{-\\mathtt{k}},
$$

taking into account the maximum precision specified by `k` to return
the simplest possible such approximation."
    (lisp Fraction (x k)
      (cr:rationalize-r x k)))

  ;; this is just for testing purposes. Intentionally not exported.
  (define (raw-approx x)
    "Returns an approximation for `CReal`s."
    (lisp Integer (x)
      (cr:raw-approx-r x)))

  (declare cr-print (CReal -> UFix -> Boolean))
  (define (cr-print x k)
    "Prints a real `x` up to `k` bits of precision."
    (lisp Boolean (x k)
      (cr:print-r x k))))

(coalton-toplevel

  (define-instance (math:Quantizable CReal)
    (define (floor x)
      (lisp Integer (x)
        (cr:floor-r x)))
    (define (ceiling x)
      (lisp Integer (x)
        (cr:ceiling-r x)))
    (define (proper x)
      (lisp (Tuple Integer CReal) (x)
        (cl:multiple-value-bind (integer-part decimal-part)
            (cr:truncate-r x)
          (Tuple integer-part decimal-part)))))

  (define-instance (math:Real CReal)
    (define (real-approx prec x)
      (rational-approx x prec))))
