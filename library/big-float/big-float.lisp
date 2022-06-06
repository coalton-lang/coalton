;;;; big-float.lisp
;;;;
;;;; Arbitrary precision floats using SBCL's MPFR library.

(coalton-library/utils:defstdlib-package #:coalton-library/big-float
  (:use #:coalton
        #:coalton-library/classes
        #:coalton-library/arith)

  (:export
   #:RoundingMode
   #:rndna
   #:rndn
   #:rndz
   #:rndu
   #:rndd
   #:rnda
   #:rndf
   #:set-rounding-mode!
   #:get-rounding-mode
   #:with-rounding

   #:set-precision!
   #:get-precision
   #:with-precision

   #:Big-Float
   #:with-precision-rounding

   #:bf-pi
   #:bf-e))

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(in-package #:coalton-library/big-float)

#-sbcl (error "This file is hopelessly SBCL specific.")
#+sbcl
cl-user::
(eval-when (:compile-toplevel :load-toplevel)
  (loop :until (uiop:featurep :sb-mpfr)
        :do (restart-case (error "SB-MPFR failed to load, for some reason. ~
                                  This is probably due to the shared library ~
                                  not existing, or the system being unable ~
                                  to find it.")
              (reload-sb-mpfr ()
                :report "Reload the MPFR library"
                (handler-case (sb-mpfr::load-mpfr)
                  (simple-warning (e) (declare (ignore e))))))))

;;; Preliminary patched functionality for SB-MPFR
;;;
;;; This functionality has been submitted upstream as a patch to
;;; SBCL. This code will be able to be deleted when that is merged.

(cl:declaim (cl:inline __gmpq_init))
(sb-alien:define-alien-routine __gmpq_init sb-alien:void
  (sb-mpfr::x (sb-alien:* (sb-alien:struct sb-gmp::gmprat))))

(cl:declaim (cl:inline __gmpq_clear))
(sb-alien:define-alien-routine __gmpq_clear sb-alien:void
  (sb-mpfr::x (sb-alien:* (sb-alien:struct sb-gmp::gmprat))))

(cl:declaim (cl:inline sb-mpfr::mpfr_get_q))
(sb-alien:define-alien-routine sb-mpfr::mpfr_get_q sb-alien:void
  (sb-mpfr::rop (sb-alien:* (sb-alien:struct sb-gmp::gmprat)))
  (sb-mpfr::op  (sb-alien:* (sb-alien:struct sb-mpfr::mpfrfloat))))

(cl:defun mpz->bigint (z)
  (cl:let* ((size (cl:abs (sb-alien:slot z 'sb-gmp::mp_size)))
            (neg? (cl:minusp (sb-alien:slot z 'sb-gmp::mp_size)))
            (bigint (sb-gmp::allocate-bignum (cl:1+ size))))
    (sb-sys:with-pinned-objects (bigint)
      (cl:* (cl:if neg? -1 1)
            (sb-gmp::gmp-z-to-bignum (sb-alien:slot z 'sb-gmp::mp_d) bigint size)))))

(cl:defun mpq->rational (q)
  (sb-kernel:build-ratio (mpz->bigint (sb-alien:slot q 'sb-gmp::mp_num))
                         (mpz->bigint (sb-alien:slot q 'sb-gmp::mp_den))))

(cl:defun mpfr->rational (f)
  (sb-alien:with-alien ((q (sb-alien:struct sb-gmp::gmprat)))
    (__gmpq_init (sb-alien:addr q))
    (sb-mpfr::mpfr_get_q (sb-alien:addr q) f)
    (cl:prog1 (mpq->rational q)
      (__gmpq_clear (sb-alien:addr q)))))


;;; Coalton Implementation

(coalton-toplevel
  ;; Rounding modes
  (repr :native (cl:member :MPFR_RNDNA :MPFR_RNDN :MPFR_RNDZ :MPFR_RNDU :MPFR_RNDD :MPFR_RNDA :MPFR_RNDF))
  (define-type RoundingMode)

  (define rndna
    "RouND to Nearest Away."
    (lisp RoundingMode () ':MPFR_RNDNA))
  (define rndn
    "RouND to Nearest, with the even rounding rule."
    (lisp RoundingMode () ':MPFR_RNDN))
  (define rndz
    "RouND toward Zero."
    (lisp RoundingMode () ':MPFR_RNDZ))
  (define rndu
    "RouND Up, toward positive infinity."
    (lisp RoundingMode () ':MPFR_RNDU))
  (define rndd
    "RouND Down, toward negative infinity."
    (lisp RoundingMode () ':MPFR_RNDD))
  (define rnda
    "RouND Away from zero."
    (lisp RoundingMode () ':MPFR_RNDA))
  (define rndf
    "Faithful rounding (experimental)."
    (lisp RoundingMode () ':MPFR_RNDF))

  ;; Calculation Configuration
  (declare set-precision! (UFix -> Unit))
  (define (set-precision! prec-bits)
    "Set the precision of Big-Float arithmetic to PREC-BITS bits."
    (unless (> prec-bits 0)
      (error "Precision must be positive."))
    (lisp Unit (prec-bits)
      (sb-mpfr:set-precision prec-bits)
      Unit))

  (declare set-rounding-mode! (RoundingMode -> Unit))
  (define (set-rounding-mode! r)
    "Set the global rounding mode for Big-Float operations."
    (lisp Unit (r)
      (cl:setf sb-mpfr:*mpfr-rnd* r)
      Unit))

  (declare get-precision (Unit -> UFix))
  (define (get-precision _)
    "Get the current precision of Big-Float arithmetic."
    (lisp UFix ()
      sb-mpfr:+mpfr-precision+))

  (declare get-rounding-mode (Unit -> RoundingMode))
  (define (get-rounding-mode _)
    "Get the current rounding-mode of Big-Float arithmetic."
    (lisp RoundingMode ()
      sb-mpfr:*mpfr-rnd*))

  (declare with-precision-rounding
           (UFix -> RoundingMode -> (Unit -> :a) -> :a))
  (define (with-precision-rounding prec-bits rnd f)
    "Call F with a temporary Big-Float PREC-BITS precision and RND rounding-mode."
    (lisp :a (f prec-bits rnd)
      (cl:let ((sb-mpfr:+mpfr-precision+ prec-bits)
               (sb-mpfr:*mpfr-rnd* rnd))
        (coalton-impl/codegen::A1 f Unit))))

  (declare with-precision (UFix -> (Unit -> :a) -> :a))
  (define (with-precision prec-bits f)
    "Call F with a temporary Big-Float precision PREC-BITS."
    (with-precision-rounding prec-bits (get-rounding-mode) f))

  (declare with-rounding (RoundingMode -> (Unit -> :a) -> :a))
  (define (with-rounding rnd f)
    "Call F with a temporary Big-Float rounding-mode RND."
    (with-precision-rounding (get-precision) rnd f))

  ;; Float Type
  (repr :native sb-mpfr:mpfr-float)
  (define-type Big-Float
    "An arbitrary (but fixed) precision floating point number.")

  ;; Equality
  (define-instance (Eq Big-Float)
    (define (== a b)
      (lisp Boolean (a b)
        (to-boolean (sb-mpfr:= a b)))))

  ;; Ordering
  (define-instance (Ord Big-Float)
    (define (<=> a b)
      (lisp Ord (a b)
        (cl:ecase (sb-mpfr:compare a b)
          (-1  LT)
          (0   EQ)
          (1   GT)))))

  ;; Basic number operations
  (define-instance (Num Big-Float)
    (define (+ a b)
      (lisp Big-Float (a b)
        (cl:values (sb-mpfr:add a b))))

    (define (- a b)
      (lisp Big-Float (a b)
        (cl:values (sb-mpfr:sub a b))))

    (define (* a b)
      (lisp Big-Float (a b)
        (cl:values (sb-mpfr:mul a b))))

    (define (fromInt n)
      (lisp Big-Float (n)
        (sb-mpfr:coerce n 'sb-mpfr:mpfr-float))))

  ;; Conversion
  (define-instance (Into Integer Big-Float)
    (define (into a)
      (fromInt a)))

  (define-instance (Into Fraction Big-Float)
    (define (into a)
      (lisp Big-Float (a)
        (sb-mpfr:coerce a 'sb-mpfr:mpfr-float))))

  (define-instance (Into Single-Float Big-Float)
    (define (into a)
      (lisp Big-Float (a)
        (sb-mpfr:coerce a 'sb-mpfr:mpfr-float))))

  (define-instance (Into Double-Float Big-Float)
    (define (into a)
      (lisp Big-Float (a)
        (sb-mpfr:coerce a 'sb-mpfr:mpfr-float))))

  ;; Quantization and division
  (declare bf-floor (Big-Float -> (Tuple Integer Big-Float)))
  (define (bf-floor f)
    (lisp (Tuple Integer Big-Float) (f)
      (cl:let ((x (sb-mpfr:floor f)))
        (Tuple (sb-mpfr:coerce x 'cl:integer)
               (sb-mpfr:sub x f)))))

  (declare bf-ceiling (Big-Float -> (Tuple Integer Big-Float)))
  (define (bf-ceiling f)
    (lisp (Tuple Integer Big-Float) (f)
      (cl:let ((x (sb-mpfr::ceil f)))   ; SBCL bug: not exported correctly
        (Tuple (sb-mpfr:coerce x 'cl:integer)
               (sb-mpfr:sub x f)))))

  (define-instance (Quantizable Big-Float)
    (define (quantize f)
      (match (Tuple (bf-floor f) (bf-ceiling f))
        ((Tuple (Tuple fl flr) (Tuple ce cer))
         (Quantization f fl flr ce cer)))))

  (define-instance (Dividable Big-Float Big-Float)
    (define (general/ a b)
      (lisp Big-Float (a b)
        (cl:values (sb-mpfr:div a b)))))

  ;; Trig
  (define-instance (Trigonometric Big-Float)
    (define (sin x)
      (lisp Big-Float (x)
        (cl:values (sb-mpfr:sin x))))
    (define (cos x)
      (lisp Big-Float (x)
        (cl:values (sb-mpfr:cos x))))
    (define (tan x)
      (lisp Big-Float (x)
        (cl:values (sb-mpfr:tan x))))
    (define (asin x)
      (lisp Big-Float (x)
        (cl:values (sb-mpfr:asin x))))
    (define (acos x)
      (lisp Big-Float (x)
        (cl:values (sb-mpfr:acos x))))
    (define (atan x)
      (lisp Big-Float (x)
        (cl:values (sb-mpfr:atan x)))))

  ;; Exp/Log
  (define-instance (Exponentiable Big-Float)
    (define (expt x n)
      (lisp Big-Float (x n)
        (cl:values (sb-mpfr:power x n))))
    (define (log n x)
      (lisp Big-Float (x n)
        (cl:values (sb-mpfr:div (sb-mpfr:log x) (sb-mpfr:log n))))))

  (declare bf-sqrt (Big-Float -> Big-Float))
  (define (bf-sqrt x)
    (lisp Big-Float (x)
      (cl:values (sb-mpfr:sqrt x))))

  (specialize sqrt bf-sqrt (Big-Float -> Big-Float))

  ;; Float
  ;;
  (define (bf-pi _)
    "Return the value of pi to the currently set precision."
    (lisp Big-Float ()
      (cl:values (sb-mpfr:const-pi))))
  (define (bf-ee _)
    "Return the value of ee = exp(1) to the currently set precision."
    (lisp Big-Float ()
      (cl:values (sb-mpfr:exp (sb-mpfr:coerce 1 'sb-mpfr:mpfr-float)))))

  ;; BUG: These are calculated just once, so if we change precision,
  ;; these will *NOT* get updated.
  (define-instance (Float Big-Float)
    (define pi (bf-pi))
    (define ee (bf-ee)))

  (define-instance (RealFloat Big-Float)
    (define (rationalize x)
      (lisp Fraction (x)
        (mpfr->rational (sb-mpfr::mpfr-float-ref x)))))
)                                       ; COALTON-TOPLEVEL

(coalton-library/complex::%define-standard-complex-instances Big-Float)

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/BIG-FLOAT")
