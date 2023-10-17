;;;; big-float.lisp
;;;;
;;;; Arbitrary precision floats using SBCL's MPFR library.

(in-package #:cl-user)

#-sbcl
(error "This file is hopelessly SBCL specific.")

#+sbcl
(eval-when (:compile-toplevel :load-toplevel)
  (loop :until (uiop:featurep :sb-mpfr)
        :do (restart-case (error "SB-MPFR failed to load, for some reason. ~
                                  This is probably due to the shared library ~
                                  not existing, or the system being unable ~
                                  to find it. If you do not wish to install it, ~
                                  re-compile Coalton from scratch with the ~
                                  environment variable ~
                                  ~
                                  COALTON_PORTABLE_BIGFLOAT=1 ~
                                  ~
                                  set, or add the feature ~
                                  :COALTON-PORTABLE-BIGFLOAT.")
              (reload-sb-mpfr ()
                :report "Reload the MPFR library"
                (handler-case (sb-mpfr::load-mpfr)
                  (simple-warning (e) (declare (ignore e))))))))


(in-package #:coalton-library/big-float)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

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

;; Whether the mpfr ceiling function is properly exported
(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:if (uiop:featurep :sbcl)
         (cl:pushnew
          (cl:if (uiop:version< (cl:lisp-implementation-version) "2.2.5")
                 :sbcl-pre-2-2-5
                 :sbcl-post-2-2-5)
          cl:*features*)))

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
    (lisp RoundingMode () ':MPFR_RNDF)))

(coalton-toplevel
  
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
        (call-coalton-function f Unit))))

  (declare with-precision (UFix -> (Unit -> :a) -> :a))
  (define (with-precision prec-bits f)
    "Call F with a temporary Big-Float precision PREC-BITS."
    (with-precision-rounding prec-bits (get-rounding-mode) f))

  (declare with-rounding (RoundingMode -> (Unit -> :a) -> :a))
  (define (with-rounding rnd f)
    "Call F with a temporary Big-Float rounding-mode RND."
    (with-precision-rounding (get-precision) rnd f)))

(coalton-toplevel

  ;; Float Type
  (repr :native sb-mpfr:mpfr-float)
  (define-type MPFR-Float
    "An arbitrary (but fixed) precision floating point number.")

  (define-type Big-Float
    (BFValue MPFR-Float)
    (BFConst (Unit -> MPFR-Float)))

  (define (mpfr-of bf)
    (match bf
      ((BFValue v) v)
      ((BFConst f) (f))))

  ;; Equality
  (define-instance (Eq MPFR-Float)
    (define (== a b)
      (lisp Boolean (a b)
        (to-boolean (sb-mpfr:= a b)))))

  (define-instance (Eq Big-Float)
    (define (== a b)
      (== (mpfr-of a) (mpfr-of b))))

  ;; Ordering
  (define-instance (Ord MPFR-Float)
    (define (<=> a b)
      (lisp Ord (a b)
        (cl:ecase (sb-mpfr:compare a b)
          (-1  LT)
          (0   EQ)
          (1   GT)))))

  (define-instance (Ord Big-Float)
    (define (<=> a b)
      (<=> (mpfr-of a) (mpfr-of b))))

  ;; Basic number operations
  (define-instance (Num MPFR-Float)
    (define (+ a b)
      (lisp MPFR-Float (a b)
        (cl:values (sb-mpfr:add a b))))

    (define (- a b)
      (lisp MPFR-Float (a b)
        (cl:values (sb-mpfr:sub a b))))

    (define (* a b)
      (lisp MPFR-Float (a b)
        (cl:values (sb-mpfr:mul a b))))

    (define (fromInt n)
      (lisp MPFR-Float (n)
        (sb-mpfr:coerce n 'sb-mpfr:mpfr-float))))

  (define-instance (Num Big-Float)
    (define (+ a b)
      (BFValue (+ (mpfr-of a) (mpfr-of b))))
    (define (- a b)
      (BFValue (- (mpfr-of a) (mpfr-of b))))
    (define (* a b)
      (BFValue (* (mpfr-of a) (mpfr-of b))))
    (define (fromint x)
      (BFValue (fromInt x)))))

;;; Conversion
(coalton-toplevel

  (define-instance (Into Integer MPFR-Float)
    (define (into a)
      (fromInt a)))
  
  (define-instance (Into Integer Big-Float)
    (define (into a)
      (fromInt a)))

  (define-instance (Into Fraction MPFR-Float)
    (define (into a)
      (lisp MPFR-Float (a)
        (sb-mpfr:coerce a 'sb-mpfr:mpfr-float))))

  (define-instance (Into Fraction Big-Float)
    (define (into a)
      (BFValue (into a))))

  (define-instance (Into Single-Float MPFR-Float)
    (define (into a)
      (lisp MPFR-Float (a)
        (sb-mpfr:coerce a 'sb-mpfr:mpfr-float))))

  (define-instance (Into Single-Float Big-Float)
    (define (into a)
      (BFValue (into a))))
  
  (define-instance (Into Double-Float MPFR-Float)
    (define (into a)
      (lisp MPFR-Float (a)
        (sb-mpfr:coerce a 'sb-mpfr:mpfr-float))))

  (define-instance (Into Double-Float Big-Float)
    (define (into a)
      (BFValue (into a)))))

(coalton-toplevel

  ;; quantization and division
  (define-instance (Quantizable MPFR-Float)
    (define (floor f)
      (lisp Integer (f)
        (cl:let ((x (sb-mpfr:floor f)))
          (sb-mpfr:coerce x 'cl:integer))))
    (define (ceiling f)
      #+sbcl-pre-2-2-5
      (negate (floor (negate f)))
      #+sbcl-post-2-2-5
      (lisp Integer (f)
        (cl:let ((x (sb-mpfr:ceiling f)))
          (sb-mpfr:coerce x 'cl:integer))))
    (define (proper f)
      (lisp (Tuple Integer MPFR-Float) (f)
        (cl:let ((x (sb-mpfr:truncate f)))
          (Tuple
           (sb-mpfr:coerce x 'cl:integer)
           (sb-mpfr:sub f x))))))

  ;; quantization and division
  (define-instance (Quantizable Big-Float)
    (define (floor f)
      (floor (mpfr-of f)))
    (define (ceiling f)
      (ceiling (mpfr-of f)))
    (define (proper f)
      (match (proper (mpfr-of f))
        ((Tuple n x)
         (Tuple n (BFValue x))))))

  (define-instance (Reciprocable MPFR-Float)
    (define (/ a b)
      (lisp MPFR-float (a b)
        (cl:values (sb-mpfr:div a b))))
    (define (reciprocal a)
      (/ 1 a)))
  
  (define-instance (Reciprocable Big-Float)
    (define (/ a b)
      (BFValue (/ (mpfr-of a) (mpfr-of b))))
    (define (reciprocal a)
      (BFValue (/ 1 (mpfr-of a)))))

  (define-instance (Real MPFR-Float)
    (define (real-approx prec x)
      (coalton-library/math/real::rational-approx prec x)))

  (define-instance (Real Big-Float)
    (define (real-approx prec x)
      (real-approx prec (mpfr-of x))))

  (define-instance (Rational MPFR-Float)
    (define (to-fraction x)
      (lisp Fraction (x)
        (mpfr->rational (sb-mpfr::mpfr-float-ref x))))
    (define (best-approx x)
      (real-approx (get-precision) x)))
  
  (define-instance (Rational Big-Float)
    (define (to-fraction x)
      (to-fraction (mpfr-of x)))
    (define (best-approx x)
      (best-approx (mpfr-of x))))
  
  (define-instance (Transfinite MPFR-Float)
    (define infinity (/ 1 0))
    (define (infinite? x)
      (lisp Boolean (x)
        (to-boolean (sb-mpfr:infinityp x))))
    (define nan (/ 0 0))
    (define (nan? x)
      (lisp Boolean (x)
        (to-boolean (sb-mpfr:nan-p x)))))

  (define-instance (Transfinite Big-Float)
    (define infinity (BFValue (/ 1 0)))
    (define (infinite? x)
      (infinite? (mpfr-of x)) )
    (define nan (BFValue (/ 0 0)))
    (define (nan? x)
      (nan? (mpfr-of x))))

  (define-instance (Dividable Integer MPFR-Float)
    (define (general/ a b)
      (if (== 0 b)
          (/ (fromInt a) (fromInt b))
          (into (exact/ a b)))))
  
  (define-instance (Dividable Integer Big-Float)
    (define (general/ a b)
      (if (== 0 b)
          (/ (fromInt a) (fromInt b))
          (into (exact/ a b))))))

(coalton-library/math/complex::%define-standard-complex-instances MPFR-Float)

(coalton-library/math/complex::%define-standard-complex-instances Big-Float)

(coalton-toplevel
  
  (define (bf-pi)
    "Return the value of pi to the currently set precision."
    (lisp MPFR-Float ()
      (cl:values (sb-mpfr:const-pi))))

  ;; Trig
  (define-instance (Trigonometric MPFR-Float)
    (define (sin x)
      (lisp MPFR-Float (x)
        (cl:values (sb-mpfr:sin x))))
    (define (cos x)
      (lisp MPFR-Float (x)
        (cl:values (sb-mpfr:cos x))))
    (define (tan x)
      (lisp MPFR-Float (x)
        (cl:values (sb-mpfr:tan x))))
    (define (asin x)
      (lisp MPFR-Float (x)
        (cl:values (sb-mpfr:asin x))))
    (define (acos x)
      (lisp MPFR-Float (x)
        (cl:values (sb-mpfr:acos x))))
    (define (atan x)
      (lisp MPFR-Float (x)
        (cl:values (sb-mpfr:atan x))))
    ;; NOTE: in MPFR, Pi is calculated just once, so if we change precision,
    ;; it will *NOT* get updated.
    (define pi (bf-pi)))
  
  ;; Trig
  (define-instance (Trigonometric Big-Float)
    (define (sin x)
      (BFValue (sin (mpfr-of x))))
    (define (cos x)
      (BFValue (cos (mpfr-of x))))
    (define (tan x)
      (BFValue (tan (mpfr-of x))))
    (define (asin x)
      (BFvalue (asin (mpfr-of x))))
    (define (acos x)
      (BFValue (acos (mpfr-of x))))
    (define (atan x)
      (BFValue (atan (mpfr-of x))))
    (define pi (BFConst bf-pi))))

(coalton-toplevel
  
  (define (bf-ee)
    "Return the value of ee = exp(1) to the currently set precision."
    (lisp MPFR-Float ()
      (cl:values (sb-mpfr:exp (sb-mpfr:coerce 1 'sb-mpfr:mpfr-float)))))

  ;; Exp/Log
  (define-instance (Exponentiable MPFR-Float)
    (define (exp x)
      (lisp MPFR-Float (x)
        (cl:values (sb-mpfr:exp x))))
    (define (pow x n)
      (cond
        ((< x 0) nan)
        (True
         (lisp MPFR-Float (x n)
           (cl:values (sb-mpfr:power x n))))))
    (define (log n x)
      (cond
        ((<= n 0) nan)
        ((== n 2)
         (lisp MPFR-Float (x)
           (cl:values (sb-mpfr:log2 x))))
        ((== n 10)
         (lisp MPFR-Float (x)
           (cl:values (sb-mpfr:log10 x))))
        (True (/ (ln x) (ln n)))))
    (define (ln x)
      (lisp MPFR-Float (x)
        (cl:values (sb-mpfr:log x))))
    ;; NOTE: in MPFR, EE is calculated just once, so if we change precision,
    ;; it will *NOT* get updated.
    (define ee 
      "Return the value of ee = exp(1) to the currently set precision."
      (bf-ee)))

  ;; Exp/Log
  (define-instance (Exponentiable Big-Float)
    (define (exp x)
      (BFValue
       (exp (mpfr-of x))))
    (define (pow x n)
      (BFValue
       (pow (mpfr-of x) (mpfr-of n))))
    (define (log n x)
      (BFValue
       (log (mpfr-of n) (mpfr-of x))))
    (define (ln x)
      (BFValue
       (ln (mpfr-of x))))
    (define ee 
      "Return the value of ee = exp(1) to the currently set precision."
      (BFConst bf-ee))))

(coalton-toplevel
  
  (define-instance (Radical MPFR-Float)
    (define (sqrt x)
      (lisp MPFR-Float (x)
        (cl:values (sb-mpfr:sqrt x))))
    (define (nth-root n x)
      (coalton-library/math/elementary::canonical-nth-root n x)))
  
  (define-instance (Radical Big-Float)
    (define (sqrt x)
      (BFValue (sqrt (mpfr-of x))))
    (define (nth-root n x)
      (BFValue (nth-root n (mpfr-of x)))))
  
  (define-instance (Polar MPFR-Float)
    (define (phase z)
      (atan2 (imag-part z) (real-part z)))
    (define (polar z)
      (Tuple (magnitude z) (phase z))))
  
  (define-instance (Polar Big-Float)
    (define (phase z)
      (atan2 (imag-part z) (real-part z)))
    (define (polar z)
      (Tuple (magnitude z) (phase z))))


  (define-instance (Elementary MPFR-Float))
  (define-instance (Elementary Big-Float)))
                                        
;COALTON-TOPLEVEL

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/BIG-FLOAT")
