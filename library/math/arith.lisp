;;;; arith.lisp
;;;;
;;;; Number types and basic arithmetic.

(coalton-library/utils::defstdlib-package #:coalton-library/math/arith
    (:use
     #:coalton
     #:coalton-library/builtin
     #:coalton-library/classes
     #:coalton-library/functions
     #:coalton-library/utils)
  (:local-nicknames
   (#:bits #:coalton-library/bits))
  (:export
   #:Reciprocable
   #:/
   #:reciprocal
   #:Dividable
   #:general/
   #:/
   #:Transfinite
   #:infinity
   #:infinite?
   #:finite?
   #:negative-infinity
   #:nan
   #:nan?
   #:integer->float
   #:negate
   #:abs
   #:sign
   #:ash
   #:mkFraction
   #:numerator
   #:denominator
   #:1+
   #:1-
   #:positive?
   #:negative?
   #:nonpositive?
   #:nonnegative?
   #:nonzero?))

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(in-package #:coalton-library/math/arith)

(coalton-toplevel
  ;;
  ;; Division
  ;;

  (define-class ((Num :a)  => Reciprocable :a)
    "Any number with a multplicative inverse (reciprocal) where:
    1 = (* (reciprocal x) x) = (* x (reciprocal x))
    (/ x y) = (* x (reciprocal y))

If no reciprocal exists for an element, produce a run-time error (e.g. zero).
"
    (/ (:a -> :a -> :a))
    (reciprocal (:a -> :a)))

  (define-class (Dividable :arg-type :res-type)
    "The representation of a type such that division within that type possibly results in another type. For instance,


    (Dividable Integer Fraction)


establishes that division of two `Integer`s can result in a `Fraction`, whereas


    (Dividable Single-Float Single-Float)


establishes that division of two `Single-Float`s can result in a `Single-Float`.

Note that `Dividable` does *not* establish a default result type; you must constrain the result type yourself.

The function general/ is partial, and will error produce a run-time error if the divisor is zero.
"
    ;; This is a type that is more pragmatic and less mathematical in
    ;; nature. It expresses a division relationship between one input
    ;; type and one output type.
    (general/ (:arg-type -> :arg-type -> :res-type)))

  (define-instance (Reciprocable :a => (Dividable :a :a))
    (define (general/ a b) (/ a b)))

  (define-class (Transfinite :a)
    "Numberic type with a value for (positive) 'infinity' and/or 'NaN'"
    (infinity :a)
    (infinite? (:a -> Boolean))
    (nan :a)
    (nan? (:a -> Boolean)))

  (declare finite? ((Transfinite :a) => :a -> Boolean))
  (define (finite? x)
    "Neither infinite or NaN."
    (or (infinite? x) (nan? x))))

(cl:declaim (cl:inline %unsigned->signed))
(cl:defun %unsigned->signed (bits x)
  ;; This is the two's complement conversion of X (interpreted as BITS
  ;; bits) to a signed integer (as a Lisp object).
  (cl:-
   (cl:ldb (cl:byte (cl:1- bits) 0) x)
   (cl:dpb 0 (cl:byte (cl:1- bits) 0) x)))

(cl:defmacro %define-overflow-handler (name bits)
  `(cl:defun ,name (value)
     (cl:typecase value
       ((cl:signed-byte ,bits) value)
       (cl:otherwise
        (cl:cerror "Continue, wrapping around."
                   ,(cl:format cl:nil "Signed value overflowed ~D bits." bits))
        (%unsigned->signed ,bits (cl:mod value ,(cl:expt 2 bits)))))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:defparameter +fixnum-bits+
    #+sbcl sb-vm:n-fixnum-bits
    #-sbcl (cl:1+ (cl:floor (cl:log cl:most-positive-fixnum 2))))
  (cl:defparameter +unsigned-fixnum-bits+
    (cl:1- +fixnum-bits+)))

(%define-overflow-handler %handle-8bit-overflow 8)
(%define-overflow-handler %handle-16bit-overflow 16)
(%define-overflow-handler %handle-32bit-overflow 32)
(%define-overflow-handler %handle-64bit-overflow 64)
(%define-overflow-handler %handle-fixnum-overflow #.+fixnum-bits+)

(cl:defmacro %define-number-stuff (coalton-type)
  (cl:let* ((ops   '(cl:< cl:<= cl:>= cl:>))
            (specs (cl:loop
                      :for op :in ops
                      :collect (cl:gentemp (cl:symbol-name op)))))
    `(cl:progn
       ;; Inline these functions unconditionally.
       (cl:declaim (cl:inline ,@specs))
       (coalton-toplevel
         (define-instance (Eq ,coalton-type)
           (define (== a b)
             (lisp Boolean (a b)
               (to-boolean (cl:= a b)))))

         (define-instance (Ord ,coalton-type)
           (define (<=> a b)
             (lisp Ord (a b)
               (cl:cond
                 ((cl:< a b)
                  LT)
                 ((cl:> a b)
                  GT)
                 (cl:t
                  EQ)))))

         ;; These are originally defined in classes.lisp.
         ;;
         ;; These are specializations so we don't need to produce
         ;; intermediate MATCH calls.
         ,@(cl:loop
              :for op :in ops
              :for coalton-op := (cl:find-symbol (cl:symbol-name op) "COALTON-LIBRARY/CLASSES")
              :for spec :in specs
              :for type := `(,coalton-type -> ,coalton-type -> Boolean)
              :collect `(declare ,spec ,type)
              :collect `(define (,spec a b)
                          (lisp Boolean (a b)
                            (to-boolean (,op a b))))
              :collect `(specialize ,coalton-op ,spec ,type))))))

(%define-number-stuff U8)
(%define-number-stuff U16)
(%define-number-stuff U32)
(%define-number-stuff U64)
(%define-number-stuff I8)
(%define-number-stuff I16)
(%define-number-stuff I32)
(%define-number-stuff I64)
(%define-number-stuff Integer)
(%define-number-stuff Fraction)
(%define-number-stuff IFix)
(%define-number-stuff UFix)
(%define-number-stuff Single-Float)
(%define-number-stuff Double-Float)


(coalton-toplevel
  (define-instance (Num I8)
    (define (+ a b)
      (lisp I8 (a b)
        (%handle-8bit-overflow (cl:+ a b))))
    (define (- a b)
      (lisp I8 (a b)
        (%handle-8bit-overflow (cl:- a b))))
    (define (* a b)
      (lisp I8 (a b)
        (%handle-8bit-overflow (cl:* a b))))
    (define (fromInt x)
      (lisp I8 (x)
        (%handle-8bit-overflow x))))

  (define-instance (Num I16)
    (define (+ a b)
      (lisp I16 (a b)
        (%handle-16bit-overflow (cl:+ a b))))
    (define (- a b)
      (lisp I16 (a b)
        (%handle-16bit-overflow (cl:- a b))))
    (define (* a b)
      (lisp I16 (a b)
        (%handle-16bit-overflow (cl:* a b))))
    (define (fromInt x)
      (lisp I16 (x)
        (%handle-16bit-overflow x))))

  (define-instance (Num I32)
    (define (+ a b)
      (lisp I32 (a b)
        (%handle-32bit-overflow (cl:+ a b))))
    (define (- a b)
      (lisp I32 (a b)
        (%handle-32bit-overflow (cl:- a b))))
    (define (* a b)
      (lisp I32 (a b)
        (%handle-32bit-overflow (cl:* a b))))
    (define (fromInt x)
      (lisp I32 (x)
        (%handle-32bit-overflow x))))

  (define-instance (Num I64)
    (define (+ a b)
      (lisp I64 (a b)
        (%handle-64bit-overflow (cl:+ a b))))
    (define (- a b)
      (lisp I64 (a b)
        (%handle-64bit-overflow (cl:- a b))))
    (define (* a b)
      (lisp I64 (a b)
        (%handle-64bit-overflow (cl:* a b))))
    (define (fromInt x)
      (lisp I64 (x)
        (%handle-64bit-overflow x))))

  (define-instance (Num IFix)
    (define (+ a b)
      (lisp IFix (a b)
        (%handle-fixnum-overflow (cl:+ a b))))
    (define (- a b)
      (lisp IFix (a b)
        (%handle-fixnum-overflow (cl:- a b))))
    (define (* a b)
      (lisp IFix (a b)
        (%handle-fixnum-overflow (cl:* a b))))
    (define (fromInt x)
      (lisp IFix (x)
        (%handle-fixnum-overflow x)))))


(cl:defmacro %define-signed-instances (coalton-type bits cl:&rest supers)
  (cl:declare (cl:ignore bits))
  `(coalton-toplevel
     (define-instance (Into Integer ,coalton-type)
       (define (into x) (fromInt x)))

     ,@(cl:loop :for super :in supers :collecting
          `(define-instance (Into ,coalton-type ,super)
             (define (into x)
               (lisp ,super (x)
                 x))))))

(%define-signed-instances I8   8               Integer I64 I32 I16)
(%define-signed-instances I16  16              Integer I64 I32)
(%define-signed-instances I32  32              Integer I64)
(%define-signed-instances I64  64              Integer)
(%define-signed-instances IFix #.+fixnum-bits+ Integer)


(cl:defmacro %define-unsigned-num-instance (coalton-type bits cl:&rest supers)
  `(coalton-toplevel
     (define-instance (Num ,coalton-type)
       (define (+ a b)
         (lisp ,coalton-type (a b)
           (cl:values (cl:mod (cl:+ a b) ,(cl:expt 2 bits)))))
       (define (- a b)
         (lisp ,coalton-type (a b)
           (cl:values (cl:mod (cl:- a b) ,(cl:expt 2 bits)))))
       (define (* a b)
         (lisp ,coalton-type (a b)
           (cl:values (cl:mod (cl:* a b) ,(cl:expt 2 bits)))))
       (define (fromInt x)
         (lisp ,coalton-type (x)
           (cl:values (cl:mod x ,(cl:expt 2 bits))))))

     (define-instance (Into Integer ,coalton-type)
       (define (into x) (fromInt x)))

     ,@(cl:loop :for super :in supers :collecting
          `(define-instance (Into ,coalton-type ,super)
             (define (into x)
               (lisp ,super (x)
                 x))))

     (define-instance (Into ,coalton-type Single-Float)
       (define (into x)
         (lisp Single-Float (x)
           (cl:coerce x 'cl:single-float))))

     (define-instance (Into ,coalton-type Double-Float)
       (define (into x)
         (lisp Double-Float (x)
           (cl:coerce x 'cl:double-float))))))

(%define-unsigned-num-instance U8   8                        Integer U64 I64 U32 I32 U16 I16)
(%define-unsigned-num-instance U16  16                       Integer U64 I64 U32 I32)
(%define-unsigned-num-instance U32  32                       Integer U64 I64)
(%define-unsigned-num-instance U64  64                       Integer)
(%define-unsigned-num-instance UFix #.+unsigned-fixnum-bits+ Integer)

(coalton-toplevel
  (define-instance (Num Integer)
    (define (+ a b)
      (lisp Integer (a b) (cl:+ a b)))
    (define (- a b)
      (lisp Integer (a b) (cl:- a b)))
    (define (* a b)
      (lisp Integer (a b) (cl:* a b)))
    (define (fromInt x)
      x))

  (declare negate (Num :a => :a -> :a))
  (define (negate x)
    (- 0 x))

  (declare abs ((Ord :a) (Num :a) => :a -> :a))
  (define (abs x)
    "Absolute value of X."
    (if (< x 0)
        (negate x)
        x))

  (declare sign ((Ord :a) (Num :a) => :a -> Integer))
  (define (sign x)
    "The sign of X."
    (if (< x 0)
        -1
        1))

  (declare ash (Integer -> Integer -> Integer))
  (define (ash x n)
    "Compute the \"arithmetic shift\" of X by N. "
    (lisp Integer (x n) (cl:ash x n)))

  (declare negative-infinity ((Transfinite :a) (Num :a) => :a))
  (define negative-infinity
    (negate infinity))

  ;; We avoid "Rational" or "Ratio" since those might be a more
  ;; generic concept than a humble fraction of integers. This
  ;; fraction is always assumed to be in reduced terms.

  (declare mkFraction (Integer -> Integer -> Fraction))
  (define (mkFraction a b)
    (lisp Fraction (a b)
      (cl:/ a b)))

  (declare numerator (Fraction -> Integer))
  (define (numerator q)
    "The numerator of a fraction."
    (lisp Integer (q)
      (cl:numerator q)))

  (declare denominator (Fraction -> Integer))
  (define (denominator q)
    "The denominator of a fraction."
    (lisp Integer (q)
      (cl:denominator q)))

  (define-instance (Num Fraction)
    (define (+ p q)
      (lisp Fraction (p q)
        (cl:+ p q)))
    (define (- p q)
      (lisp Fraction (p q)
        (cl:- p q)))
    (define (* p q)
      (lisp Fraction (p q)
        (cl:* p q)))
    (define (fromInt z)
      (lisp Fraction (z) z)))

  (define-instance (Reciprocable Fraction)
    (define (/ a b)
      (lisp Fraction (a b)
        (cl:/ a b)))
    (define (reciprocal q)
      (lisp Fraction (q)
        (cl:/ q))))

  (define-instance (Dividable Integer Fraction)
    (define (general/ x y)
      (mkFraction x y))))

(cl:defun %optional-coerce (z cl-type)
  "Attempts to coerce Z to an Optional CL-TYPE, returns NONE if failed."
  (cl:let ((x (cl:ignore-errors
               (cl:coerce z cl-type))))
    (cl:if (cl:null x)
           None
           (Some x))))

(coalton-toplevel
  (define-instance (Transfinite Single-Float)
    (define infinity
      (lisp Single-Float ()
        float-features:single-float-positive-infinity))
    (define nan
      (lisp Single-Float ()
        float-features:single-float-nan))
    (define (nan? x)
      (Lisp Boolean (x)
        (float-features:float-NaN-p x)))
    (define (infinite? x)
      (Lisp Boolean (x)
        (float-features:float-infinity-p x))))

  (define-instance (Transfinite Double-Float)
    (define infinity
      (lisp Double-Float ()
        float-features:double-float-positive-infinity))
    (define nan
      (lisp Double-Float ()
        float-features:double-float-nan))
    (define (nan? x)
      (Lisp Boolean (x)
        (float-features:float-NaN-p x)))
    (define (infinite? x)
      (Lisp Boolean (x)
        (float-features:float-infinity-p x)))))

(cl:defmacro %define-real-float-arith (coalton-type underlying-type)
  "Defines the arithmetic instances for a lisp floating-point type"
  `(coalton-toplevel
     (define-instance (Num ,coalton-type)
       (define (+ a b)
         (lisp ,coalton-type (a b)
           (float-features:with-float-traps-masked cl:t
             (cl:+ a b))))
       (define (- a b)
         (lisp ,coalton-type (a b)
           (float-features:with-float-traps-masked cl:t
             (cl:- a b))))
       (define (* a b)
         (lisp ,coalton-type (a b)
           (float-features:with-float-traps-masked cl:t
             (cl:* a b))))
       (define (fromInt x)
         (match (lisp (Optional ,coalton-type) (x)
                  (%optional-coerce x ,underlying-type))
           ((Some x) x)
           ((None) (if (< 0 x)
                       negative-infinity
                       infinity)))))

     (define-instance (Into Fraction ,coalton-type)
       (define (into x)
         (general/ (numerator x)
                   (denominator x))))

     (define-instance (Reciprocable ,coalton-type)
       (define (/ x y)
         (lisp ,coalton-type (x y)
           (float-features:with-float-traps-masked cl:t
             (cl:/ x y))))
       (define (reciprocal x)
         (lisp ,coalton-type (x)
           (float-features:with-float-traps-masked cl:t
             (cl:/ x)))))

     (define-instance (Dividable Integer ,coalton-type)
       (define (general/ x y)
         (if (== y 0)
             (/ (fromInt x) (fromInt y))
             (match (lisp (Optional ,coalton-type) (x y)
                      (%optional-coerce (cl:/ x y) ,underlying-type))
               ((Some x) x)
               ((None) (if (and (> x 0) (> y 0))
                           infinity
                           negative-infinity))))))

     (define-instance (TryInto ,coalton-type Fraction)
       (define (tryInto x)
         (if (finite? x)
             (Ok (lisp Fraction (x) (cl:rational x)))
             (Err "Could not convert NaN or infinity into a Fraction"))))))

(%define-real-float-arith Single-Float 'cl:single-float)
(%define-real-float-arith Double-Float 'cl:double-float)

;;;; `Bits' instances
;;; signed

(cl:defmacro define-signed-bit-instance (type handle-overflow)
  (cl:flet ((lisp-binop (op)
              `(lisp ,type (left right)
                 (,op left right))))
    `(coalton-toplevel
       (define-instance (bits:Bits ,type)
         (define (bits:and left right)
           ,(lisp-binop 'cl:logand))
         (define (bits:or left right)
           ,(lisp-binop 'cl:logior))
         (define (bits:xor left right)
           ,(lisp-binop 'cl:logxor))
         (define (bits:not bits)
           (lisp ,type (bits) (cl:lognot bits)))
         (define (bits:shift amount bits)
           (lisp ,type (amount bits)
             (,handle-overflow (cl:ash bits amount))))))))

(define-signed-bit-instance I8 %handle-8bit-overflow)
(define-signed-bit-instance I16 %handle-16bit-overflow)
(define-signed-bit-instance I32 %handle-32bit-overflow)
(define-signed-bit-instance I64 %handle-64bit-overflow)
(define-signed-bit-instance IFix %handle-fixnum-overflow)
(define-signed-bit-instance Integer cl:identity)

;;; unsigned

(cl:declaim (cl:inline unsigned-lognot)
            (cl:ftype (cl:function (cl:unsigned-byte cl:unsigned-byte)
                                   (cl:values cl:unsigned-byte cl:&optional))
                      unsigned-lognot))
(cl:defun unsigned-lognot (int n-bits)
  (cl:- (cl:ash 1 n-bits) int 1))

(cl:declaim (cl:inline handle-unsigned-overflow)
            (cl:ftype (cl:function (cl:unsigned-byte cl:unsigned-byte)
                                   (cl:values cl:unsigned-byte cl:&optional))
                      handle-unsigned-overflow))
(cl:defun handle-unsigned-overflow (int n-bits)
  (cl:logand (cl:1- (cl:ash 1 n-bits))
             int))

(cl:defmacro define-unsigned-bit-instance (type width)
  (cl:flet ((define-binop (coalton-name lisp-name)
              `(define (,coalton-name left right)
                 (lisp ,type (left right)
                   (,lisp-name left right)))))
    `(coalton-toplevel
       (define-instance (bits:Bits ,type)
         ,(define-binop 'bits:and 'cl:logand)
         ,(define-binop 'bits:or 'cl:logior)
         ,(define-binop 'bits:xor 'cl:logxor)
         (define (bits:not bits)
           (lisp ,type (bits) (unsigned-lognot bits ,width)))
         (define (bits:shift amount bits)
           (lisp ,type (amount bits)
             (cl:logand (cl:ash bits amount)
                        (cl:1- (cl:ash 1 ,width)))))))))

(define-unsigned-bit-instance U8 8)
(define-unsigned-bit-instance U16 16)
(define-unsigned-bit-instance U32 32)
(define-unsigned-bit-instance U64 64)
(define-unsigned-bit-instance UFix #.+unsigned-fixnum-bits+)

;;;; `Hash' instances

(define-sxhash-hasher I8)
(define-sxhash-hasher I16)
(define-sxhash-hasher I32)
(define-sxhash-hasher I64)
(define-sxhash-hasher U8)
(define-sxhash-hasher U16)
(define-sxhash-hasher U32)
(define-sxhash-hasher U64)
(define-sxhash-hasher Integer)
(define-sxhash-hasher IFix)
(define-sxhash-hasher UFix)
(define-sxhash-hasher Single-Float)
(define-sxhash-hasher Double-Float)

;;; `Num' extensions
(coalton-toplevel
  (declare 1+ ((Num :num) => :num -> :num))
  (define (1+ num)
    (+ num 1))

  (declare 1- ((Num :num) => :num -> :num))
  (define (1- num)
    (- num 1))

  (declare positive? ((Num :a) (Ord :a) => :a -> Boolean))
  (define (positive? x)
    (> x 0))

  (declare negative? ((Num :a) (Ord :a) => :a -> Boolean))
  (define (negative? x)
    (< x 0))

  (declare nonpositive? ((Num :a) (Ord :a) => :a -> Boolean))
  (define (nonpositive? x)
    (<= x 0))

  (declare nonnegative? ((Num :a) (Ord :a) => :a -> Boolean))
  (define (nonnegative? x)
    (>= x 0))

  (declare nonzero? ((Num :a) => :a -> Boolean))
  (define (nonzero? x)
    (/= x 0)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/ARITH")
