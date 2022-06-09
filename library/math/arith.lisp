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
   #:integer->single-float
   #:integer->double-float
   #:single-float->integer
   #:double-float->integer
   #:negate
   #:abs
   #:sign
   #:ash
   #:mkFraction
   #:numerator
   #:denominator
   #:Trigonometric
   #:sin #:cos #:tan
   #:sinh #:cosh #:tanh
   #:asin #:acos #:atan
   #:asinh #:acosh #:atanh
   #:sincos
   #:Exponentiable
   #:log #:expt #:sqrt #:exp #:ln
   #:Float
   #:pi #:ee
   #:atan2
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
    (define (general/ a b) (/ a b))))

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
  (declare integer->single-float (Integer -> Single-Float))
  (define (integer->single-float z)
    (lisp Single-Float (z)
      (cl:let ((x (cl:ignore-errors
                   (cl:coerce z 'cl:single-float))))
        (cl:if (cl:null x)
               float-features:single-float-nan
               x))))

  (declare integer->double-float (Integer -> Double-Float))
  (define (integer->double-float z)
    (lisp Double-Float (z)
      (cl:let ((x (cl:ignore-errors
                   (cl:coerce z 'cl:double-float))))
        (cl:if (cl:null x)
               float-features:double-float-nan
               x))))

  (declare single-float->integer (Single-Float -> Optional Integer))
  (define (single-float->integer x)
    "Round a Single-Float to the nearest Integer."
    (lisp (Optional Integer) (x)
      (cl:if (cl:or (float-features:float-infinity-p x)
                    (float-features:float-nan-p x))
             None
             (Some (cl:round x)))))

  (declare double-float->integer (Double-Float -> Optional Integer))
  (define (double-float->integer x)
    "Round a Double-Float to the nearest Integer."
    (lisp (Optional Integer) (x)
      (cl:if (cl:or (float-features:float-infinity-p x)
                    (float-features:float-nan-p x))
             None
             (Some (cl:round x)))))

  (define-instance (Num Integer)
    (define (+ a b)
      (lisp Integer (a b) (cl:+ a b)))
    (define (- a b)
      (lisp Integer (a b) (cl:- a b)))
    (define (* a b)
      (lisp Integer (a b) (cl:* a b)))
    (define (fromInt x)
      x))

  (define-instance (Num Single-Float)
    (define (+ a b)
      (lisp Single-Float (a b) (cl:+ a b)))
    (define (- a b)
      (lisp Single-Float (a b) (cl:- a b)))
    (define (* a b)
      (lisp Single-Float (a b) (cl:* a b)))
    (define (fromInt x)
      (integer->single-float x)))

  (define-instance (Num Double-Float)
    (define (+ a b)
      (lisp Double-Float (a b) (cl:+ a b)))
    (define (- a b)
      (lisp Double-Float (a b) (cl:- a b)))
    (define (* a b)
      (lisp Double-Float (a b) (cl:* a b)))
    (define (fromInt x)
      (integer->double-float x)))

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
    (lisp Integer (x n) (cl:ash x n))))

(coalton-toplevel
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
      (lisp Fraction (z) z))))

(coalton-toplevel
  (define-instance (Reciprocable Fraction)
    (define (/ a b)
      (lisp Fraction (a b)
        (cl:/ a b)))
    (define (reciprocal q)
      (lisp Fraction (q)
        (cl:/ q))))

  (define-instance (Reciprocable Single-Float)
    (define (/ x y)
      (lisp Single-Float (x y)
        (cl:/ x y)))
    (define (reciprocal x)
      (lisp Single-Float (x)
        (cl:/ x))))

  (define-instance (Reciprocable Double-Float)
    (define (/ x y)
      (lisp Double-Float (x y)
        (cl:/ x y)))
    (define (reciprocal x)
      (lisp Double-Float (x)
        (cl:/ x))))

  (define-instance (Dividable Integer Fraction)
    (define (general/ x y)
      (mkFraction x y)))

  (define-instance (Dividable Integer Single-Float)
    (define (general/ x y)
      (lisp Single-Float (x y)
        (cl:coerce (cl:/ x y) 'cl:single-float))))

  (define-instance (Dividable Integer Double-Float)
    (define (general/ x y)
      (lisp Double-Float (x y)
        (cl:coerce (cl:/ x y) 'cl:double-float)))))

(coalton-toplevel
  (define-class (Trigonometric :a)
    (sin   (:a -> :a))
    (cos   (:a -> :a))
    (tan   (:a -> :a))
    (asin  (:a -> :a))
    (acos  (:a -> :a))
    (atan  (:a -> :a)))

  (declare sincos (Trigonometric :a => :a -> (Tuple :a :a)))
  (define (sincos x)
    (Tuple (sin x) (cos x)))

  (define-class (Exponentiable :a)
    (expt (:a -> :a -> :a))
    (log  (:a -> :a -> :a)))

  (declare sqrt ((Into Fraction :a) (Exponentiable :a) => :a -> :a))
  (define (sqrt x)
    "Returns the square root of the input"
    (expt x (into 1/2)))

  (define-class ((Into Fraction :a) (Reciprocable :a) (Trigonometric :a) (Exponentiable :a)
                 => Float :a)
    "A finite, floating-point approximation of a real or imaginary number"
    (ee :a)
    (pi :a))

  (declare exp ((Float :f) => :f -> :f))
  (define exp
    "Raise ee to the power of X"
    (expt ee))

  (declare ln ((Float :f) => :f -> :f))
  (define ln
    "The natural logarithm of a given value"
    (log ee))

  ;; See http://clhs.lisp.se/Body/f_sinh_.htm

  (declare sinh ((Float :f) => :f -> :f))
  (define (sinh x)
    (/ (- (exp x) (exp (negate x))) 2))

  (declare cosh ((Float :f) => :f -> :f))
  (define (cosh x)
    (/ (+ (exp x) (exp (negate x))) 2))

  (declare tanh ((Float :f) => :f -> :f))
  (define (tanh x)
    (/ (sinh x) (cosh x)))

  (declare asinh ((Float :f) => :f -> :f))
  (define (asinh x)
    (ln (+ x (sqrt (+ 1 (expt x 2))))))

  (declare acosh ((Float :f) => :f -> :f))
  (define (acosh x)
    (* 2 (ln (+ (sqrt (/ (+ x 1) 2)) (sqrt (/ (- x 1) 2))))))

  (declare atanh ((Float :f) => :f -> :f))
  (define (atanh x)
    (/ (- (ln (+ 1 x)) (ln (- 1 x))) (fromInt 2)))

  (declare atan2 ((Ord :f) (Float :f) => :f -> :f -> :f))
  (define (atan2 y x)
    "Computes the two-argument arctangent of y and x, which is roughly the same as (atan (/ y x)) when defined and accounting for the quadrant of the point (x, y)."
    (match (Tuple (<=> x 0) (<=> y 0))
      ((Tuple (GT) _)    (atan (/ y x)))
      ((Tuple (LT) (LT)) (- (atan (/ y x)) pi))
      ((Tuple (LT) _)    (+ (atan (/ y x)) pi))
      ((Tuple (EQ) (GT)) (/ pi (fromint 2)))
      ((Tuple (EQ) (LT)) (/ pi (fromint -2)))
      ((Tuple (EQ) (EQ)) 0))))


(cl:defmacro %define-real-float-instance (coalton-type underlying-type)
  "Defines the float instances for a lisp floating-point type"
  `(coalton-toplevel
     (define-instance (Trigonometric ,coalton-type)
       ,(generate-unary-wrapper coalton-type 'sin  'cl:sin)
       ,(generate-unary-wrapper coalton-type 'cos  'cl:cos)
       ,(generate-unary-wrapper coalton-type 'tan  'cl:tan)
       ,(generate-unary-wrapper coalton-type 'asin 'cl:asin :domain '(cl:lambda (x) (cl:<= -1 x 1)))
       ,(generate-unary-wrapper coalton-type 'acos 'cl:acos :domain '(cl:lambda (x) (cl:<= -1 x 1)))
       ,(generate-unary-wrapper coalton-type 'atan 'cl:atan))

     (define-instance (Exponentiable ,coalton-type)
       (define (expt base power)
         (lisp ,coalton-type (base power)
           (cl:when (cl:minusp base)
             (cl:error "Cannot exponentiate with a negative base ~a" base))
           (cl:unless (cl:or (cl:plusp base) (cl:plusp power))
             (cl:error "Either the base (~a) or the power (~a) must be positive" base power))
           (cl:expt base power)))
       (define (log base number)
         (lisp ,coalton-type (base number)
           (cl:when (cl:minusp base)
             (cl:error "Cannot take a logarithm with the non-positive real base ~a" base))
           (cl:when (cl:minusp number)
             (cl:error "Cannot take the logarithm of the non-positive real number ~a" number))
           (cl:log number base))))

     (define-instance (Into Fraction ,coalton-type)
       (define (into x)
         (lisp ,coalton-type (x)
           (cl:coerce x ',underlying-type))))
     
     (define-instance (Float ,coalton-type)
       (define ee
         (lisp ,coalton-type ()
           (cl:exp (cl:coerce 1 ',underlying-type))))

       (define pi
         (lisp ,coalton-type ()
           (cl:coerce cl:pi ',underlying-type))))))

(%define-real-float-instance Single-Float cl:single-float)
(%define-real-float-instance Double-Float cl:double-float)

(cl:defmacro %define-integer-expt-instance (coalton-type)
  `(coalton-toplevel
     (define-instance (Exponentiable ,coalton-type)
       (define (expt base power)
         (lisp ,coalton-type (base power)
           (cl:when (cl:minusp power)
             (cl:error "Cannot exponentiate to the power of the negative integer ~a" power))
           (cl:expt base power)))
       (define (log b x)
         ;; The floor of the logarithm with base B > 1 of X >= 1.
         ;; See GHC's wordLogBase#
         (let ((quot 
                  (fn (n m)
                    (lisp ,coalton-type (n m)
                      (cl:values (cl:truncate n m)))))

               (ilog-rec 
                 (fn (y)
                   (if (< x y)
                       (the (Tuple :a :a) (Tuple x 0))
                       (match (ilog-rec (* y y))
                         ((Tuple a b)
                          (if (< a y)
                              (Tuple a (* 2 b))
                              (Tuple (quot a y) (+ (* 2 b) 1)))))))))
         (cond
           ((== x 1) 0)
           ((< x 1)  (error "Power of ILOG must be greater than or equal to 1."))
           ((<= b 1) (error "Base of ILOG must be greater than 1."))
           ((== b 2) (- (lisp ,coalton-type (x) (cl:integer-length x)) 1))
           (True     (match (ilog-rec b) ((Tuple _ b) b)))))))))

(%define-integer-expt-instance Integer)
(%define-integer-expt-instance U8) 
(%define-integer-expt-instance I8) 
(%define-integer-expt-instance U16) 
(%define-integer-expt-instance I16) 
(%define-integer-expt-instance U32) 
(%define-integer-expt-instance I32) 
(%define-integer-expt-instance U64) 
(%define-integer-expt-instance I64) 
(%define-integer-expt-instance Ifix) 
(%define-integer-expt-instance Ufix) 

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
