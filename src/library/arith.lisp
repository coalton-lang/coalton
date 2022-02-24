;;;; arith.lisp
;;;;
;;;; Number types and basic arithmetic.

(coalton-library/utils::defstdlib-package #:coalton-library/arith
  (:use
     #:coalton
     #:coalton-library/builtin
     #:coalton-library/classes)
  (:local-nicknames
   (#:bits #:coalton-library/bits))
  (:export
   #:Dividable #:/
   #:Quantization
   #:Quantizable #:quantize
   #:integer->single-float
   #:integer->double-float
   #:single-float->integer
   #:double-float->integer
   #:Fraction
   #:negate
   #:abs
   #:sign
   #:expt
   #:ash
   #:mod
   #:even?
   #:odd?
   #:gcd
   #:lcm
   #:numerator
   #:denominator
   #:reciprocal
   #:complex
   #:real-part
   #:imag-part
   #:conjugate
   #:ii
   #:floor
   #:ceiling
   #:round
   #:save/
   #:exact/
   #:inexact/
   #:floor/
   #:ceiling/
   #:round/
   #:single/
   #:double/
   #:1+
   #:1-))

(cl:in-package #:coalton-library/arith)

(coalton-toplevel
  ;;
  ;; Dividable
  ;;

  (define-class (Dividable :arg-type :res-type)
    "The representation of a type such that division within that type possibly results in another type. For instance,


    (Dividable Integer Fraction)


establishes that division of two `Integer`s can result in a `Fraction`, whereas


    (Dividable Single-Float Single-Float)


establishes that division of two `Single-Float`s can result in a `Single-Float`.

Note that `Dividable` does *not* establish a default result type; you must constrain the result type yourself.

The function / is partial, and will error produce a run-time error if the divisor is zero.
"
    ;; This is a type that is more pragmatic and less mathematical in
    ;; nature. It expresses a division relationship between one input
    ;; type and one output type.
    (/ (:arg-type -> :arg-type -> :res-type)))

  ;;
  ;; Quantizable
  ;;

  (define-type (Quantization :a)
    "Represents an integer quantization of `:a`. See the `Quantizable` typeclass.

The fields are defined as follows:

1. A value of type `:a`.

2. The greatest integer less than or equal to a particular value.

3. The remainder of this as a value of type `:a`.

4. The least integer greater than or equal to a particular value.

5. The remainder of this as a value of type `:a`.
"
    (Quantization :a Integer :a Integer :a))

  (define-class ((Ord :a) (Num :a) => (Quantizable :a))
    "The representation of a type that allows \"quantizing\", \"snapping to integers\", or \"rounding.\" (All of these concepts are roughly equivalent.)
"
    ;; Given a X of type :A, (QUANTIZE X) will return the least
    ;; integer greater or equal to X, and the greatest integer less
    ;; than or equal to X, along with their respective remainders
    ;; expressed as values of type :T.
    (quantize (:a -> (Quantization :a)))))


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
  `(coalton-toplevel
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
              EQ)))))))

(%define-number-stuff U8)
(%define-number-stuff U16)
(%define-number-stuff U32)
(%define-number-stuff U64)
(%define-number-stuff I8)
(%define-number-stuff I16)
(%define-number-stuff I32)
(%define-number-stuff I64)
(%define-number-stuff Integer)
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


(cl:defmacro %define-signed-instances (coalton-type bits)
  (cl:declare (cl:ignore bits))
  `(coalton-toplevel
     (define-instance (Into Integer ,coalton-type)
       (define (into x) (fromInt x)))

     (define-instance (Into ,coalton-type Integer)
       (define (into x)
         (lisp Integer (x)
           x)))))

(%define-signed-instances I8  8)
(%define-signed-instances I16 16)
(%define-signed-instances I32 32)
(%define-signed-instances I64 64)
(%define-signed-instances IFix #.+fixnum-bits+)


(cl:defmacro %define-unsigned-num-instance (coalton-type bits)
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

     (define-instance (Into ,coalton-type Integer)
       (define (into x)
         (lisp Integer (x)
           x)))

     (define-instance (Into ,coalton-type Single-Float)
       (define (into x)
         (lisp Single-Float (x)
           (cl:coerce x 'cl:single-float))))

     (define-instance (Into ,coalton-type Double-Float)
       (define (into x)
         (lisp Double-Float (x)
           (cl:coerce x 'cl:double-float))))))

(%define-unsigned-num-instance U8  8)
(%define-unsigned-num-instance U16 16)
(%define-unsigned-num-instance U32 32)
(%define-unsigned-num-instance U64 64)
(%define-unsigned-num-instance UFix #.+unsigned-fixnum-bits+)

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

  (declare single-float->integer (Single-Float -> (Optional Integer)))
  (define (single-float->integer x)
    "Round a Single-Float to the nearest Integer."
    (lisp (Optional Integer) (x)
      (cl:if (cl:or (float-features:float-infinity-p x)
                    (float-features:float-nan-p x))
             None
             (Some (cl:round x)))))

  (declare double-float->integer (Double-Float -> (Optional Integer)))
  (define (double-float->integer x)
    "Round a Double-Float to the nearest Integer."
    (lisp (Optional Integer) (x)
      (cl:if (cl:or (float-features:float-infinity-p x)
                    (float-features:float-nan-p x))
             None
             (Some (cl:round x))))))

(coalton-toplevel
  (define-type Fraction
    "A ratio of integers always in reduced form."
    ;; We avoid "Rational" or "Ratio" since those might be a more
    ;; generic concept than a humble fraction of integers. This
    ;; fraction is always assumed to be in reduced terms.
    ;;
    ;; This shouldn't be pattern matched against with user code.
    ;;
    ;; See arith.lisp for more info.
    (%Fraction Integer Integer))

  (define-instance (Eq Fraction)
    (define (== p q)
      (and (== (numerator p) (numerator q))
           (== (denominator p) (denominator q)))))

  (define-instance (Ord Fraction)
    (define (<=> p q)
      (<=> (* (numerator p) (denominator q))
           (* (numerator q) (denominator p)))))

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

  (declare negate ((Num :a) => (:a -> :a)))
  (define (negate x)
    (- 0 x))

  (declare abs ((Ord :a) (Num :a) => (:a -> :a)))
  (define (abs x)
    "Absolute value of X."
    (if (< x 0)
        (negate x)
        x))

  (declare sign ((Ord :a) (Num :a) => (:a -> Integer)))
  (define (sign x)
    "The sign of X."
    (if (< x 0)
        -1
        1))

  (declare expt (Integer -> Integer -> Integer))
  (define (expt base power)
    "Exponentiate BASE to a non-negative POWER."
    (if (< power 0)
        (error "Can't exponentiate with a negative exponent.")
        (lisp Integer (base power) (cl:expt base power))))

  (declare ash (Integer -> Integer -> Integer))
  (define (ash x n)
    "Compute the \"arithmetic shift\" of X by N. "
    (lisp Integer (x n) (cl:ash x n)))

  (declare mod (Integer -> Integer -> Integer))
  (define (mod num base)
    "Compute NUM modulo BASE."
    (if (== base 0)
        (error "Can't mod by 0.")
        (lisp Integer (num base) (cl:values (cl:mod num base)))))

  (declare even? (Integer ->  Boolean))
  (define (even? n)
    "Is N even?"
    (lisp Boolean (n) (to-boolean (cl:evenp n))))

  (declare odd? (Integer -> Boolean))
  (define (odd? n)
    "Is N odd?"
    (lisp Boolean (n) (to-boolean (cl:oddp n))))

  (declare gcd (Integer -> Integer -> Integer))
  (define (gcd a b)
    "Compute the greatest common divisor of A and B."
    (lisp Integer (a b) (cl:gcd a b)))

  (declare lcm (Integer -> Integer -> Integer))
  (define (lcm a b)
    "Compute the least common multiple of A and B."
    (lisp Integer (a b) (cl:lcm a b)))

  (declare %reduce-fraction (Fraction -> Fraction))
  (define (%reduce-fraction q)
    (let ((n (numerator q))
          (d (denominator q))
          (g (gcd n d)))
      (if (== 0 n)
          (%Fraction 0 1)
          (%Fraction
           (* (* (sign n) (sign d))
              (lisp Integer (n g) (cl:values (cl:floor n g))))
           (lisp Integer (d g) (cl:values (cl:floor (cl:abs d) g)))))))

  (define (%mkFraction n d)
    (progn
      (when (== 0 d)
        (error "Division by zero"))
      (%reduce-fraction
       (%Fraction n d))))

  (declare numerator (Fraction -> Integer))
  (define (numerator q)
    "The numerator of a fraction."
    (match q
      ((%Fraction n _) n)))

  (declare denominator (Fraction -> Integer))
  (define (denominator q)
    "The denominator of a fraction."
    (match q
      ((%Fraction _ d) d)))

  (declare reciprocal (Fraction -> Fraction))
  (define (reciprocal q)
    "The reciprocal of a fraction."
    (match q
      ;; n/d and d/n will always be reduced
      ((%Fraction n d) (%Fraction d n))))

  (define-instance (Num Fraction)
    (define (+ p q)
      (let ((a (* (numerator p) (denominator q)))
            (b (* (numerator q) (denominator p)))
            (c (* (denominator p) (denominator q))))
        (%mkFraction (+ a b) c)))
    (define (- p q)
      (let ((a (* (numerator p) (denominator q)))
            (b (* (numerator q) (denominator p)))
            (c (* (denominator p) (denominator q))))
        (%mkFraction (- a b) c)))
    (define (* p q)
      (%mkFraction (* (numerator p) (numerator q))
                   (* (denominator p) (denominator q))))
    (define (fromInt z)
      (%Fraction z 1)))

  (define-instance (Dividable Fraction Fraction)
    (define (/ a b)
      (* a (reciprocal b))))

  (define-instance (Dividable Single-Float Single-Float)
    (define (/ x y)
      (lisp Single-Float (x y)
        (cl:/ x y))))

  (define-instance (Dividable Double-Float Double-Float)
    (define (/ x y)
      (lisp Double-Float (x y)
        (cl:/ x y))))

  (define-instance (Dividable Integer Fraction)
    (define (/ x y)
      (%mkFraction x y)))

  (define-instance (Dividable Integer Single-Float)
    (define (/ x y)
      (lisp Single-Float (x y)
        (cl:coerce (cl:/ x y) 'cl:single-float))))

  (define-instance (Dividable Integer Double-Float)
    (define (/ x y)
      (lisp Double-Float (x y)
        (cl:coerce (cl:/ x y) 'cl:double-float)))))


(coalton-toplevel
  (define-instance (Into Integer String)
    (define (into z)
      (lisp String (z)
        (cl:format cl:nil "~D" z))))

  (define-instance (TryInto String Integer)
    (define (tryInto s)
      (lisp (Result String Integer) (s)
        (cl:let ((z (cl:ignore-errors (cl:parse-integer s))))
          (cl:if (cl:null z)
                 (Err "String doesn't have integer syntax.")
                 (Ok z)))))))

;;;; Complex

(coalton-toplevel
  (repr :native (cl:or cl:number complex))
  (define-type (Complex :a)
    (%Complex :a :a))

  (declare real-part ((Complex :a) -> :a))
  (define (real-part n)
    (lisp :a (n)
      (cl:typecase n
        (cl:number (cl:realpart n))
        (complex (complex/%complex-_0 n)))))

  (declare imag-part ((Complex :a) -> :a))
  (define (imag-part n)
    (lisp :a (n)
      (cl:typecase n
        (cl:number (cl:imagpart n))
        (complex (complex/%complex-_1 n)))))

  (define-class (Complex :a)
    (complex (:a -> :a -> (Complex :a))))

  (declare conjugate ((Complex :a) (Num :a) => (Complex :a) -> (Complex :a)))
  (define (conjugate n)
    (complex (real-part n) (negate (imag-part n))))

  (declare ii ((Complex :a) (Num :a) => (Complex :a)))
  (define ii
    "The complex unit i. (The double ii represents a blackboard-bold i.)"
    (complex 0 1))

  (define-instance (Complex (Complex :a))
    (define (complex a b)
      (%Complex a b)))

  (define-instance (Eq (Complex :a) => (Eq (Complex (Complex :a))))
    (define (== a b)
      (and (== (real-part a) (real-part b))
           (== (imag-part a) (imag-part b)))))

  (define-instance (Num (Complex :a) => (Num (Complex (Complex :a))))
    (define (+ a b)
      (%Complex (+ (real-part a) (real-part b))
                (+ (imag-part a) (imag-part b))))
    (define (- a b)
      (%Complex (- (real-part a) (real-part b))
                (- (imag-part a) (imag-part b))))
    (define (* a b)
      (%Complex (* (real-part a) (real-part b))
                 (* (imag-part a) (imag-part b))))
    (define (fromInt n)
      (%Complex (fromInt n) 0))))

(cl:defmacro %define-native-complex-instances (type repr)
  `(coalton-toplevel
     (define-instance (Complex ,type)
       (define (complex a b)
         (lisp (Complex ,type) (a b)
           (cl:declare (cl:type ,repr a b))
           (cl:complex a b))))

     (define-instance (Eq (Complex ,type))
       (define (== a b)
         (lisp Boolean (a b)
           (cl:declare (cl:type (cl:or ,repr (cl:complex ,repr))))
           (cl:= a b))))

     (define-instance (Num (Complex ,type))
       (define (+ a b)
         (lisp (Complex ,type) (a b)
           (cl:declare (cl:type (cl:or ,repr (cl:complex ,repr))))
           (cl:+ a b)))
       (define (- a b)
         (lisp (Complex ,type) (a b)
           (cl:declare (cl:type (cl:or ,repr (cl:complex ,repr))))
           (cl:- a b)))
       (define (* a b)
         (lisp (Complex ,type) (a b)
           (cl:declare (cl:type (cl:or ,repr (cl:complex ,repr))))
           (cl:* a b)))
       (define (fromInt n)
         (lisp (Complex ,type) (n)
           (cl:coerce n ',repr))))

     (define-instance (Dividable (Complex ,type) (Complex ,type))
       (define (/ a b)
         (lisp (Complex ,type) (a b)
           (cl:declare (cl:type (cl:or ,repr (cl:complex ,repr))))
           (cl:/ a b))))

     (define-instance (Into Integer (Complex ,type))
       (define into fromInt))))

(%define-native-complex-instances Integer cl:integer)
(%define-native-complex-instances Single-Float cl:single-float)
(%define-native-complex-instances Double-Float cl:double-float)

(cl:defmacro %define-standard-complex-instances (type)
  `(coalton-toplevel
     (define-instance (Complex ,type)
       (define complex %Complex))

     (define-instance (Eq (Complex ,type))
       (define (== a b)
         (and (== (real-part a) (real-part b))
              (== (imag-part a) (imag-part b)))))

     (define-instance (Num (Complex ,type))
       (define (+ a b)
         (%Complex (+ (real-part a) (real-part b))
                   (+ (imag-part a) (imag-part b))))
       (define (- a b)
         (%Complex (- (real-part a) (real-part b))
                   (- (imag-part a) (imag-part b))))
       (define (* a b)
         (%Complex (* (real-part a) (real-part b))
                   (* (imag-part a) (imag-part b))))
       (define (fromInt n)
         (%Complex (fromInt n) 0)))

     (define-instance (Into Integer (Complex ,type))
       (define into fromInt))))

(%define-standard-complex-instances Fraction)


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


;;; `Quantization'

(coalton-toplevel
  (define-instance (Quantizable Integer)
    (define (quantize x)
      (Quantization x x 0 x 0))))

(cl:macrolet ((define-integer-quantizations (cl:&rest int-types)
                `(coalton-toplevel
                   ,@(cl:loop :for ty :in int-types :collect
                        `(define-instance (Quantizable ,ty)
                           (define (quantize x)
                             (let ((n (into x)))
                               (Quantization x n 0 n 0))))))))
  (define-integer-quantizations I32 I64 U8 U32 U64))

(coalton-toplevel
  (define-instance (Quantizable Single-Float)
    (define (quantize f)
      (lisp (Quantization Single-Float) (f)
        (uiop:nest
         (cl:multiple-value-bind (fl-quo fl-rem) (cl:floor f))
         (cl:multiple-value-bind (ce-quo ce-rem) (cl:ceiling f))
         (Quantization f fl-quo fl-rem ce-quo ce-rem)))))

  (define-instance (Quantizable Double-Float)
    (define (quantize f)
      (lisp (Quantization Double-Float) (f)
        (uiop:nest
         (cl:multiple-value-bind (fl-quo fl-rem) (cl:floor f))
         (cl:multiple-value-bind (ce-quo ce-rem) (cl:ceiling f))
         (Quantization f fl-quo fl-rem ce-quo ce-rem)))))

  (define-instance (Quantizable Fraction)
    (define (quantize q)
      (let ((n (numerator q))
            (d (denominator q)))
        (lisp (Quantization Fraction) (n d)
          ;; Not the most efficient... just relying on CL to do the
          ;; work.
          (cl:flet ((to-frac (f)
                      (%Fraction (cl:numerator f) (cl:denominator f))))
            (cl:let ((f (cl:/ n d)))
              (uiop:nest
               (cl:multiple-value-bind (fl-quo fl-rem) (cl:floor f))
               (cl:multiple-value-bind (ce-quo ce-rem) (cl:ceiling f))
               (Quantization f
                             fl-quo (to-frac fl-rem)
                             ce-quo (to-frac ce-rem)))))))))

  (define (floor x)
    "Return the greatest integer less than or equal to X."
    (match (quantize x)
      ((Quantization _ z _ _ _) z)))

  (define (ceiling x)
    "Return the least integer greater than or equal to X."
    (match (quantize x)
      ((Quantization _ _ _ z _) z)))

  (define (round x)
    "Return the nearest integer to X, with ties breaking toward positive infinity."
    (match (quantize x)
      ((Quantization _ a ar b br)
       (match (<=> (abs ar) (abs br))
         ((LT) a)
         ((GT) b)
         ((EQ) (max a b))))))

  (declare safe/ ((Num :a) (Dividable :a :b) => (:a -> :a -> (Optional :b))))
  (define (safe/ x y)
    "Safely divide X by Y, returning None if Y is zero."
    (if (== y 0)
        None
        (Some (/ x y))))

  (declare exact/ (Integer -> Integer -> Fraction))
  (define (exact/ a b)
    "Exactly divide two integers and produce a fraction."
    (/ a b))

  (declare inexact/ (Integer -> Integer -> Double-Float))
  (define (inexact/ a b)
    "Compute the quotient of integers as a double-precision float.

Note: This does *not* divide double-float arguments."
    (/ a b))

  (declare floor/ (Integer -> Integer -> Integer))
  (define (floor/ a b)
    "Divide two integers and compute the floor of the quotient."
    (floor (exact/ a b)))

  (declare ceiling/ (Integer -> Integer -> Integer))
  (define (ceiling/ a b)
    "Divide two integers and compute the ceiling of the quotient."
    (ceiling (exact/ a b)))

  (declare round/ (Integer -> Integer -> Integer))
  (define (round/ a b)
    "Divide two integers and round the quotient."
    (round (exact/ a b)))

  (declare single/ (Single-Float -> Single-Float -> Single-Float))
  (define (single/ a b)
    "Compute the quotient of single-precision floats as a single-precision float."
    (/ a b))

  (declare double/ (Double-Float -> Double-Float -> Double-Float))
  (define (double/ a b)
    "Compute the quotient of single-precision floats as a single-precision float."
    (/ a b)))

;;; `Num' extensions
(coalton-toplevel
  (declare 1+ ((Num :num) => :num -> :num))
  (define (1+ num)
    (+ num (fromInt 1)))

  (declare 1- ((Num :num) => :num -> :num))
  (define (1- num)
    (- num (fromInt 1))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/ARITH")


