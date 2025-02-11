;;;; complex.lisp
;;;;
;;;; Complex numbers

(coalton-library/utils:defstdlib-package #:coalton-library/math/complex
    (:use #:coalton
          #:coalton-library/classes
          #:coalton-library/utils
          #:coalton-library/math/arith)
  (:local-nicknames
   (#:arith #:coalton-library/math/arith)
   (#:types #:coalton-library/types))
  (:export
   #:complex
   #:real-part
   #:imag-part
   #:conjugate
   #:square-magnitude
   #:ii))

(in-package #:coalton-library/math/complex)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(cl:defvar *native-complex-types* cl:nil
  "A list of Common Lisp types that are native arguments to `cl:complex`.
This list is populated by the macro `%define-native-complex-instances`
below.")

(coalton-toplevel
  ;; The representation of (Complex :t) is specially dealt with by the
  ;; compiler in lisp-type.lisp.
  (define-type (Complex :a)
    "Complex number that may either have a native or constructed representation."
    (%Complex :a :a))

  (define-instance (types:RuntimeRepr :t => types:RuntimeRepr (Complex :t))
    (define (types:runtime-repr a)
      (let ((inner-type (types:runtime-repr (types:proxy-inner a))))
        (lisp types:LispType (inner-type)
          (cl:if (cl:member inner-type *native-complex-types*)
                 `(cl:complex ,inner-type)
                 'Complex))))))

;; Quirk: We had to split the above COALTON-TOPLEVEL from the bottom
;; one because Allegro needs to know about Complex before it gets used
;; as a Lisp type in codegen. SBCL and CCL tolerate it fine.

(coalton-toplevel
  (define-class (Num :a => Complex :a)
    (complex (:a -> :a -> (Complex :a)))
    (real-part (Complex :a -> :a))
    (imag-part (Complex :a -> :a)))

  (define-instance ((Complex :a) => Into :a (Complex :a))
    (define (into a)
      (complex a 0)))

  (declare conjugate ((Complex :a) => Complex :a -> Complex :a))
  (define (conjugate n)
    "The complex conjugate."
    (complex (real-part n) (negate (imag-part n))))

  (declare square-magnitude (Complex :a => Complex :a -> :a))
  (define (square-magnitude a)
    "The length of a complex number."
    (+ (* (real-part a) (real-part a))
       (* (imag-part a) (imag-part a))))

  (declare ii ((Complex :a) => Complex :a))
  (define ii
    "The complex unit i. (The double ii represents a blackboard-bold i.)"
    (complex 0 1))

  (define-instance (Complex :a => Eq (Complex :a))
    (define (== a b) (complex-equal a b)))

  (define-instance (Complex :a => Num (Complex :a))
    (define (+ a b) (complex-plus a b))
    (define (- a b) (complex-minus a b))
    (define (* a b) (complex-times a b))
    (define (fromInt n)
      (complex (fromInt n) 0)))

  ;; BUG: This shouldn't be overlapping
  ;; (define-instance ((Complex :num) (Complex :frac) (Dividable :num :frac)
  ;;                   => (Dividable (Complex :num) (Complex :frac)))
  ;;   (define (general/ a b) (complex-divide a b)))

  (define-instance ((Complex :a) (Reciprocable :a) => Reciprocable (Complex :a))
    (define (reciprocal x)
      (let a = (real-part x))
      (let b = (imag-part x))
      (let divisor = (reciprocal (square-magnitude x)))
      ;; z^-1 = z*/|z|^2
      (complex (* a divisor) (negate (* b divisor))))
    (define (/ a b)
      (complex-divide a b)))

  (define-instance (Complex :a => Complex (Complex :a))
    (define (complex a b)
      (%Complex a b))
    (define (real-part a)
      (match a
        ((%Complex a _) a)))
    (define (imag-part a)
      (match a
        ((%Complex _ b) b))))

  ;; Below are specializable functions, as class methods cannot be specialized
  ;; This allows us to call out to faster lisp functions for doing arithmetic.
  ;; These will only be called from monomorphized forms.
  (define (complex-equal a b)
    (and (== (real-part a) (real-part b))
         (== (imag-part a) (imag-part b))))
  (declare complex-plus ((Complex :a) => Complex :a -> Complex :a -> Complex :a))
  (define (complex-plus a b)
    (complex (+ (real-part a) (real-part b))
             (+ (imag-part a) (imag-part b))))
  (declare complex-minus ((Complex :a) => Complex :a -> Complex :a -> Complex :a))
  (define (complex-minus a b)
    (complex (- (real-part a) (real-part b))
             (- (imag-part a) (imag-part b))))
  (declare complex-times ((Complex :a) => Complex :a -> Complex :a -> Complex :a))
  (define (complex-times a b)
    (let ra = (real-part a))
    (let ia = (imag-part a))
    (let rb = (real-part b))
    (let ib = (imag-part b))
    (complex (- (* ra rb) (* ia ib))
             (+ (* ra ib) (* ia rb))))
  (declare complex-divide ((Complex :a) (Complex :b) (Dividable :a :b)
                           => Complex :a -> Complex :a -> Complex :b))
  (define (complex-divide a b)
    (let dividend = (* a (conjugate b)))
    (let divisor = (square-magnitude b))
    (complex (general/ (real-part dividend) divisor)
             (general/ (imag-part dividend) divisor))))

(cl:defmacro %define-native-complex-instances (type repr)

  (cl:let
      ((conj (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-COMPLEX-CONJUGATE")))
       (equal (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-COMPLEX-EQUAL")))
       (plus (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-COMPLEX-PLUS")))
       (minus (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-COMPLEX-MINUS")))
       (times (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-COMPLEX-TIMES")))
       (divide (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-COMPLEX-DIVIDE"))))

    `(cl:progn
       (cl:pushnew ',repr *native-complex-types* :test 'cl:equal)

       (coalton-toplevel
         (define-instance (Complex ,type)
           (define (complex a b)
             (lisp (Complex ,type) (a b)
               (cl:declare (cl:type ,repr a b))
               (cl:complex a b)))
           (define (real-part a)
             (lisp ,type (a)
               (cl:realpart a)))
           (define (imag-part a)
             (lisp ,type (a)
               (cl:imagpart a))))

         (specialize conjugate ,conj (Complex ,type -> Complex ,type))
         (inline)
         (declare ,conj (Complex ,type -> Complex ,type))
         (define (,conj a)
           (lisp (Complex ,type) (a)
             (cl:declare (cl:type (cl:or ,repr (cl:complex ,repr)) a))
             (cl:conjugate a)))

         (specialize complex-equal ,equal (Complex ,type -> Complex ,type -> Boolean))
         (declare ,equal (Complex ,type -> Complex ,type -> Boolean))
         (define (,equal a b)
           (lisp Boolean (a b)
             (cl:declare (cl:type (cl:or ,repr (cl:complex ,repr)) a b))
             (cl:= a b)))

         (specialize complex-plus ,plus (Complex ,type -> Complex ,type -> Complex ,type))
         (declare ,plus (Complex ,type -> Complex ,type -> Complex ,type))
         (define (,plus a b)
           (lisp (Complex ,type) (a b)
             (cl:declare (cl:type (cl:or ,repr (cl:complex ,repr)) a b))
             (cl:+ a b)))

         (specialize complex-minus ,minus (Complex ,type -> Complex ,type -> Complex ,type))
         (declare ,minus (Complex ,type -> Complex ,type -> Complex ,type))
         (define (,minus a b)
           (lisp (Complex ,type) (a b)
             (cl:declare (cl:type (cl:or ,repr (cl:complex ,repr)) a b))
             (cl:- a b)))

         (specialize complex-times ,times (Complex ,type -> Complex ,type -> Complex ,type))
         (declare ,times (Complex ,type -> Complex ,type -> Complex ,type))
         (define (,times a b)
           (lisp (Complex ,type) (a b)
             (cl:declare (cl:type (cl:or ,repr (cl:complex ,repr)) a b))
             (cl:* a b)))

         (specialize complex-divide ,divide (Complex ,type -> Complex ,type -> Complex ,type))
         (declare ,divide (Complex ,type -> Complex ,type -> Complex ,type))
         (define (,divide a b)
           (lisp (Complex ,type) (a b)
             (cl:declare (cl:type (cl:or ,repr (cl:complex ,repr)) a b))
             (cl:/ a b)))))))

(%define-native-complex-instances U8 (cl:unsigned-byte 8))
(%define-native-complex-instances U16 (cl:unsigned-byte 16))
(%define-native-complex-instances U32 (cl:unsigned-byte 32))
(%define-native-complex-instances U64 (cl:unsigned-byte 64))
(%define-native-complex-instances UFix (cl:unsigned-byte #.coalton-library/math/num::+unsigned-fixnum-bits+))
(%define-native-complex-instances I8 (cl:signed-byte 8))
(%define-native-complex-instances I16 (cl:signed-byte 12))
(%define-native-complex-instances I32 (cl:signed-byte 32))
(%define-native-complex-instances I64 (cl:signed-byte 64))
(%define-native-complex-instances IFix (cl:signed-byte #.coalton-library/math/num::+fixnum-bits+))
(%define-native-complex-instances Integer cl:integer)
(%define-native-complex-instances Single-Float cl:single-float)
(%define-native-complex-instances Double-Float cl:double-float)
(%define-native-complex-instances Fraction cl:rational)

(cl:defmacro %define-standard-complex-instances (type)
  `(coalton-toplevel
     (define-instance (Complex ,type)
       (define (complex a b)
         (%Complex a b))
       (define (real-part a)
         (match a
           ((%Complex a _) a)))
       (define (imag-part a)
         (match a
           ((%Complex _ b) b))))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/COMPLEX")
