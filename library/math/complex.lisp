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
   #:Complex                            ; data type
   #:ComplexComponent                   ; type class
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
    "A complex number with a real and imaginary component.

This object does not have any public constructors. Instead, use the
function `complex` of the `ComplexComponent` type class.

A `Complex` object may either have a native or constructed
representation. See the `ComplexComponent` type class for allowed
component types."
    (%Complex :a :a))

  (define-instance (types:RuntimeRepr :t => types:RuntimeRepr (Complex :t))
    (define (types:runtime-repr a)
      (let ((inner-type (types:runtime-repr (types:proxy-inner a))))
        (lisp types:LispType (inner-type)
          (cl:if (cl:member inner-type *native-complex-types*)
                 `(cl:complex ,inner-type)
                 'Complex))))
    (define (types:coalton-type-string _)
      ;; FIXME!!
      "Complex")))

;; Quirk: We had to split the above COALTON-TOPLEVEL from the bottom
;; one because Allegro needs to know about Complex before it gets used
;; as a Lisp type in codegen. SBCL and CCL tolerate it fine.

(coalton-toplevel
  (define-class (Num :a => ComplexComponent :a)
    "A type class for describing complex component types. This type class
also encodes the construction and projection of `Complex` data types."
    (complex (:a -> :a -> Complex :a))
    (real-part (Complex :a -> :a))
    (imag-part (Complex :a -> :a)))

  (define-instance (ComplexComponent :a => Into :a (Complex :a))
    (inline)
    (define (into a)
      (complex a 0)))

  (inline)
  (declare conjugate (ComplexComponent :a => Complex :a -> Complex :a))
  (define (conjugate z)
    "The complex conjugate. If $z=a+bi$ then the conjugate $\\bar z=a-bi$."
    (complex (real-part z) (negate (imag-part z))))

  (inline)
  (declare square-magnitude (ComplexComponent :a => Complex :a -> :a))
  (define (square-magnitude z)
    "The squared length of a complex number:
$$\\vert z\\vert^2=(\\operatorname{Re} z)^2+(\\operatorname{Im} z)^2.$$"
    (let ((r (real-part z))
          (i (imag-part z)))
       (+ (* r r) (* i i))))

  (declare ii (ComplexComponent :a => Complex :a))
  (define ii
    "The complex unit $i=\\sqrt{-1}$. (The double `ii` represents a
blackboard-bold 𝕚.)"
    (complex 0 1))

  (define-instance (ComplexComponent :a => Eq (Complex :a))
    (inline)
    (define (== a b) (complex-equal a b)))

  (define-instance (ComplexComponent :a => Num (Complex :a))
    (inline)
    (define (+ a b) (complex-plus a b))
    (inline)
    (define (- a b) (complex-minus a b))
    (inline)
    (define (* a b) (complex-times a b))
    (inline)
    (define (fromInt n) (complex-fromint n)))

  ;; BUG: This shouldn't be overlapping
  ;; (define-instance ((Complex :num) (Complex :frac) (Dividable :num :frac)
  ;;                   => (Dividable (Complex :num) (Complex :frac)))
  ;;   (define (general/ a b) (complex-divide a b)))

  (define-instance ((ComplexComponent :a) (Reciprocable :a) => Reciprocable (Complex :a))
    (inline)
    (define (reciprocal x)
      (complex-reciprocal x))
    (inline)
    (define (/ a b)
      (complex-divide a b)))

  ;; (define-instance (ComplexComponent :a => ComplexComponent (Complex :a))
  ;;   (define (complex a b)
  ;;     (%Complex a b))
  ;;   (define (real-part a)
  ;;     (match a
  ;;       ((%Complex a _) a)))
  ;;   (define (imag-part a)
  ;;     (match a
  ;;       ((%Complex _ b) b))))

  ;; Below are specializable functions, as class methods cannot be specialized
  ;; This allows us to call out to faster lisp functions for doing arithmetic.
  ;; These will only be called from monomorphized forms.
  (declare complex-equal (ComplexComponent :a => Complex :a -> Complex :a -> Boolean))
  (define (complex-equal a b)
    (and (== (real-part a) (real-part b))
         (== (imag-part a) (imag-part b))))

  (declare complex-plus (ComplexComponent :a => Complex :a -> Complex :a -> Complex :a))
  (define (complex-plus a b)
    (complex (+ (real-part a) (real-part b))
             (+ (imag-part a) (imag-part b))))

  (declare complex-minus (ComplexComponent :a => Complex :a -> Complex :a -> Complex :a))
  (define (complex-minus a b)
    (complex (- (real-part a) (real-part b))
             (- (imag-part a) (imag-part b))))

  (declare complex-times (ComplexComponent :a => Complex :a -> Complex :a -> Complex :a))
  (define (complex-times a b)
    (let ra = (real-part a))
    (let ia = (imag-part a))
    (let rb = (real-part b))
    (let ib = (imag-part b))
    (complex (- (* ra rb) (* ia ib))
             (+ (* ra ib) (* ia rb))))

  (declare complex-fromint (ComplexComponent :a => Integer -> Complex :a))
  (define (complex-fromint n)
    (complex (fromint n) 0))

  (declare complex-reciprocal ((ComplexComponent :a) (Reciprocable :a) => Complex :a -> Complex :a))
  (define (complex-reciprocal x)
    (let a = (real-part x))
    (let b = (imag-part x))
    (let divisor = (reciprocal (square-magnitude x)))
    ;; z^-1 = z*/|z|^2
    (complex (* a divisor) (negate (* b divisor))))

  (declare complex-divide ((ComplexComponent :a)
                           (ComplexComponent :b)
                           (Dividable :a :b)
                           => Complex :a -> Complex :a -> Complex :b))
  (define (complex-divide a b)
    (let dividend = (* a (conjugate b)))
    (let divisor = (square-magnitude b))
    (complex (general/ (real-part dividend) divisor)
             (general/ (imag-part dividend) divisor))))

(cl:defmacro %define-native-complex-instances (type repr cl:&key (division cl:t))

  (cl:let
      ((conj (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-COMPLEX-CONJUGATE")))
       (equal (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-COMPLEX-EQUAL")))
       (plus (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-COMPLEX-PLUS")))
       (minus (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-COMPLEX-MINUS")))
       (times (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-COMPLEX-TIMES")))
       (cfromint (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-COMPLEX-FROMINT")))
       (divide (cl:if (cl:not division) cl:nil (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-COMPLEX-DIVIDE"))))
       (recip (cl:if (cl:not division) cl:nil (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-COMPLEX-RECIPROCAL")))))

    `(cl:progn
       (cl:pushnew ',repr *native-complex-types* :test 'cl:equal)

       (coalton-toplevel
         (define-instance (ComplexComponent ,type)
           (inline)
           (define (complex a b)
             (lisp (Complex ,type) (a b)
               (cl:declare (cl:type ,repr a b))
               (cl:complex a b)))
           (inline)
           (define (real-part a)
             (lisp ,type (a)
               (cl:realpart a)))
           (inline)
           (define (imag-part a)
             (lisp ,type (a)
               (cl:imagpart a))))

         (specialize conjugate ,conj (Complex ,type -> Complex ,type))
         (inline)
         (declare ,conj (Complex ,type -> Complex ,type))
         (define (,conj a)
           (lisp (Complex ,type) (a)
             (cl:declare (cl:type (cl:complex ,repr) a))
             (cl:conjugate a)))

         (specialize complex-equal ,equal (Complex ,type -> Complex ,type -> Boolean))
         (inline)
         (declare ,equal (Complex ,type -> Complex ,type -> Boolean))
         (define (,equal a b)
           (lisp Boolean (a b)
             (cl:declare (cl:type (cl:complex ,repr) a b))
             (cl:= a b)))

         (specialize complex-plus ,plus (Complex ,type -> Complex ,type -> Complex ,type))
         (inline)
         (declare ,plus (Complex ,type -> Complex ,type -> Complex ,type))
         (define (,plus a b)
           (lisp (Complex ,type) (a b)
             (cl:declare (cl:type (cl:complex ,repr) a b))
             (cl:+ a b)))

         (specialize complex-minus ,minus (Complex ,type -> Complex ,type -> Complex ,type))
         (inline)
         (declare ,minus (Complex ,type -> Complex ,type -> Complex ,type))
         (define (,minus a b)
           (lisp (Complex ,type) (a b)
             (cl:declare (cl:type (cl:complex ,repr) a b))
             (cl:- a b)))

         (specialize complex-times ,times (Complex ,type -> Complex ,type -> Complex ,type))
         (inline)
         (declare ,times (Complex ,type -> Complex ,type -> Complex ,type))
         (define (,times a b)
           (lisp (Complex ,type) (a b)
             (cl:declare (cl:type (cl:complex ,repr) a b))
             (cl:* a b)))

         (specialize complex-fromint ,cfromint (Integer -> Complex ,type))
         (inline)
         (declare ,cfromint (Integer -> Complex ,type))
         (define (,cfromint n)
           (let ((f (the ,type (fromint n))))
             (lisp (Complex ,type) (f)
               (cl:complex f ,(cl:coerce 0 repr)))))

         ,@(cl:if
            (cl:not division)
            cl:nil
            `((specialize complex-divide ,divide (Complex ,type -> Complex ,type -> Complex ,type))
              (inline)
              (declare ,divide (Complex ,type -> Complex ,type -> Complex ,type))
              (define (,divide a b)
                (lisp (Complex ,type) (a b)
                  (cl:declare (cl:type (cl:complex ,repr) a b))
                  (cl:/ a b)))

              (specialize complex-reciprocal ,recip (Complex ,type -> Complex ,type))
              (inline)
              (declare ,recip (Complex ,type -> Complex ,type))
              (define (,recip a)
                (lisp (Complex ,type) (a)
                  (cl:declare (cl:type (cl:complex ,repr) a))
                  (cl:/ a)))))))))

(%define-native-complex-instances F32 cl:single-float)
(%define-native-complex-instances F64 cl:double-float)
(%define-native-complex-instances Fraction cl:rational)
(%define-native-complex-instances Integer cl:integer :division cl:nil)


(cl:defmacro %define-standard-complex-instances (type)
  `(coalton-toplevel
     (define-instance (ComplexComponent ,type)
       (inline)
       (define (complex a b)
         (%Complex a b))
       (inline)
       (define (real-part a)
         (match a
           ((%Complex a _) a)))
       (inline)
       (define (imag-part a)
         (match a
           ((%Complex _ b) b))))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/COMPLEX")
