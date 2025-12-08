;;;; integral.lisp
;;;;
;;;; Integral domains and operations on integers

(coalton-library/utils::defstdlib-package #:coalton-library/math/integral
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/builtin
   #:coalton-library/math/arith
   #:coalton-compatibility-layer)
  (:import-from
   #:coalton-library/bits #:Bits)
  (:local-nicknames
   (#:bits #:coalton-library/bits)
   (#:compat #:coalton-compatibility-layer))
  (:export
   #:Remainder
   #:Integral
   #:div
   #:mod
   #:divMod
   #:quot
   #:rem
   #:quotRem
   #:toInteger
   #:integral->num
   #:lsh
   #:rsh
   #:even?
   #:odd?
   #:^
   #:^^
   #:gcd
   #:lcm
   #:isqrt
   #:ilog))


(in-package #:coalton-library/math/integral)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (define-class (Num :a => Remainder :a)
    "Remainder is typically an integral domain satisfying:

```
a = (+ (* b (quot a b)) (rem a b))
a = (+ (* b (div a b)) (mod a b))
```
"
    (quot (:a -> :a -> :a))
    (rem (:a -> :a -> :a))
    (quotRem (:a -> :a -> (Tuple :a :a)))
    (div (:a -> :a -> :a))
    (mod (:a -> :a -> :a))
    (divMod (:a -> :a -> (Tuple :a :a))))

  (define-class ((Remainder :int) (Ord :int) => Integral :int)
    "Integral is a number that is either even or odd where `div` and `quot`
are floored and truncated division, respectively."
    (toInteger (:int -> Integer)))

  (inline)
  (declare integral->num ((Integral :a) (Num :b) => :a -> :b))
  (define (integral->num n)
    "Converts any Integral N into any Num."
    (fromInt (toInteger n)))

  (declare rsh ((Integral :n) (Bits :b) => :b -> :n -> :b))
  (define (rsh x n)
    "Right shift X by N"
    (bits:shift (negate (toInteger n)) x))

  (declare lsh ((Integral :n) (Bits :b) => :b -> :n -> :b))
  (define (lsh x n)
    "Left shift X by N"
    (bits:shift (toInteger n) x))

  (declare even? (Integral :a => :a -> Boolean))
  (define (even? n)
    "Is N even?"
    (== 0 (mod n 2)))

  (declare odd? (Integral :a => :a -> Boolean))
  (define (odd? n)
    "Is N odd?"
    (== 1 (mod n 2)))

  (declare ^ ((Num :a) (Integral :int) => (:a -> :int -> :a)))
  (define (^ base power)
    "Exponentiate BASE to a non-negative POWER."
    (let (
          ;; (g x n b) = (* (x ^ n) b)
          (g 
            (fn (a n b)
              (cond
                ((even? n) (g (* a a) (quot n 2) b))
                ((> n 1) (g (* a a) (quot n 2) (* a b)))
                (True (* a b)))))
          ;; (f a n) = (a ^ n)
          (f
            (fn (a n)
              (cond
                ((even? n) (f (* a a) (quot n 2)))
                ((> n 1) (g (* a a) (quot n 2) a))
                (True a)))))
      (cond
        ((> power 3) (f base power))
        ((== power 3) (* (* base base) base))
        ((== power 2) (* base base))
        ((== power 1) base)
        ((== power 0) 1)
        (True (error "Can't exponentiate with a negative exponent.")))))

  (declare ^^ ((Reciprocable :a) (Integral :int) => (:a -> :int -> :a)))
  (define (^^ base power)
    "Exponentiate BASE to a signed POWER."
    (if (< power 0)
        (^ (/ 1 base) (negate power))
        (^ base power)))

  (declare gcd ((Remainder :a) (Ord :a) => :a -> :a -> :a))
  (define (gcd a b)
    "The greatest common divisor of A and B."
    (if (== b 0) a
        (gcd (abs b) (abs (rem a b)))))

  (declare lcm ((Remainder :a) (Ord :a) => :a -> :a -> :a))
  (define (lcm a b)
    "The least common multiple of A and B."
    (if (or (== a 0) (== b 0))
        0
        (* (abs a) (quot (abs b) (gcd a b)))))

  (declare factorial ((Integral :int) => :int -> :int))
  (define (factorial n)
    "The factorial of N."
    (cond
      ((< n 0)
       (error "Cannot FACTORIAL a negative number."))
      (True
       (rec % ((a n) (acc 1))
         (if (== 0 n)
             acc
             (% (- a 1) (* a acc)))))))

  (declare ilog ((Integral :int) => :int -> :int -> :int))
  (define (ilog b x)
    "The floor of the logarithm with base B > 1 of X >= 1."
    ;; See GHC's wordLogBase#
    (let ((ilog-rec 
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
        ((< x 1) (error "Power of ILOG must be greater than or equal to 1."))
        ((<= b 1) (error "Base of ILOG must be greater than 1."))
        (True (match (ilog-rec b) ((Tuple _ b) b))))))

  (declare isqrt ((Integral :int) => :int -> :int))
  (define (isqrt x)
    "The floor of the square root of N > 0."
    (let ((isqrt-rec 
            (fn (x0)
              (let ((x1 (div (+ x0 (div x x0)) 2)))
                (if (<= x0 x1)
                    x0
                    (isqrt-rec x1))))))
      (cond
        ((> x 1) (isqrt-rec (div x 2)))
        ((>= x 0) x)
        (True (error "Cannot take ISQRT of a negative number."))))))

(cl:defmacro %define-remainder-native (type)
  `(coalton-toplevel
     (define-instance (Remainder ,type)
       (inline)
       (define (quot a n)
         (fromInt (lisp Integer (a n) (cl:truncate a n))))
       (inline)
       (define (rem a n)
         (lisp ,type (a n) (cl:rem a n)))
       (inline)
       (define (quotRem a n)
         (match (lisp (Tuple Integer ,type) (a n)
                  (cl:multiple-value-call 'Tuple (cl:truncate a n)))
           ((Tuple q r)
            (Tuple (fromInt q) r))))
       (inline)
       (define (div a n)
         (fromInt (lisp Integer (a n) (cl:floor a n))))
       (inline)
       (define (mod a n)
         (lisp ,type (a n) (cl:mod a n)))
       (inline)
       (define (divMod a n)
         (match (lisp (Tuple Integer ,type) (a n)
                  (cl:multiple-value-call 'Tuple (cl:floor a n)))
           ((Tuple d m)
            (Tuple (fromInt d) m)))))))

(cl:defmacro %define-integral-native (type signed)
  (cl:let ((even? (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-EVEN?")))
           (odd? (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-ODD?")))
           (gcd (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-GCD")))
           (^ (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-^")))
           (^^ (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-^^")))
           (lcm (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-LCM")))
           (isqrt (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-ISQRT"))))
    `(cl:progn 
       (%define-remainder-native ,type)
       
       (coalton-toplevel
         (define-instance (Integral ,type)
           (inline)
           (define (toInteger x) (into x)))

         (specialize even? ,even? (,type -> Boolean))
         (inline)
         (declare ,even? (,type -> Boolean))
         (define (,even? n)
           (lisp Boolean (n) (to-boolean (cl:evenp n))))

         (specialize odd? ,odd? (,type -> Boolean))
         (inline)
         (declare ,odd? (,type -> Boolean))
         (define (,odd? n)
           (lisp Boolean (n) (to-boolean (cl:oddp n))))

         (specialize ^ ,^ (,type -> ,type -> ,type))
         (declare ,^ (,type -> ,type -> ,type))
         ,(cl:cond
            (signed
             `(define (,^ base power)
                (if (< power 0)
                    (error "Can't exponentiate with a negative exponent.")
                    (lisp ,type (base power) (cl:expt base power)))))
            (cl:t
             `(progn
                (inline)
                (define (,^ base power)
                  (lisp ,type (base power) (cl:expt base power))))))

         (specialize ^^ ,^^ (,type -> ,type -> ,type))
         (inline)
         (declare ,^^ (,type -> ,type -> ,type))
         (define (,^^ base power)
           (lisp ,type (base power) (cl:expt base power)))

         (specialize gcd ,gcd (,type -> ,type -> ,type))
         (inline)
         (declare ,gcd (,type -> ,type -> ,type))
         (define (,gcd a b)
           (lisp ,type (a b) (cl:gcd a b)))

         (specialize lcm ,lcm (,type -> ,type -> ,type))
         (inline)
         (declare ,lcm (,type -> ,type -> ,type))
         (define (,lcm a b)
           ;; Allow Coalton to handle fixnum overflow
           (fromInt (lisp Integer (a b) (cl:lcm a b))))

         (specialize isqrt ,isqrt (,type -> ,type))
         (declare ,isqrt (,type -> ,type))
         ,(cl:cond
            (signed
             `(define (,isqrt a)
                (if (< a 0)
                    (error "Can't take ISQRT of a negative number.")
                    (lisp ,type (a) (cl:isqrt a)))))
            (cl:t
             `(progn
                (inline)
                (define (,isqrt a)
                  (lisp ,type (a) (cl:isqrt a))))))))))

(%define-integral-native Integer cl:t)
(%define-integral-native I8 cl:t)
(%define-integral-native I16 cl:t)
(%define-integral-native I32 cl:t)
(%define-integral-native I64 cl:t)
(%define-integral-native IFix cl:t)
(%define-integral-native Bit cl:nil)
(%define-integral-native U8 cl:nil)
(%define-integral-native U16 cl:nil)
(%define-integral-native U32 cl:nil)
(%define-integral-native U64 cl:nil)
(%define-integral-native UFix cl:nil)
(%define-remainder-native Fraction)
(%define-remainder-native F32)
(%define-remainder-native F64)

(cl:defmacro %define-native-expt (type)
  (cl:let ((^ (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-^")))
           (^^ (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-^^"))))

    `(coalton-toplevel
       (specialize ^ ,^ (,type -> Integer -> ,type))
       (declare ,^ (,type -> Integer -> ,type))
       (define (,^ base power)
         (if (< power 0)
             (error "Can't exponentiate with a negative exponent.")
             (lisp ,type (base power) (cl:expt base power))))

       (specialize ^^ ,^^ (,type -> Integer -> ,type))
       (inline)
       (declare ,^^ (,type -> Integer -> ,type))
       (define (,^^ base power)
         (lisp ,type (base power) (cl:expt base power))))))

(%define-native-expt Fraction)
(%define-native-expt F32)
(%define-native-expt F64)

(compat:try-lock-package "COALTON-LIBRARY/MATH/INTEGRAL")
