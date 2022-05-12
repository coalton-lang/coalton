;;;; integral.lisp
;;;;
;;;; Integral domains and operations on integers

(coalton-library/utils::defstdlib-package #:coalton-library/integral
    (:use
     #:coalton
     #:coalton-library/classes
     #:coalton-library/builtin
     #:coalton-library/arith)
  (:import-from
   #:coalton-library/bits
   #:Bits)
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
   #:even?
   #:odd?
   #:^
   #:^^
   #:gcd
   #:lcm
   #:isqrt
   #:ilog))

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(cl:in-package #:coalton-library/integral)

(coalton-toplevel
  (define-class (Num :a => Remainder :a)
    "Remainder is typically an integral domain satisfying:
    a = (+ (* b (quot a b)) (rem a b))
    a = (+ (* b (div a b)) (mod a b))"
    (quot (:a -> :a -> :a))
    (rem (:a -> :a -> :a))
    (quotRem (:a -> :a -> (Tuple :a :a)))
    (div (:a -> :a -> :a))
    (mod (:a -> :a -> :a))
    (divMod (:a -> :a -> (Tuple :a :a))))

  (define-class ((Remainder :int) (Ord :int) => (Integral :int))
    "Integral is a number that is either even or odd where `div' and `quot'
are floored and truncated division, respectively."
    (toInteger (:int -> Integer)))

  (declare factorial ((Integral :int) => :int -> :int))
  (define (factorial n)
    "The factorial of N."
    (let ((declare factorial-rec ((Integral :a) => :a -> :a))
          (factorial-rec
            (fn (a)
              (if (> a 0)
                  (* a (factorial-rec (- a 1)))
                  1))))
      (if (< n 0)
          (error "Cannot FACTORIAL a negative number.")
          (factorial-rec n))))

  (declare ilog ((Integral :int) => :int -> :int -> :int))
  (define (ilog b x)
    "The floor of the logarithm with base B > 1 of X >= 1."
    ;; See GHC's wordLogBase#
    (let ilog-rec =
      (fn (y)
        (if (< x y)
            (the (Tuple :a :a) (Tuple x 0))
            (match (ilog-rec (* y y))
              ((Tuple a b)
               (if (< a y)
                   (Tuple a (* 2 b))
                   (Tuple (quot a y) (+ (* 2 b) 1))))))))
    (cond
      ((== x 1) 0)
      ((< x 1) (error "Power of ILOG must be greater than or equal to 1."))
      ((<= b 1) (error "Base of ILOG must be greater than 1."))
      (True (match (ilog-rec b) ((Tuple _ b) b)))))

  (declare isqrt ((Integral :int) => :int -> :int))
  (define (isqrt x)
    "The floor of the square root of N > 0."
    (let isqrt-rec =
      (fn (a)
        (let b = (quot (+ (* a a) x) (* 2 a)))
        (if (> a b)
            (isqrt-rec b)
            a)))
    (cond
      ((> x 1) (isqrt-rec x))
      ((== x 1) 1)
      ((== x 0) 0)
      ((< x 0) (error "Cannot take ISQRT of a negative number.")))))

(cl:defmacro %define-integral-native (type signed)
  (cl:let ((ilog (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-ILOG")))
           (isqrt (cl:intern (cl:concatenate 'cl:string (cl:symbol-name type) "-ISQRT"))))
    `(coalton-toplevel
       (define-instance (Remainder ,type)
         (define (quot a n)
           (lisp ,type (a n)
             (cl:nth-value 0 (cl:truncate a n))))
         (define (rem a n)
           (lisp ,type (a n) (cl:rem a n)))
         (define (quotRem a n)
           (lisp (Tuple ,type ,type) (a n)
             (cl:multiple-value-call 'Tuple (cl:truncate a n))))
         (define (mod a n)
           (lisp ,type (a n) (cl:mod a n)))
         (define (div a n)
           (lisp ,type (a n)
             (cl:nth-value 0 (cl:floor a n))))
         (define (divMod a n)
           (lisp (Tuple ,type ,type) (a n)
             (cl:multiple-value-call 'Tuple (cl:floor a n)))))

       (define-instance (Integral ,type)
         (define toInteger into))

       (specialize isqrt ,isqrt (,type -> ,type))
       (declare ,isqrt (,type -> ,type))
       (define (,isqrt a)
         ,(cl:if signed
                 `(if (< a 0)
                      (error "Can't take ISQRT of a negative number.")
                      (lisp ,type (a) (cl:isqrt a)))
                 `(lisp ,type (a) (cl:isqrt a))))

       (specialize ilog ,ilog (,type -> ,type -> ,type))
       (declare ,ilog (,type -> ,type -> ,type))
       (define (,ilog b x)
         (cond
           ((== b 2) (- (lisp :a (x) (cl:integer-length x)) 1))
           (True (ilog b x)))))))

(%define-integral-native Integer t)
(%define-integral-native I8 t)
(%define-integral-native I16 t)
(%define-integral-native I32 t)
(%define-integral-native I64 t)
(%define-integral-native IFix t)
(%define-integral-native U8 nil)
(%define-integral-native U16 nil)
(%define-integral-native U32 nil)
(%define-integral-native U64 nil)
(%define-integral-native UFix nil)

(coalton-toplevel

  (declare even? (Integer -> Boolean))
  (define (even? n)
    "Is N even?"
    (== 0 (rem n 2)))

  (declare odd? (Integer -> Boolean))
  (define (odd? n)
    "Is N odd?"
    (not (even? n)))

  (declare gcd (Integer -> Integer -> Integer))
  (define (gcd a b)
    "The greatest common divisor of A and B."
    (if (== b 0) a
        (gcd (abs b) (abs (rem a b)))))

  (declare lcm (Integer -> Integer -> Integer))
  (define (lcm a b)
    "The least common multiple of A and B."
    (if (or (== a 0) (== b 0))
        0
        (* (abs a) (quot (abs b) (gcd a b)))))

  (declare ^ ((Num :a) => (:a -> Integer -> :a)))
  (define (^ base power)
    "Exponentiate BASE to a non-negative POWER."
    (let ((declare g (Num :a => :a -> Integer -> :a -> :a))
          ;; (g x n b) = (* (x ^ n) b)
          (g (fn (a n b)
               (cond
                 ((even? n) (g (* a a) (quot n 2) b))
                 ((> n 1) (g (* a a) (quot n 2) (* a b)))
                 (True (* a b)))))
          (declare f (Num :a => :a -> Integer -> :a))
          ;; (f a n) = (a ^ n)
          (f (fn (a n)
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

  (declare ^^ ((Dividable :a :a) (Num :a) => (:a -> Integer -> :a)))
  (define (^^ base power)
    "Exponentiate BASE to a signed POWER."
    (if (< power 0)
        (^ (/ 1 base) (negate power))
        (^ base power))))

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
       (declare ,^^ (,type -> Integer -> ,type))
       (define (,^^ base power)
         (lisp ,type (base power) (cl:expt base power))))))

(%define-native-expt Fraction)
(%define-native-expt Single-Float)
(%define-native-expt Double-Float)

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/INTEGRAL")
