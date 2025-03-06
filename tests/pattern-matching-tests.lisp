(in-package #:coalton-native-tests)

(coalton-toplevel 
  (repr :enum)
  (define-type MyEnum
    Jalapeno
    Onion
    Lime))

(define-test test-match-on-enum ()
  (let ((declare f (MyEnum -> String))
        (f (fn (x)
             (match x
               ((Jalapeno) "jalapeno")
               ((Onion) "onion")
               (_ "lime?")))))
    (is (== (f Jalapeno) "jalapeno"))
    (is (== (f Lime) "lime?"))))

(define-test test-match-on-ints ()
  (let ((f (fn (x)
             (match x
               (0 "zero")
               (1 "one")
               (2 "two")
               (_ "error")))))
    (is (== (f 0)
            "zero"))
    (is (== (f 1)
            "one"))
    (is (== (f 2)
            "two"))))

(define-test test-match-on-nums ()

  (let ((f (fn (x)
             (match x
               (0 "zero")
               (1 "one")
               (2 "two")))))
    (is (== (f (the IFix 0))
            "zero"))
    (is (== (f (the U8 1))
            "one"))
    (is (== (f (the I16 2))
            "two"))))

(define-test test-match-lists ()
  (let ((f (fn (xs)
             (match xs
               ((Nil) 0)
               ((Cons x xs) 1)))))
    (is (== (f Nil) 0))
    (is (== (f (make-list 1 2 3)) 1))))

(coalton-toplevel
  (define-type match-foo
    (MFoo Integer)
    (MBar (Tuple Integer Integer))))

(define-test test-match-constructors ()
  (let ((f (fn (x)
             (match x
               ((MFoo n) n)
               ((MBar t) (fst t))))))
    (is (== (f (MFoo 9)) 9))
    (is (== (f (MBar (Tuple 7 8))) 7))))

(define-test test-match-on-fractions ()
  (is (match 1/4
        (1/4 True)
        (_ False))))

(define-test test-match-on-single-floats ()
  (is (match 0.15f0
        (0.15f0 True)
        (_ False))))

(define-test test-match-on-double-floats ()
  (is (match 0.15d0
        (0.15d0 True)
        (_ False))))

(define-test test-match-on-strings ()
  (is (match "red"
        ("red" True)
        (_ False))))

(define-test test-match-on-chars ()
  (is (match #\c
        (#\c True)
        (_ False))))
