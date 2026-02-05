(in-package #:coalton-native-tests)

(coalton-toplevel 
  (repr :enum)
  (derive Eq)
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
    (is (== (f Jalapeno)
            "jalapeno"))
    (is (== (f Onion)
            "onion"))
    (is (== (f Lime)
            "lime?")))

  (let ((declare f (MyEnum -> (Optional MyEnum)))
        (f (fn (x)
             (match x
               ((Jalapeno) None)
               ((Onion) None)
               (x (Some x))))))
    (is (== (f Jalapeno)
            None))
    (is (== (f Onion)
            None))
    (is (== (f Lime)
            (Some Lime))))

  (let ((declare f (Ord -> String))
        (f (fn (x)
             (match x
               ((LT) "lt")
               ((EQ) "eq")
               ((GT) "gt")))))
    (is (== (f LT)
            "lt"))
    (is (== (f EQ)
            "eq"))
    (is (== (f GT)
            "gt"))))

(define-test test-match-on-boolean ()
  (let ((f (fn (x)
             (match x
               ((True) 1)
               ((False) 2)))))
    (is (== (f True)
            1))
    (is (== (f False)
            2)))

  (let ((f (fn (x)
             (match x
               ((True) True)
               (x x)))))
    (is (== (f True)
            True))
    (is (== (f False)
            False)))

  (let ((f (fn (x)
             (match x
               ((True) 1)
               (_ 2)))))
    (is (== (f True)
            1))
    (is (== (f False)
            2))))

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

(coalton-toplevel
 (declare prod-proj-1 (Tuple :a :b -> (Tuple :a (Tuple :a :b))))
 (define (prod-proj-1 (= tpl (Tuple a _))) (Tuple a tpl)))

(define-test test-match-bindings ()
  (let mb = (MBar (Tuple 10 20)))
  (let tpl = (match mb
               ((MFoo n)                   (Tuple n n))
               ((MBar (= tpl (Tuple _ _))) tpl)))
  (is (== tpl (Tuple 10 20)))

  (is (match mb
        ((Mbar (= tpl (Tuple (= a _) (= b _))))
         (== tpl (Tuple a b)))
        (_ False)))

  (is (match (make-list 1 2 3 4 5)
        ;; match cons with 2 in second position
        ;; but bind whole list and tail to vars
        ((= lst (Cons a (= tl (Cons 2 _))))
         (== lst (Cons a tl)))
        (_ False)))

  (let ((declare x (Tuple Integer Integer))
        (x (Tuple 1 2)))
    (is (== (Tuple 1 x) (prod-proj-1 x)))
    (let (Tuple 1 (= tpl (Tuple _ _))) = (prod-proj-1 x))
    (is (== x tpl))))

(coalton-toplevel
  (declare mv-match-pair (Integer -> (Tuple Integer Integer)))
  (define (mv-match-pair x)
    (Tuple x (1+ x)))

  (declare mv-match-bind-var (Integer -> (Tuple Integer Integer)))
  (define (mv-match-bind-var x)
    (match (mv-match-pair x)
      (pair (Tuple (snd pair) (fst pair)))))

  (declare mv-match-bind-constructor (Integer -> (Tuple Integer Integer)))
  (define (mv-match-bind-constructor x)
    (match (mv-match-pair x)
      ((Tuple a b) (Tuple b a))))

  (declare mv-match-wildcard-eligible (Integer -> Integer))
  (define (mv-match-wildcard-eligible x)
    (match (mv-match-pair x)
      ((Tuple 0 b) b)
      (_ x))))

(define-test test-tuple-multiple-values-match-var ()
  (is (== (Tuple 11 10)
          (mv-match-bind-var 10)))
  (is (== (mv-match-bind-var 42)
          (mv-match-bind-constructor 42))))

(define-test test-tuple-multiple-values-match-wildcard ()
  (is (== 1
          (mv-match-wildcard-eligible 0)))
  (is (== 10
          (mv-match-wildcard-eligible 10)))
  (is (== (mv-match-wildcard-eligible 42)
          (fst (mv-match-pair 42)))))
