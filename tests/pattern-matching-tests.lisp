(in-package #:coalton-native-tests)

(coalton-toplevel
  (derive Eq)
  (define-type PatternNumber (PatternNumber Integer))
  (define-instance (Num PatternNumber)
    (define (+ (PatternNumber a) (PatternNumber b)) (PatternNumber (+ a b)))
    (define (- (PatternNumber a) (PatternNumber b)) (PatternNumber (- a b)))
    (define (* (PatternNumber a) (PatternNumber b)) (PatternNumber (* a b)))
    (define (fromInt x) (PatternNumber x)))
  (declare numeric-one? (Num :a => :a -> Boolean))
  (define (numeric-one? x) (match x (1 True) (_ False)))
  (declare nested-numeric-one? (Num :a => Optional :a -> Boolean))
  (define (nested-numeric-one? x) (match x ((Some 1) True) (_ False)))
  (declare delayed-numeric-one? (Num :a => :a -> (Void -> Boolean)))
  (define (delayed-numeric-one? x) (fn () (match x (1 True) (_ False)))))

(define-test test-overloaded-integer-patterns ()
  (is (numeric-one? (the Integer 1)))
  (is (numeric-one? (the IFix 1)))
  (is (numeric-one? (the UFix 1)))
  (is (numeric-one? 1.0))
  (is (numeric-one? 1.0d0))
  (is (numeric-one? (the Fraction 1)))
  (is (numeric-one? (the big-float:Big-Float 1)))
  (is (numeric-one? (PatternNumber 1)))
  (is (not (numeric-one? 2.0d0)))
  (is (not (numeric-one? (PatternNumber 2))))
  (is (nested-numeric-one? (Some 1.0d0)))
  (is (nested-numeric-one? (Some (PatternNumber 1))))
  (is (not (nested-numeric-one? (Some (PatternNumber 2)))))
  (is (not (nested-numeric-one? (the (Optional Integer) None))))
  (is ((delayed-numeric-one? (PatternNumber 1))))
  (is (not ((delayed-numeric-one? (PatternNumber 2)))))
  (let calls = (cell:new 0))
  (is (match (progn (cell:write! calls (+ 1 (cell:read calls))) 2.0d0)
        (1 False) (2 True) (_ False)))
  (is (== 1 (cell:read calls)))
  (let (Some 1) = (Some (PatternNumber 1)))
  ;; Both an inline literal scrutinee and a parameter pattern need conversion.
  (is (match 1.0d0 (1 True) (_ False)))
  (let f = (fn ((Some 1)) True))
  (is (f (Some 1.0d0))))

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

  (let ((declare f (Ordering -> String))
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
  (declare tuple-match-pair (Integer -> (Tuple Integer Integer)))
  (define (tuple-match-pair x)
    (Tuple x (1+ x)))

  (declare tuple-match-bind-var (Integer -> (Tuple Integer Integer)))
  (define (tuple-match-bind-var x)
    (match (tuple-match-pair x)
      (pair (Tuple (snd pair) (fst pair)))))

  (declare tuple-match-bind-constructor (Integer -> (Tuple Integer Integer)))
  (define (tuple-match-bind-constructor x)
    (match (tuple-match-pair x)
      ((Tuple a b) (Tuple b a))))

  (declare tuple-match-wildcard-eligible (Integer -> Integer))
  (define (tuple-match-wildcard-eligible x)
    (match (tuple-match-pair x)
      ((Tuple 0 b) b)
      (_ x))))

(define-test test-tuple-match-var ()
  (is (== (Tuple 11 10)
          (tuple-match-bind-var 10)))
  (is (== (tuple-match-bind-var 42)
          (tuple-match-bind-constructor 42))))

(define-test test-tuple-match-wildcard ()
  (is (== 1
          (tuple-match-wildcard-eligible 0)))
  (is (== 10
          (tuple-match-wildcard-eligible 10)))
  (is (== (tuple-match-wildcard-eligible 42)
          (fst (tuple-match-pair 42)))))

(cl:in-package #:coalton-tests)

(deftest test-superclass-fundep-concrete-instance-methods ()
  (check-coalton-types
   "(define-class (C :a :b (:a -> :b)))
    (define-instance (C Integer String))
    (define-class (C :a :b => D :a)
      (output (:a -> :b))
      (input (:b -> :a))
      (local-id (forall (:b) (:a * :b -> :b))))
    (define-instance (D Integer)
      (define (output _) \"hello\")
      (define (input text) (if (== text \"hello\") 1 0))
      (define (local-id _ value) value))"))

(deftest test-superclass-fundep-renamed-instance-methods ()
  (check-coalton-types
   "(define-class (C :a :b (:a -> :b))
      (base (:a -> :b)))
    (define-class (C :a :b => D :a)
      (output (:a -> :b))
      (same (Eq :b => :a * :b -> Boolean)))
    (define-instance (C (List :x) :y => D (List :x))
      (define (output xs) (base xs))
      (define (same xs value) (== (base xs) value)))"))

(deftest test-superclass-fundep-swapped-instance-methods ()
  (check-coalton-types
   "(define-class (C :a :b (:a -> :b)))
    (define-class (C :a :b => D :a)
      (output (:a -> :b))
      (same (Eq :b => :a * :b -> Boolean)))
    (define-instance (C (Tuple :b :c) :c => D (Tuple :b :c))
      (define (output (Tuple _ c)) c)
      (define (same (Tuple _ c) value) (== c value)))"))

(deftest test-instance-underapplied-value-warns-once ()
  (let ((warnings nil))
    (handler-bind ((source:source-warning
                    (lambda (condition)
                      (push (princ-to-string condition) warnings)
                      (muffle-warning condition))))
      (check-coalton-types
       "(define-class (C :a) (method (:a -> Integer)))
        (define-instance (C Integer)
          (define (method value)
            (fn (other) (+ value other))
            1))"))
    (is (= 1 (count-if (lambda (warning)
                        (search "Value may be underapplied" warning))
                      warnings)))))

(deftest test-runtime-repr-instance-exceptions-check-type-head ()
  (dolist (package-name '("COALTON/LISPARRAY" "COALTON/MATH/COMPLEX"))
    (let ((*package* (find-package package-name)))
      (flet ((check-instance (type)
               (coalton-impl/typechecker/define-instance::check-instance-valid
                (parse-form
                 (format nil "(define-instance (coalton/types:RuntimeRepr ~A))" type)
                 #'coalton-impl/parser/toplevel::parse-define-instance))))
        (is (null (check-instance (if (string= package-name "COALTON/LISPARRAY")
                                      "(LispArray Integer)"
                                      "(Complex Integer)"))))
        (dolist (type '("Integer" "(List Integer)" "(Tuple Integer String)" "(:f Integer)"))
          (signals tc:tc-error (check-instance type)))))))

(deftest test-deep-constructor-pattern-analysis ()
  (let* ((specializer 'coalton-impl/analysis/pattern-exhaustiveness::specialize-matrix)
         (original (symbol-function specializer))
         (specializations 0)
         (warnings nil))
    (flet ((nested (inner)
             (dotimes (depth 16 inner)
               (setf inner (format nil "(Layer ~A)" inner)))))
      (unwind-protect
           (progn
             ;; Bound work instead of using a machine-dependent wall-clock timeout.
             (setf (symbol-function specializer)
                   (lambda (matrix pattern)
                     (when (> (incf specializations) 2000)
                       (error "Repeated constructor specialization in pattern analysis"))
                     (funcall original matrix pattern)))
             (handler-bind ((source:source-warning
                             (lambda (condition)
                               (push (princ-to-string condition) warnings)
                               (muffle-warning condition))))
               (let ((type (format nil "(~A -> Integer)" (nested "(Tuple Boolean Boolean)"))))
                 (check-coalton-types
                  (format nil
                          "(define-type (Layer :a) (Layer :a))
                           (declare deep-match ~A)
                           (define (deep-match value)
                             (match value
                               (~A 0)
                               (~A 1)
                               (~A 2)
                               (~A 3)
                               (_ 4)))"
                          type
                          (nested "(Tuple (True) (True))")
                          (nested "(Tuple (True) (False))")
                          (nested "(Tuple (False) (True))")
                          (nested "(Tuple (False) (False))"))
                  (cons "deep-match" type))))
             (is (< specializations 2000))
             (is (= 1 (count-if (lambda (warning)
                                 (search "useless match case" warning))
                               warnings)))
             (is (notany (lambda (warning)
                           (search "non-exhaustive match" warning))
                         warnings)))
        (setf (symbol-function specializer) original)))))
