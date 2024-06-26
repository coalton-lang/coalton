(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare two-arg-double-float-add
     (Double-Float -> Double-Float -> Double-Float))
  (inline)
  (define (two-arg-double-float-add x y)
    (lisp Double-Float (x y) (cl:+ x y))))

(coalton-toplevel
  (define (two-arg-double-float-add-caller x y)
    (two-arg-double-float-add x y))
  (define two-arg-double-float-add-underapply
    (two-arg-double-float-add 3.0d0))
  (define (two-arg-double-float-add-underapply-caller x)
    (two-arg-double-float-add-underapply x)))

(define-test function-inline ()
  (is (== 5.0d0 (two-arg-double-float-add-caller 2.0d0 3.0d0)))
  (is (== 5.0d0 (two-arg-double-float-add-underapply-caller 2.0d0))))

(coalton-toplevel
  (define-class (class-for-inline-method-test :a)
    (method-for-inline-test (:a -> :a -> :a)))
  (define-instance (class-for-inline-method-test Double-Float)
    (inline)
    (define (method-for-inline-test x y)
      (lisp Double-Float (x y) (cl:+ x y)))))

(coalton-toplevel
  (declare method-for-inline-test-caller
    (Double-Float -> Double-Float -> Double-Float))
  (define (method-for-inline-test-caller x y)
    (method-for-inline-test x y)))

(define-test method-inline ()
  (is (== 5.0d0 (method-for-inline-test-caller 2.0d0 3.0d0))))

(in-package #:coalton-tests)

(deftest function-inline-error ()
  (signals coalton-impl/parser:parse-error
    (check-coalton-types
     "(inline myfun)
      (define (myfun x) x)")))

(deftest method-inline-error ()
  (signals coalton-impl/parser:parse-error
    (check-coalton-types
     "(define-class (C :a)
        (m (:a -> :a -> :a)))
      (define-instance (m Double-Float)
        (inline m)
        (define (m x y)
          (lisp Double-Float (x y) (cl:+ x y))))")))
