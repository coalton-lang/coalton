(in-package #:coalton-tests)

(deftest test-def-before-instance ()
  "Test that bindings are initialized in a correct order so that constructing instance structs never reads an uninitialized variable.

From coalton-lang/coalton issue #199"
  (with-toplevel-compilation ()
    (coalton-toplevel
      (define-type TestLetrecOrder
        TestLetrecOrder))

    (coalton-toplevel
      (declare testLetrecOrderToString (TestLetrecOrder -> String))
      (define (testLetrecOrderToString x)
        "TestLetrecOrder")

      (define-instance (Into TestLetrecOrder String)
        (define into testLetrecOrderToString)))

    (coalton-toplevel
      (declare getTestLetrecString (Unit -> String))
      (define (getTestLetrecString unit)
        (into TestLetRecOrder))))

  (is (equal "TestLetrecOrder"
             (coalton-test-user::getTestLetrecString Unit))))

(deftest interleaved-instances ()
  (with-toplevel-compilation ()
    (coalton-toplevel
      (define-type InterleavedInstancesT InterleavedInstancesT)

      (define-type (InterleavedInstancesT2 :a)
        (InterleavedInstancesT2 :a))

      (define-instance (Semigroup InterleavedInstancesT)
        (define (<> a b) InterleavedInstancesT))

      (define-instance (Monoid InterleavedInstancesT)
        (define mempty InterleavedInstancesT))

      (define-instance (Semigroup :a => (Semigroup (InterleavedInstancesT2 :a)))
        (define (<> a b)
          (match (Tuple a b)
            ((Tuple (InterleavedInstancesT2 a) (InterleavedInstancesT2 b)) (InterleavedInstancesT2 (<> a b))))))

      (define-instance (Monoid :a => (Monoid (InterleavedInstancesT2 :a)))
        (define mempty (InterleavedInstancesT2 mempty)))

      (declare x (InterleavedInstancesT2 InterleavedInstancesT))
      (define x mempty)))
  (is (equalp (coalton-test-user::x) (coalton-test-user::x))))
