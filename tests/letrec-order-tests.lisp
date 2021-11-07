(in-package #:coalton-tests)

(deftest test-letrec-order ()
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
        (define into testLetrecOrderToString)))))
