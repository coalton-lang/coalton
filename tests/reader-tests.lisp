(in-package #:coalton-tests)

(defmacro coalton-example-with-gensym ()
  (let ((x (gensym)))
    `(coalton:coalton
      (coalton:let ((,x 5))
        ,x))))

(deftest test-uninterned-symbols ()
  (let ((y (coalton-example-with-gensym)))
    (is (= y 5))))

(deftest read-eval-error ()
  "Check that errors signalled during direct evaluation of Coalton have correct messages and are printable."
  (let ((*compile-file-truename* nil)
        (*load-truename* nil)
        (*package* (find-package "COALTON-USER")))
    (handler-case
        (progn
          (eval (read-from-string
                 "(coalton-toplevel (define 1 x))"))
          (is nil "error was not signalled"))
      (coalton-impl/parser/base:parse-error (c)
        (is (string= "Invalid variable"
                     (source:message c))
            "condition message is correct")
        (is (princ-to-string c)
            "condition prints without error")))

    (eval (read-from-string "(coalton-toplevel
  (declare add-3 (UFix -> UFix))
  (define (add-3 x)
    (+ 3 x)))"))
    (handler-case
        (eval (read-from-string "(coalton (add-3 \"two\"))"))
      (coalton-impl/typechecker/base:tc-error (c)
        (is (string= "Type mismatch"
                     (source:message c))
            "condition message is correct")))))
