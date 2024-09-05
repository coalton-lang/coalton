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
  (handler-case
      (let ((*compile-file-truename* nil)
            (*load-truename* nil))
        (eval (read-from-string
               "(coalton:coalton-toplevel (coalton:define 1 x))"))
        (is nil "error was not signalled"))
    (coalton-impl/parser/base:parse-error (c)
      ;; Just confirm the error is printable - read/eval/reread will
      ;; reindent code differently on different platforms
      (is (princ-to-string c)))))
