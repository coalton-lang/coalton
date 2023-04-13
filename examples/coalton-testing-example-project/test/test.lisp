(defpackage #:coalton-testing-example-project/test
  (:use #:coalton #:coalton-prelude #:coalton-testing)
  (:local-nicknames (#:example #:coalton-testing-example-project))
  (:export #:run-tests))
(in-package #:coalton-testing-example-project/test)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:coalton-testing-example-project/fiasco-test-package)

(coalton-fiasco-init #:coalton-testing-example-project/fiasco-test-package)

(cl:defun run-tests ()
  (fiasco:run-package-tests
   :packages '(#:coalton-testing-example-project/fiasco-test-package)
   :interactive cl:t))

(define-test my-first-test ()
  (is True))

(define-test test-always-returns-zero ()
  (is (== 0 (example:always-returns-zero)))

  (is (== 0 (example:always-returns-zero))
      "ALWAYS-RETURNS-ZERO returned a non-zero value!"))

(define-test test-one-element-list ()
  (matches (Cons _ (Nil))
      (example:one-element-list 0))

  (matches (Cons _ (Nil))
      (example:one-element-list 0)
      "ONE-ELEMENT-LIST returned a list with length other than 1!")

  (match (example:one-element-list 0)
    ((Nil) (is False "ONE-ELEMENT-LIST returned an empty list!"))
    ((Cons elt (Nil)) (is (== 0 elt)))
    ((Cons _ _) (is False "ONE-ELEMENT-LIST returned a list with more than 1 element!"))))
