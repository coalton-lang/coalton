(fiasco:define-test-package #:my-app/test
  (:use #:coalton #:coalton-testing)
  (:export #:run-all-tests))

(coalton-fiasco-init #:my-app/test)

(cl:defun run-tests ()
  (fiasco:run-package-tests
   :packages '(#:my-app/test)
   :intereactive cl:t))

(define-test hello-world ()
  (is ()))
