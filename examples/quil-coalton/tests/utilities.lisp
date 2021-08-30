;;;; utilities.lisp

(in-package #:quil-coalton-tests)

(defun run-quil-coalton-tests ()
  (run-package-tests
   :package ':quil-coalton-tests
   :interactive t))
