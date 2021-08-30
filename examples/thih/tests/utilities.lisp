;;;; utilities.lisp

(in-package #:thih-coalton-tests)

(defun run-thih-coalton-tests ()
  (run-package-tests
   :package ':thih-coalton-tests
   :interactive t))
