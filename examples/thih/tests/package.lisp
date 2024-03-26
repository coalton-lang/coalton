;;;; package.lisp

(defpackage #:thih-coalton/tests
  (:use #:coalton-testing)
  (:local-nicknames (#:th #:thih-coalton))
  (:export #:run-tests))

(in-package #:thih-coalton/tests)

(named-readtables:in-readtable coalton:coalton)

(fiasco:define-test-package #:thih-coalton/fiasco-test-package
    (:documentation "Tests for the THIH-COALTON system."))

(coalton-fiasco-init #:thih-coalton/fiasco-test-package)

(cl:defun run-tests ()
  (fiasco:run-package-tests
   :packages '(#:thih-coalton/fiasco-test-package)
   :interactive cl:t))





