;;;; package.lisp

(fiasco:define-test-package #:quil-coalton-tests
  (:documentation "Tests for the QUIL-COALTON system.")
  (:use #:cl #:quil-coalton)
  (:export
   #:run-quil-coalton-tests))
