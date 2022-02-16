;;;; package.lisp

(fiasco:define-test-package #:thih-coalton-tests
  (:documentation "Tests for the THIH-COALTON system.")
  (:use #:cl #:thih-coalton)
  (:shadowing-import-from
   #:thih-coalton
   #:Type)
  (:shadowing-import-from
   #:coalton
   #:make-list
   #:Nil)
  (:export
   #:run-thih-coalton-tests))
