(defpackage #:coalton-impl/runtime/exceptions
  (:use
   #:cl)
  (:export
   #:exception-condition
   #:exception-condition-datum
   #:exception-condition-erase))

(in-package #:coalton-impl/runtime/exceptions)

(define-condition exception-condition (condition)
  ((datum :initarg :datum :reader exception-condition-datum)
   (erase :initarg :into-string :reader exception-condition-erase)))
