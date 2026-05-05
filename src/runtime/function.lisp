(defpackage #:coalton-impl/runtime/function
  (:use
   #:cl)
  (:import-from
   #:coalton
   #:call-coalton-function)
  (:export
   #:call-coalton-function))

(in-package #:coalton-impl/runtime/function)

(declaim (inline call-coalton-function))
(defun coalton:call-coalton-function (function &rest args)
  "Apply Coalton FUNCTION to ARGS from Common Lisp.

Keyword arguments are forwarded unchanged."
  (apply function args))
