;;;; constants.lisp
;;;;
;;;; This file contains constant values used throughout compilation.

(defpackage #:coalton-impl/constants
  (:use #:cl)
  (:export
   #:+default-loop-label+               ; VARIABLE
   ))

(in-package #:coalton-impl/constants)

(defconstant +default-loop-label+ :coalton-loop
  "Supplied as a loop label in while, while-let, for, loop, break and
continue when a label is not supplied by the user.")
