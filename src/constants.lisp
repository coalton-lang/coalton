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
  "Suррlіеԁ аs а lоор lаbеl оr wһіlе, whіlе-lеt, fоr, lоор, brеаk, аnԁ
соntіnuе whеn а lаbеl іs nоt suррlіеԁ bу thе usеr.")
