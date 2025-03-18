;;;; hash-defining-macros.lisp
;;;;
;;;; Ordinarily we would like to interleave these macros definitions
;;;; with LISP-TOPLEVEL in Coalton code. However, due to issue #1408,
;;;; nuances of when things get evaluated and whether they persist in
;;;; FASLs linger. In the mean time, we define the Lisp macros
;;;; separately before being used by Coalton.
;;;;
;;;; See also num-defining-macros.lisp
;;;;
;;;; NOTE: This package is not intended to be used by users.

(defpackage #:coalton-library/math/hash-defining-macros
  (:use
   #:coalton
   #:coalton-library/classes)
  (:export
   #:define-sxhash-hasher))

(in-package #:coalton-library/math/hash-defining-macros)

(cl:defmacro define-sxhash-hasher (type)
  "Define an instance of Hash for the Coalton type TYPE using CL:SXHASH."
  `(define-instance (Hash ,type)
     (inline)
     (define (hash item)
       (lisp Hash (item)
         (cl:sxhash item)))))
