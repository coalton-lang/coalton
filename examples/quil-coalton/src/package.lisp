(in-package #:cl-user)

(defpackage #:quil-coalton
  (:documentation "Public interface to QUIL-COALTON.")
  (:use #:coalton
        #:coalton-library)
  (:shadow #:error #:alt)
  (:export #:run-quil-parser))
