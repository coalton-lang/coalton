(in-package #:cl-user)

(defpackage #:quil-coalton
  (:documentation "Public interface to QUIL-COALTON.")
  (:shadow #:take #:char)
  (:use #:coalton
        #:coalton-prelude
        #:coalton-library/collections/immutable/list)
  (:local-nicknames
   (#:string #:coalton-library/string))
  (:import-from
   #:coalton-library/functions
   #:asum)
  (:export #:run-quil-parser))
