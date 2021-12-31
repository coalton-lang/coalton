(uiop:define-package #:coalton/hashtable-shim
  (:documentation "A Common Lisp interface for generic mutable hash tables, for use in COALTON-LIBRARY's hash tables.")
  (:use #:cl)
  (:export
   #:make-custom-hash-table
   #:custom-hash-table-get
   #:custom-hash-table-set
   #:custom-hash-table-containsp
   #:custom-hash-table-remove
   #:custom-hash-table-count
   #:custom-hash-table-foreach))
(cl:in-package #:coalton/hashtable-shim)

(deftype key ()
  't)

(deftype val ()
  't)

(deftype custom-hash-table (&optional key val)
  (declare (ignore key val))
  't)

(deftype hash ()
  '(and fixnum unsigned-byte))

(deftype hash-function ()
  '(function (key) (values hash &optional)))

(deftype test-function ()
  '(function (key key) (values boolean &optional)))

(deftype size ()
  '(and fixnum unsigned-byte))

(declaim (ftype (function (size hash-function test-function)
                          (values custom-hash-table &optional))
                make-custom-hash-table))

(declaim (ftype (function (custom-hash-table key)
                          (values (or null val) boolean &optional))
                custom-hash-table-get))

(declaim (ftype (function (custom-hash-table key val)
                          (values &optional))
                custom-hash-table-set))

(declaim (ftype (function (custom-hash-table key)
                          (values boolean &optional))
                custom-hash-table-containsp))

(declaim (ftype (function (custom-hash-table key)
                          (values &optional))
                custom-hash-table-remove))

(declaim (ftype (function (custom-hash-table)
                          (values size &optional))
                custom-hash-table-count))

(deftype visitor-function ()
  '(function (key val) (values &rest t)))

(declaim (ftype (function (custom-hash-table visitor-function)
                          (values &optional))
                custom-hash-table-foreach))
