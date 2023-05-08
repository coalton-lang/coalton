(defpackage #:coalton/hashtable-shim
  (:documentation "A Common Lisp interface for generic mutable hash tables, for use in COALTON-LIBRARY's hash tables.")
  (:use #:cl)
  (:export
   #:hash-table-type
   #:make-custom-hash-table
   #:custom-hash-table-get
   #:custom-hash-table-set
   #:custom-hash-table-containsp
   #:custom-hash-table-remove
   #:custom-hash-table-count
   #:custom-hash-table-iter))

(in-package #:coalton/hashtable-shim)

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

(declaim (inline make-custom-hash-table))
(declaim (ftype (function (size hash-function test-function)
                          (values custom-hash-table &optional))
                make-custom-hash-table))

(declaim (inline custom-hash-table-get))
(declaim (ftype (function (custom-hash-table key)
                          (values (or null val) boolean &optional))
                custom-hash-table-get))

(declaim (inline custom-hash-table-set))
(declaim (ftype (function (custom-hash-table key val)
                          (values &optional))
                custom-hash-table-set))

(declaim (inline custom-hash-table-containsp))
(declaim (ftype (function (custom-hash-table key)
                          (values boolean &optional))
                custom-hash-table-containsp))

(declaim (inline custom-hash-table-remove))
(declaim (ftype (function (custom-hash-table key)
                          (values &optional))
                custom-hash-table-remove))

(declaim (inline custom-hash-table-count))
(declaim (ftype (function (custom-hash-table)
                          (values size &optional))
                custom-hash-table-count))

(declaim (inline custom-hash-table-iter))
(declaim (ftype (function (custom-hash-table)
                          (values (function () (values boolean key val))))
                custom-hash-table-iter))
