(in-package #:coalton/hashtable-shim)

#+sbcl-pre-2-2-2
(defun make-custom-hash-table (size hash-function test-function)
  (let ((weakness 0) ; not weak
        (ht-kind 48) ; KIND: custom
        (synch 0) ; not synchronized
        (userfun-flag 2) ; custom hash func flag
        (size (max 7 ; copied from sbcl's target-hash-table.lisp and maphash.lisp
                   (min size (ash 1 24))))
        (rehash-size 1.5f0) ; copied from maphash.lisp
        (rehash-threshold 1f0)
        (test-name 'coalton-custom-hash-table))
    (sb-impl::%make-hash-table
     (logior weakness
             ht-kind
             synch
             userfun-flag)
     test-name
     test-function
     hash-function
     size
     rehash-size
     rehash-threshold)))

#+sbcl-post-2-2-2
(defun make-custom-hash-table (size hash-function test-function)
  (make-hash-table :size size
                   :test test-function
                   :hash-function hash-function))

(deftype hash-table-type ()
  'hash-table)

(defun custom-hash-table-get (table key)
  (gethash key table))

(defun custom-hash-table-set (table key val)
  (setf (gethash key table) val)
  (values))

(defun custom-hash-table-containsp (table key)
  (nth-value 1 (custom-hash-table-get table key)))

(defun custom-hash-table-remove (table key)
  (remhash key table)
  (values))

(defun custom-hash-table-count (table)
  (hash-table-count table))

(defun custom-hash-table-iter (table)
  (with-hash-table-iterator (getter table)
    (lambda ()
      (getter))))
