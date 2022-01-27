(cl:in-package #:coalton/hashtable-shim)

(defun make-custom-hash-table (size hash-function test-function)
  (make-hash-table :size size
                   :test test-function
                   :hash-function hash-function))

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

(defun custom-hash-table-foreach (table visitor)
  (maphash visitor table)
  (values))
