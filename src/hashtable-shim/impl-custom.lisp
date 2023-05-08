(in-package #:coalton/hashtable-shim)

(deftype hash-table-type ()
  'coalton/hashtable-shim/hash-table:hash-table)

(defun make-custom-hash-table (size hash-function test-function)
  (coalton/hashtable-shim/hash-table:make-hash-table size hash-function test-function))

(defun custom-hash-table-get (table key)
  (coalton/hashtable-shim/hash-table:hash-table-get table key))

(defun custom-hash-table-set (table key val)
  (coalton/hashtable-shim/hash-table:hash-table-insert table key val))

(defun custom-hash-table-containsp (table key)
  (nth-value 1 (coalton/hashtable-shim/hash-table:hash-table-get table key)))

(defun custom-hash-table-remove (table key)
  (coalton/hashtable-shim/hash-table:hash-table-delete table key))

(defun custom-hash-table-count (table)
  (coalton/hashtable-shim/hash-table:hash-table-count table))

(defun custom-hash-table-iter (table)
  (coalton/hashtable-shim/hash-table:hash-table-iter table))
