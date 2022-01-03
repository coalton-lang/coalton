(cl:in-package #:coalton/hashtable-shim)

(cerror "Ignore (hash table operations will fail at runtime)"
        "Coalton's custom hash tables are currently not supported on ~a"
        (lisp-implementation-type))

(defmacro error-unsupported ()
  '(error "Coalton's custom hash tables are unsupported on ~a" (lisp-implementation-type)))

(defun make-custom-hash-table (size hash-function test-function)
  (declare (ignore size hash-function test-function))
  (error-unsupported))

(defun custom-hash-table-get (table key)
  (declare (ignore table key))
  (error-unsupported))

(defun custom-hash-table-set (table key val)
  (declare (ignore table key val))
  (error-unsupported))

(defun custom-hash-table-containsp (table key)
  (declare (ignore table key))
  (error-unsupported))

(defun custom-hash-table-remove (table key)
  (declare (ignore table key))
  (error-unsupported))

(defun custom-hash-table-count (table)
  (declare (ignore table))
  (error-unsupported))

(defun custom-hash-table-foreach (table visitor)
  (declare (ignore table visitor))
  (error-unsupported))
