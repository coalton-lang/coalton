(cl:in-package #:coalton/hashtable-shim)

(deftype custom-hash-table (&optional key val)
  (declare (ignore key val))
  'hash-table)

(defun make-custom-hash-table (name size hash-function test-function)
  (when (not (fboundp name))
    (setf (symbol-function name) test-function)
    (eval
     `(sb-ext:define-hash-table-test ,name
          (lambda (x) (funcall ,hash-function x)))))
  (make-hash-table :size size :test name))

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
