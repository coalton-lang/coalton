(cl:in-package #:coalton/hashtable-shim)

(deftype custom-hash-table (&optional key val)
  (declare (ignore key val))
  '(or genhash::hash-container hash-table))

(defun make-custom-hash-table (name size hash-function test-function)
  (handler-bind
      ((genhash:unknown-hash
         (lambda (e)
           (genhash:register-test-designator name hash-function test-function))))
    (genhash:make-generic-hashtable :size size :test name)))

(defun custom-hash-table-get (table key)
  (genhash:hashref key table))

(defun custom-hash-table-set (table key val)
  (setf (genhash:hashref key table) val)
  (values))

(defun custom-hash-table-containsp (table key)
  (nth-value 1 (custom-hash-table-get table key)))

(defun custom-hash-table-remove (table key)
  (genhash:hashrem key table)
  (values))

(defun custom-hash-table-count (table)
  (genhash:generic-hash-table-count table))

(defun custom-hash-table-foreach (table visitor)
  (genhash:hashmap visitor table)
  (values))
