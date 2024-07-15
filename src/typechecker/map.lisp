;;; Environment maps provide readable representations of hash
;;; table-valued fields present in Coalton environment updates
;;; in generated Lisp code.

(defpackage #:coalton-impl/typechecker/map
  (:use
   #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:environment-map
   #:make-map
   #:get-table
   #:get-value))

(in-package #:coalton-impl/typechecker/map)

(defstruct environment-map
  "A readable, hash table-backed map."
  (table (util:required 'table) :type hash-table :read-only t)
  (test  (util:required 'test)  :type symbol     :read-only t))

(defun make-map (&key initial-contents (test 'equal))
  "Construct an environment map containing the keys and values of the association list ALIST.

Environment maps have a readable representation as an alist.
Hash table is initialized using TEST."
  (let ((table (make-hash-table :test test)))
    (dolist (cons initial-contents)
      (alexandria:ensure-gethash (car cons) table (cdr cons)))
    (make-environment-map :table table
                          :test test)))

(defmethod make-load-form ((self environment-map) &optional environment)
  (make-load-form-saving-slots self :environment environment))

(defmethod print-object ((self environment-map) stream)
  (if *print-readably*
      (let ((alist (alexandria:hash-table-alist (environment-map-table self))))
        (write-string "#.(" stream)
        (write 'make-map :stream stream)
        (dolist (value `(:initial-contents ',alist :test ',(environment-map-test self)))
          (write-char #\Space stream)
          (write value :stream stream))
        (write-string ")" stream))
      (call-next-method)))

(declaim (inline get-table))
(defun get-table (environment-map)
  "Return the underlying hash table that provides storage for environment map ENVIRONMENT-MAP."
  (declare (type environment-map environment-map))
  (environment-map-table environment-map))

(declaim (inline get-value))
(defun get-value (environment-map key)
  "Return the value in environment map ENVIRONMENT-MAP associated with KEY."
  (declare (type environment-map environment-map))
  (gethash key (environment-map-table environment-map)))

(declaim (inline (setf get-value)))
(defun (setf get-value) (value environment-map key)
  "Mutate environment map ENVIRONMENT-MAP to associate KEY with VALUE."
  (declare (type environment-map environment-map))
  (setf (gethash key (environment-map-table environment-map)) value)
  environment-map)
