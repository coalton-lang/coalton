(in-package #:coalton-impl)

(defun process-toplevel-declarations (decl-forms env)
  "Parse all the type declaration forms in DECL-FORMS and create a
HASH-TABLE mapping from the names to the declared type"
  (declare (type tc:environment env))
  (let ((table (make-hash-table)))
    (dolist (form decl-forms)
      (multiple-value-bind (name declared-type)
          (parse-declaration form env)
        (setf (gethash name table) declared-type)))
    table))

(defun parse-declaration (declaration env)
  (declare (type tc:environment env))
  (assert (and (eql (first declaration) 'coalton:declare)
               (= 3 (length declaration)))
          () "Malformed DECLARE form ~A" declaration)
  (let* ((name (second declaration))
        (type-expr (third declaration))
        (declared-type (tc:parse-and-resolve-type env type-expr)))
    (unless (equalp
             (tc:kind-of declared-type)
             tc:+kstar+)
      (error 'tc:type-construction-error :type declared-type))
    (values name declared-type)))
