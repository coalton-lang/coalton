(in-package #:coalton-impl)

(defun process-toplevel-declarations (decl-forms env)
  "Parse all the type declaration forms in DECL-FORMS and create a
HASH-TABLE mapping from the names to the declared type"
  (declare (type environment env))
  (let ((table (make-hash-table)))
    (dolist (form decl-forms)
      (multiple-value-bind (name declared-type)
          (parse-declaration form env)
        (setf (gethash name table) declared-type)))
    table))

(defun parse-declaration (declaration env)
  (declare (type environment env))
  (assert (and (eql (first declaration) 'coalton:declare)
               (= 3 (length declaration)))
      () "Malformed DECLARE form ~A" declaration)
  (let* ((name (second declaration))
         (type-expr (third declaration))
         (declared-type (parse-and-resolve-type env type-expr)))
    (unless (equalp
             (coalton-impl/typechecker::kind-of declared-type)
             coalton-impl/typechecker::kstar)
      (error 'type-construction-error :type declared-type))
    (values name declared-type)))

(defun process-toplevel-type-declarations (decl-forms env)
  "Parse all the kind declaration forms in DECL-FORMS and create a
HASH-TABLE mapping from the names to the declared type"
  (declare (type environment env))
  (let ((table (make-hash-table)))
    (dolist (form decl-forms)
      (multiple-value-bind (name declared-kind)
          (parse-type-declaration form env)
        (if (gethash name table)
            (coalton-impl/typechecker::with-type-context ("COALTON-TOPLEVEL")
              (error 'duplicate-type-declaration
                     :name name))
            (setf (gethash name table) declared-kind))))
    table))

(defun parse-type-declaration (declaration env)
  (declare (type environment env))
  (assert (and (eql 'coalton:declare-type (first declaration))
               (= 3 (length declaration))
               (symbolp (second declaration))
               (or (listp (third declaration))
                   (symbolp (third declaration))))
      () "Malformed DECLARE-TYPE form ~A" declaration)
  (let* ((name (second declaration))
         (kind-expr (third declaration))
         (declared-kind (parse-kind env kind-expr)))

    (values name declared-kind)))
