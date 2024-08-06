(defpackage #:coalton-impl/typechecker/tc-env
  (:use
   #:cl
   #:coalton-impl/typechecker/parse-type)
  (:local-nicknames
   (#:se #:source-error)
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:make-tc-env                        ; CONSTRUCTOR
   #:tc-env                             ; STRUCT
   #:tc-env-env                         ; ACCESSOR
   #:tc-env-ty-table                    ; ACCESSOR
   #:tc-env-add-variable                ; FUNCTION
   #:tc-env-lookup-value                ; FUNCTION
   #:tc-env-add-definition              ; FUNCTION
   #:tc-env-bound-variables             ; FUNCTION
   #:tc-env-bindings-variables          ; FUNCTION
   #:tc-env-replace-type                ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/tc-env)

;;;
;;; Typechecking Environment
;;;

(defstruct (tc-env
            (:predicate nil))

  ;; The main compiler env
  (env      (util:required 'env)          :type tc:environment :read-only t)

  ;; Hash table mapping variables bound in the current translation unit to types
  (ty-table (make-hash-table :test #'eq)  :type hash-table     :read-only t))

(defun tc-env-add-variable (env name)
  "Add a variable named NAME to ENV and return the scheme."
  (declare (type tc-env env)
           (type symbol name)
           (values tc:tyvar))

  (when (gethash name (tc-env-ty-table env))
    (util:coalton-bug "Attempt to add already defined variable with name ~S." name))

  (tc:qualified-ty-type (tc:fresh-inst (setf (gethash name (tc-env-ty-table env)) (tc:to-scheme (tc:make-variable))))))

(defun tc-env-suggest-value (env name)
  "If value lookup failed, generate suggestions for what to do, if anything."
  (declare (type tc-env env)
           (type symbol name)
           (values util:string-list &optional))
  (let ((suggestions nil))
    ;; If the symbol names a type, user may have intended to use a type constructor
    (let ((type (tc:lookup-type (tc-env-env env) name :no-error t)))
      (when type
        (push (format nil "Did you mean a constructor of type ~A?" (tc:type-entry-name type))
              suggestions)))
    (nreverse suggestions)))

(defun tc-env-lookup-value (env var)
  "Lookup the type of a variable named VAR in ENV."
  (declare (type tc-env env)
           (type parser:node-variable var)
           (values tc:ty tc:ty-predicate-list))

  (let* ((var-name (parser:node-variable-name var))
         (scheme (or (gethash var-name (tc-env-ty-table env))
                     (tc:lookup-value-type (tc-env-env env) var-name :no-error t))))
    (unless scheme
      ;; Variable is unbound: create an error
      (error 'tc:tc-error
             :err (parser:source-error
                   :source (parser:node-source var)
                   :message "Unknown variable"
                   :primary-note "unknown variable"
                   :help-notes (loop :for suggestion :in (tc-env-suggest-value env var-name)
                                     :collect (se:make-source-error-help
                                               :span (parser:source-location-span (parser:node-source var))
                                               :replacement #'identity
                                               :message suggestion)))))
    (let ((qualified-type (tc:fresh-inst scheme)))
      (values (tc:qualified-ty-type qualified-type)
              (loop :for pred :in (tc:qualified-ty-predicates qualified-type)
                    :collect (tc:make-ty-predicate :class (tc:ty-predicate-class pred)
                                                   :types (tc:ty-predicate-types pred)
                                                   :source (parser:node-source var)))))))

(defun tc-env-add-definition (env name scheme)
  "Add a type named NAME to ENV."
  (declare (type tc-env env)
           (type symbol name)
           (type tc:ty-scheme scheme)
           (values null))
  (when (gethash name (tc-env-ty-table env))
    (util:coalton-bug "Attempt to add already defined type with name ~S." name))
  (setf (gethash name (tc-env-ty-table env)) scheme)
  nil)

(defun tc-env-bound-variables (env)
  (declare (type tc-env env)
           (values util:symbol-list &optional))
  (alexandria:hash-table-keys (tc-env-ty-table env)))

(defun tc-env-bindings-variables (env names)
  (declare (type tc-env env)
           (type util:symbol-list names)
           (values tc:tyvar-list))

  (remove-duplicates
   (loop :with table := (tc-env-ty-table env)
         :for name :in names
         :for ty := (gethash name table)
         :unless ty
           :do (util:coalton-bug "Unknown binding ~A" name)
         :append (tc:type-variables ty))
   :test #'eq))

(defun tc-env-replace-type (env name scheme)
  (declare (type tc-env env)
           (type symbol name)
           (type tc:ty-scheme scheme)
           (values null))

  (unless (gethash name (tc-env-ty-table env))
    (util:coalton-bug "Attempt to replace unknown type ~S" name))

  (setf (gethash name (tc-env-ty-table env)) scheme)

  nil)

(defmethod tc:apply-substitution (subs (env tc-env))
  "Applies SUBS to the types currently being checked in ENV. Does not update the types in the inner main environment because there should not be substitutions for them."
  (maphash
   (lambda (key value)
     (setf (gethash key (tc-env-ty-table env)) (tc:apply-substitution subs value)))
   (tc-env-ty-table env)))

(defmethod tc:type-variables ((env tc-env))
  "Returns all of the type variables of the types being checked in ENV. Does not return type variables from the inner main environment because it should not contain any free type variables."
  (loop :for ty :being :the :hash-values :of (tc-env-ty-table env)
        :append (tc:type-variables ty)))
