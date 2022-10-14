(defpackage #:coalton-impl/typechecker/tc-env
  (:use
   #:cl
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/parse-type)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error)
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

  ;; The main copiler env
  (env      (util:required 'env)          :type tc:environment :read-only t)

  ;; Hash table mappinig variables bound in the current translation unit to types
  (ty-table (make-hash-table :test #'eq)  :type hash-table     :read-only t))

(defun tc-env-add-variable (env name)
  "Add a variable named NAME to ENV and return the scheme."
  (declare (type tc-env env)
           (type symbol name)
           (values tc:tyvar))

  (when (gethash name (tc-env-ty-table env))
    (util:coalton-bug "Attempt to add already defined variable with name ~S." name))

  (tc:qualified-ty-type (tc:fresh-inst (setf (gethash name (tc-env-ty-table env)) (tc:to-scheme (tc:make-variable))))))

(defun tc-env-lookup-value (env var file)
  "Lookup a variable named VAR in ENV."
  (declare (type tc-env env)
           (type parser:node-variable var)
           (type coalton-file file)
           (values tc:ty tc:ty-predicate-list))


  (let* ((scheme (or (gethash (parser:node-variable-name var) (tc-env-ty-table env))

                     (tc:lookup-value-type (tc-env-env env) (parser:node-variable-name var) :no-error t)

                     ;; Binding is unknown. Create an error.
                     (let* ((sym-name (symbol-name (parser:node-variable-name var)))
                            (matches (append
                                      (remove-if-not
                                       (lambda (s) (string= (symbol-name s) sym-name))
                                       (alexandria:hash-table-keys (tc-env-ty-table env)))
                                      (remove-if-not
                                       (lambda (s) (string= (symbol-name s) sym-name))
                                       (coalton-impl/algorithm::immutable-map-keys
                                        (tc:environment-value-environment (tc-env-env env)))))))
                       (error 'tc-error
                              :err (coalton-error
                                    :span (parser:node-source var)
                                    :file file
                                    :message "Unknown variable"
                                    :primary-note "unknown variable"
                                    :help-notes (mapcar
                                                 (lambda (symbol)
                                                   (error:make-coalton-error-help
                                                    :span (parser:node-source var)
                                                    :replacement (lambda (s)
                                                                   (declare (ignore s))
                                                                   (format nil "~S" symbol))
                                                    :message (format nil "Did you mean ~S?" symbol)))
                                                 matches))))))

         (qual-ty (tc:fresh-inst scheme))

         (ty (tc:qualified-ty-type qual-ty))

         (preds (tc:qualified-ty-predicates qual-ty)))

    (values
     ty
     (loop :for pred :in preds
           :collect (tc:make-ty-predicate
                     :class (tc:ty-predicate-class pred)
                     :types (tc:ty-predicate-types pred)
                     :source (parser:node-source var))))))

(defun tc-env-add-definition (env name scheme)
  "Add a type named NAME to ENV."
  (declare (type tc-env env)
           (type symbol name)
           (type tc:ty-scheme scheme)
           (values null))

  (when (gethash name (tc-env-ty-table env))
    (util:coalton-bug "Attemt to add already defined type with name ~S." name))

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
    (util:coalton-bug "Attempt to replace unknown type ~A" name))

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
