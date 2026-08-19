(defpackage #:coalton-impl/typechecker/tc-env
  (:use
   #:cl
   #:coalton-impl/typechecker/parse-type
   #:coalton-impl/typechecker/partial-type-env)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:make-tc-env                        ; CONSTRUCTOR
   #:tc-env                             ; STRUCT
   #:tc-env-env                         ; ACCESSOR
   #:tc-env-ty-table                    ; ACCESSOR
   #:tc-env-typevar-table               ; ACCESSOR
   #:tc-env-add-variable                ; FUNCTION
   #:tc-env-lookup-value                ; FUNCTION
   #:tc-env-add-definition              ; FUNCTION
   #:tc-env-bound-variables             ; FUNCTION
   #:tc-env-bindings-variables          ; FUNCTION
   #:tc-env-parser-env                  ; FUNCTION
   #:tc-env-extend-type-variable-scope  ; FUNCTION
   #:tc-env-shadow-definition           ; FUNCTION
   #:tc-env-replace-type                ; FUNCTION
   #:tc-env-lookup-value-symbol         ; FUNCTION
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
  (ty-table (make-hash-table :test #'eq)  :type hash-table     :read-only t)

  ;; Hash table mapping scoped type variable names to the lexical
  ;; type variables they denote.
  (typevar-table (make-hash-table :test #'eq) :type hash-table :read-only t))

(defun tc-env-scoped-type-variables (env)
  (declare (type tc-env env)
           (values tc:tyvar-list &optional))
  (remove-duplicates
   (loop :for type :being :the :hash-values :of (tc-env-typevar-table env)
         :append (tc:type-variables type))
   :test #'tc:ty=))

(defun tc-env-parser-env (env)
  (declare (type tc-env env)
           (values partial-type-env &optional))
  (let ((partial-env (make-partial-type-env :env (tc-env-env env))))
    (maphash
     (lambda (name tyvar)
       (setf (gethash name (partial-type-env-ty-table partial-env)) tyvar))
     (tc-env-typevar-table env))
    partial-env))

(defun tc-env-extend-type-variable-scope (env tyvars)
  (declare (type tc-env env)
           (type tc:tyvar-list tyvars)
           (values tc-env &optional))
  (let ((typevar-table (alexandria:copy-hash-table (tc-env-typevar-table env))))
    (loop :for tyvar :in tyvars
          :for source-name := (tc:tyvar-source-name tyvar)
          :when source-name
            :do (setf (gethash source-name typevar-table) tyvar))
    (make-tc-env :env (tc-env-env env)
                 :ty-table (tc-env-ty-table env)
                 :typevar-table typevar-table)))

(defun tc-env-shadow-definition (env name scheme)
  "Return a copy of ENV where NAME resolves to SCHEME.

This is used when checking an explicitly typed binding so recursive
references within that binding reuse the same instantiated scoped
type variables instead of re-instantiating the declared scheme."
  (declare (type tc-env env)
           (type symbol name)
           (type tc:ty-scheme scheme)
           (values tc-env &optional))
  (let ((ty-table (alexandria:copy-hash-table (tc-env-ty-table env))))
    (setf (gethash name ty-table) scheme)
    (make-tc-env :env (tc-env-env env)
                 :ty-table ty-table
                 :typevar-table (tc-env-typevar-table env))))

(defun tc-env-add-variable (env name &key (allow-result-p nil))
  "Add a variable named NAME to ENV and return the scheme."
  (declare (type tc-env env)
           (type symbol name)
           (values tc:tyvar))

  (when (gethash name (tc-env-ty-table env))
    (util:coalton-bug "Attempt to add already defined variable with name ~S." name))

  (tc:qualified-ty-type
   (tc:fresh-inst
    (setf (gethash name (tc-env-ty-table env))
          (tc:to-scheme (tc:make-variable :kind tc:+kstar+
                                          :allow-result-p allow-result-p))))))

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
      (apply #'tc:tc-error (format nil "Unknown variable ~a" var-name)
             (cons (source:note (source:location var)
                                (format nil "unknown variable ~a" var-name))
                   (loop :for suggestion :in (tc-env-suggest-value env var-name)
                         :collect (source:help (source:location var) #'identity suggestion)))))
    (let ((qualified-type (tc:fresh-inst scheme)))
      (values (tc:qualified-ty-type qualified-type)
              (loop :for pred :in (tc:qualified-ty-predicates qualified-type)
                    :collect (tc:make-ty-predicate :class (tc:ty-predicate-class pred)
                                                   :types (tc:ty-predicate-types pred)
                                                   :location (source:location var)))))))

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
   (append
    (loop :with table := (tc-env-ty-table env)
          :for name :in names
          :for ty := (gethash name table)
          :unless ty
            :do (util:coalton-bug "Unknown binding ~A" name)
          :append (tc:type-variables ty))
    (tc-env-scoped-type-variables env))
   :test #'tc:ty=))

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
   (tc-env-ty-table env))
  (maphash
   (lambda (key value)
     (setf (gethash key (tc-env-typevar-table env))
           (tc:apply-substitution subs value)))
   (tc-env-typevar-table env)))

(defmethod tc:type-variables ((env tc-env))
  "Returns all of the type variables of the types being checked in ENV. Does not return type variables from the inner main environment because it should not contain any free type variables."
  (remove-duplicates
   (append
    (loop :for ty :being :the :hash-values :of (tc-env-ty-table env)
          :append (tc:type-variables ty))
    (tc-env-scoped-type-variables env))
   :test #'tc:ty=))

(defun tc-env-lookup-value-symbol (env sym loc)
  (declare (type tc-env env)
           (type symbol sym)
           (type (or source:location null) loc)
           (values tc:ty tc:ty-predicate-list))

  (let* ((scheme (or (gethash sym (tc-env-ty-table env))
                     (tc:lookup-value-type (tc-env-env env) sym :no-error t))))
    (unless scheme
      (util:coalton-bug "Unknown variable ~a" sym))
    (let ((q (tc:fresh-inst scheme)))
      (values (tc:qualified-ty-type q)
              (loop :for pred in (tc:qualified-ty-predicates q)
                    :collect (tc:make-ty-predicate
                              :class (tc:ty-predicate-class pred)
                              :types (tc:ty-predicate-types pred)
                              :location loc))))))
