(defpackage #:coalton-impl/typechecker/define-instance
  (:use
   #:cl
   #:coalton-impl/typechecker/base)
  (:import-from
   #:coalton-impl/typechecker/partial-type-env
   #:make-partial-type-env
   #:partial-type-env-add-var)
  (:import-from
   #:coalton-impl/typechecker/parse-type
   #:infer-predicate-kinds)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:toplevel-define-instance                    ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/define-instance)

;;; TODO: check for superclasses
;;; TODO: add the orphan check here

(defun toplevel-define-instance (instances env file)
  (declare (type parser:toplevel-define-instance-list instances)
           (type tc:environment env)
           (type coalton-file file)
           (values tc:environment))

  (loop :for instance :in instances
        :do (setf env (define-instance-in-environment instance env file)))

  env)

(defun define-instance-in-environment (instance env file)
  (declare (type parser:toplevel-define-instance instance)
           (type tc:environment env)
           (type coalton-file file)
           (values tc:environment))

  (let* ((unparsed-pred (parser:toplevel-define-instance-pred instance))

         (unparsed-context (parser:toplevel-define-instance-context instance))

         (partial-env (make-partial-type-env :env env)))

    ;; Define type variables in the environment
    (loop :for var :in (parser:collect-type-variables (list unparsed-pred unparsed-context))
          :do (partial-type-env-add-var partial-env (parser:tyvar-name var)))

    (let* ((pred (infer-predicate-kinds unparsed-pred nil partial-env file))

           (context (loop :for pred :in unparsed-context
                          :collect (infer-predicate-kinds pred nil partial-env file)))

           (class-name (tc:ty-predicate-class pred))

           (class (tc:lookup-class env class-name))

           (instance-codegen-sym
             (alexandria:format-symbol
              *package*
              "INSTANCE/~A"
              (with-output-to-string (s)
                (tc:with-pprint-variable-context ()
                  (let ((*print-escape* t))
                    (tc:pprint-predicate s pred))))))

           (method-names (mapcar #'car (tc:ty-class-unqualified-methods class)))

           (method-codegen-syms
             (loop :with table := (make-hash-table :test #'eq)
                   :for method-name :in method-names
                   :do (setf (gethash method-name table)
                             (alexandria:format-symbol
                              *package*
                              "~A-~S"
                              instance-codegen-sym
                              method-name))
                   :finally (return table)))

           (instance-entry
             (tc:make-ty-class-instance
              :constraints context
              :predicate pred
              :codegen-sym instance-codegen-sym
              :method-codegen-syms method-codegen-syms)))

      (when (tc:ty-class-fundeps class)
        (setf env (tc:update-instance-fundeps env pred)))

      (setf env (tc:add-instance env class-name instance-entry))

      env)))
