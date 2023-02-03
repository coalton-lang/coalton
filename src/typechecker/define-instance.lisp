(defpackage #:coalton-impl/typechecker/define-instance
  (:use
   #:cl
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/expression
   #:coalton-impl/typechecker/toplevel)
  (:import-from
   #:coalton-impl/typechecker/partial-type-env
   #:make-partial-type-env
   #:partial-type-env-add-var)
  (:import-from
   #:coalton-impl/typechecker/parse-type
   #:infer-predicate-kinds)
  (:import-from
   #:coalton-impl/typechecker/define
   #:make-tc-env
   #:infer-expl-binding-type)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:toplevel-define-instance           ; FUNCTION
   #:toplevel-typecheck-instance        ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/define-instance)

;;; TODO: other environment modifications are needed here !!!
;;; TODO: add the orphan check here
;;; TODO: handle addressable/runtime-repr instances

(defun toplevel-define-instance (instances env file)
  (declare (type parser:toplevel-define-instance-list instances)
           (type tc:environment env)
           (type coalton-file file)
           (values tc:ty-class-instance-list tc:environment))

  (values
   (loop :for instance :in instances
         :collect (multiple-value-bind (instance env_)
                      (define-instance-in-environment instance env file)
                    (setf env env_)
                    instance))

   env))

(defun toplevel-typecheck-instance (instances unparsed-instances env file)
  (declare (type tc:ty-class-instance-list instances)
           (type parser:toplevel-define-instance-list unparsed-instances)
           (type tc:environment env)
           (type coalton-file file)
           (values toplevel-define-instance-list))

  (loop :for instance :in instances
        :for unparsed-instance :in unparsed-instances
        :collect (typecheck-instance instance unparsed-instance env file)))

(defun define-instance-in-environment (instance env file)
  (declare (type parser:toplevel-define-instance instance)
           (type tc:environment env)
           (type coalton-file file)
           (values tc:ty-class-instance tc:environment))

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

           (class (tc:lookup-class env class-name)))

      (let* ((instance-codegen-sym
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

        (values instance-entry env)))))

(defun typecheck-instance (instance unparsed-instance env file)
  (declare (type tc:ty-class-instance instance)
           (type parser:toplevel-define-instance unparsed-instance)
           (type tc:environment env)
           (type coalton-file file))

  (let* ((pred (tc:ty-class-instance-predicate instance))

         (context (tc:ty-class-instance-constraints instance))

         (class-name (tc:ty-predicate-class pred))

         (class (tc:lookup-class env class-name))

         (class-pred (tc:ty-class-predicate class))

         (instance-subs (tc:predicate-match class-pred pred))

         ;; TODO: store this as a hash table in the environment
         ;; TODO: these should *not* be stored as schemes
         (method-table
           (loop :with table := (make-hash-table :test #'eq)
                 :for (name . scheme) :in (tc:ty-class-unqualified-methods class)
                 :do (setf (gethash name table) scheme)
                 :finally (return table))))

    ;; Check for superclasses and check the context of superinstances
    (loop :for superclass_ :in (tc:ty-class-superclasses class)

          :for superclass := (tc:apply-substitution instance-subs superclass_)

          :for superclass-instance
            := (or (tc:lookup-class-instance
                    env
                    superclass
                    :no-error t)
                   (error 'tc-error
                          :err (coalton-error
                                :span (parser:toplevel-define-instance-head-src unparsed-instance)
                                :file file
                                :message "Instance missing context"
                                :primary-note (format nil "No instance for ~S"
                                                      superclass))))

          :for additional-context
            := (tc:apply-substitution
                (tc:predicate-match
                 (tc:apply-substitution instance-subs (tc:ty-class-instance-predicate superclass-instance))
                 superclass)
                (tc:ty-class-instance-constraints superclass-instance))

          :do (loop :for pred :in additional-context
                    :do (unless (tc:entail env context pred)
                          (error 'tc-error
                                 :err (coalton-error
                                       :span (parser:toplevel-define-instance-head-src unparsed-instance)
                                       :file file
                                       :message "Instance missing context"
                                       :primary-note
                                       (format nil
                                               "No instance for ~S arising from constraints of superclasses ~S"
                                               pred
                                               superclass))))))

    (check-duplicates
     (parser:toplevel-define-instance-methods unparsed-instance)
     (alexandria:compose #'parser:node-variable-name #'parser:instance-method-definition-name)
     (lambda (first second)
       (error 'tc-error
              :err (coalton-error
                    :span (parser:instance-method-definition-source first)
                    :file file
                    :message "Duplicate method definition"
                    :primary-note "first definition here"
                    :notes
                    (list
                     (make-coalton-error-note
                      :type :primary
                      :span (parser:instance-method-definition-source second)
                      :message "second definition here"))))))

    ;; Ensure each method is part for the class
    (loop :for method :in (parser:toplevel-define-instance-methods unparsed-instance)
          :for name := (parser:node-variable-name (parser:instance-method-definition-name method))

          :unless (gethash name method-table)
            :do (error 'tc-error
                       :err (coalton-error
                             :span (parser:instance-method-definition-source method)
                             :file file
                             :message "Unknown method"
                             :primary-note (format nil "The method ~S is not part of class ~S"
                                                   name
                                                   class-name))))

    (let* ((methods (loop :with table := (make-hash-table :test #'eq)

                          :for method :in (parser:toplevel-define-instance-methods unparsed-instance)
                          :for name := (parser:node-variable-name (parser:instance-method-definition-name method))

                          :for class-method-scheme := (gethash name method-table)
                          :for class-method-qual-ty := (tc:fresh-inst class-method-scheme)
                          :for class-method-constraints := (tc:qualified-ty-predicates class-method-qual-ty)
                          :for class-method-ty := (tc:qualified-ty-type class-method-qual-ty)

                          ;; NOTE: Instance methods type still do not contain the class predicate
                          :for instance-method-context := (append context class-method-constraints)
                          :for instance-method-qual-ty
                            := (tc:apply-substitution instance-subs (tc:qualify instance-method-context class-method-ty))
                          :for instance-method-scheme := (tc:quantify
                                                          (tc:type-variables instance-method-qual-ty)
                                                          instance-method-qual-ty)

                          :do (multiple-value-bind (preds method subs)
                                  (infer-expl-binding-type method
                                                           instance-method-scheme
                                                           (parser:instance-method-definition-source method)
                                                           nil
                                                           (make-tc-env :env env)
                                                           file)

                                ;; Deferred predicates should always be null
                                (unless (null preds)
                                  (util:coalton-bug "Instance definition predicates should not be null."))

                                ;; Unify each predicate in the inferred type with the
                                ;; instance context to ensure the type variables match
                                ;; those in the context.
                                (loop :for context-pred :in instance-method-context
                                      :for node-pred :in (tc:qualified-ty-predicates
                                                          (node-type
                                                           (instance-method-definition-name method)))
                                      :do (setf subs (tc:compose-substitution-lists
                                                      (tc:predicate-match node-pred context-pred)
                                                      subs)))

                                (setf (gethash name table) (tc:apply-substitution subs method)))

                          :finally (return table))))

      (make-toplevel-define-instance
       :context context
       :pred pred
       :methods methods
       :source (parser:toplevel-define-instance-source unparsed-instance)
       :head-src (parser:toplevel-define-instance-head-src unparsed-instance)))))
