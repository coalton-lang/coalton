(defpackage #:coalton-impl/typechecker/define-instance
  (:use
   #:cl
   #:coalton-impl/typechecker/expression
   #:coalton-impl/typechecker/toplevel)
  (:import-from
   #:coalton-impl/typechecker/base
   #:check-duplicates)
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
   (#:se #:source-error)
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:toplevel-define-instance           ; FUNCTION
   #:toplevel-typecheck-instance        ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/define-instance)

(defun toplevel-define-instance (instances env file)
  (declare (type parser:toplevel-define-instance-list instances)
           (type tc:environment env)
           (type se:file file)
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
           (type se:file file)
           (values toplevel-define-instance-list))

  (loop :for instance :in instances
        :for unparsed-instance :in unparsed-instances
        :collect (typecheck-instance instance unparsed-instance env file)))

(defun define-instance-in-environment (instance env file)
  (declare (type parser:toplevel-define-instance instance)
           (type tc:environment env)
           (type se:file file)
           (values tc:ty-class-instance tc:environment))

  (check-for-orphan-instance instance file)
  (check-instance-valid instance file)

  (let* ((unparsed-pred (parser:toplevel-define-instance-pred instance))

         (unparsed-context (parser:toplevel-define-instance-context instance))

         (partial-env (make-partial-type-env :env env)))

    ;; Define type variables in the environment
    (loop :for var :in (parser:collect-type-variables (list unparsed-pred unparsed-context))
          :do (partial-type-env-add-var partial-env (parser:tyvar-name var)))

    (let* ((ksubs nil)

           (pred (multiple-value-bind (pred ksubs_)
                     (infer-predicate-kinds unparsed-pred ksubs partial-env file)
                   (setf ksubs ksubs_)
                   pred))

           (context (loop :for pred :in unparsed-context
                          :collect (multiple-value-bind (pred ksubs_)
                                       (infer-predicate-kinds pred ksubs partial-env file)
                                     (setf ksubs ksubs_)
                                     pred)))

           (class-name (tc:ty-predicate-class pred))

           (class (tc:lookup-class env class-name))

           (pred (tc:apply-ksubstitution ksubs pred))
           (context (tc:apply-ksubstitution ksubs context))
           (ksubs (tc:kind-monomorphize-subs (tc:kind-variables (cons pred context)) ksubs))
           (pred (tc:apply-ksubstitution ksubs pred))
           (context (tc:apply-ksubstitution ksubs context)))

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
               (loop :with table := (tc:make-map :test 'eq)
                     :for method-name :in method-names
                     :do (setf (tc:get-value table method-name)
                               (alexandria:format-symbol *package* "~A-~S"
                                                         instance-codegen-sym
                                                         method-name))
                     :finally (return table)))

             (docstring (parser:toplevel-define-instance-docstring instance))

             (instance-entry
               (tc:make-ty-class-instance
                :constraints context
                :predicate pred
                :codegen-sym instance-codegen-sym
                :method-codegen-syms method-codegen-syms
                :docstring docstring)))

        (cond (context
               (setf env (tc:set-function env instance-codegen-sym (tc:make-function-env-entry
                                                                    :name instance-codegen-sym
                                                                    :arity (length context)))))
              ((tc:lookup-function env instance-codegen-sym :no-error t)
               (setf env (tc:unset-function env instance-codegen-sym))))

        (when (tc:ty-class-fundeps class)
          (handler-case 
              (setf env (tc:update-instance-fundeps env pred))
            (tc:fundep-conflict (e)
              (error 'tc:tc-error
                     :err (se:source-error
                           :span (parser:toplevel-define-instance-head-src instance)
                           :file file
                           :message "Instance fundep conflict"
                           :primary-note (let ((*print-escape* nil))
                                           (with-output-to-string (s)
                                             (print-object e s))))))))

        (handler-case
            (setf env (tc:add-instance env class-name instance-entry))
          (tc:overlapping-instance-error (e)
            (error 'tc:tc-error
                   :err (se:source-error
                         :span (parser:toplevel-define-instance-head-src instance)
                         :file file
                         :message "Overlapping instance"
                         :primary-note (format nil "instance overlaps with ~S" (tc:overlapping-instance-error-inst2 e))))))

        (loop :for method-name :in method-names
              :for method-codegen-sym := (tc:get-value method-codegen-syms method-name) :do
                (setf env (tc:set-method-inline env method-name instance-codegen-sym method-codegen-sym)))

        (values instance-entry env)))))

(defun typecheck-instance (instance unparsed-instance env file)
  (declare (type tc:ty-class-instance instance)
           (type parser:toplevel-define-instance unparsed-instance)
           (type tc:environment env)
           (type se:file file))

  (let* ((pred (tc:ty-class-instance-predicate instance))

         (context (tc:ty-class-instance-constraints instance))

         (class-name (tc:ty-predicate-class pred))

         (class (tc:lookup-class env class-name))

         (class-pred (tc:ty-class-predicate class))

         (instance-subs (tc:predicate-match class-pred pred))

         ;; HACK: store this as a hash table in the environment
         ;; HACK: these should *not* be stored as schemes
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
                   (error 'tc:tc-error
                          :err (se:source-error
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
                          (error 'tc:tc-error
                                 :err (se:source-error
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
     #'parser:instance-method-definition-source
     (lambda (first second)
       (error 'tc:tc-error
              :err (se:source-error
                    :span (parser:instance-method-definition-source first)
                    :file file
                    :message "Duplicate method definition"
                    :primary-note "first definition here"
                    :notes
                    (list
                     (se:make-source-error-note
                      :type :primary
                      :span (parser:instance-method-definition-source second)
                      :message "second definition here"))))))

    ;; Ensure each method is part for the class
    (loop :for method :in (parser:toplevel-define-instance-methods unparsed-instance)
          :for name := (parser:node-variable-name (parser:instance-method-definition-name method))

          :unless (gethash name method-table)
            :do (error 'tc:tc-error
                       :err (se:source-error
                             :span (parser:instance-method-definition-source method)
                             :file file
                             :message "Unknown method"
                             :primary-note (let ((*package* util:+keyword-package+))
                                             (format nil "The method ~S is not part of class ~S"
                                                     name
                                                     class-name)))))

    ;; Ensure each method is defined
    (loop :for name :being :the :hash-keys :of method-table
          :for method := (find name (parser:toplevel-define-instance-methods unparsed-instance)
                               :key (alexandria:compose #'parser:node-variable-name
                                                        #'parser:instance-method-definition-name))
          :unless method
            :do (error 'tc:tc-error
                       :err (se:source-error
                             :span (parser:toplevel-define-instance-source unparsed-instance)
                             :file file
                             :message "Missing method"
                             :primary-note (format nil "The method ~S is not defined"
                                                   name))))

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

(defun check-instance-valid (instance file)
  (declare (type parser:toplevel-define-instance instance)
           (type se:file file))

  ;; Instance validation is disabled for compiler generated instances
  (when (parser:toplevel-define-instance-compiler-generated instance)
    (return-from check-instance-valid))

  (let* ((types-package (util:find-package "COALTON-LIBRARY/TYPES"))

         (runtime-repr (util:find-symbol "RUNTIMEREPR" types-package)))

    ;; Instance validation is disabled in the types package
    (when (eq *package* types-package)
      (return-from check-instance-valid))


    (when (eq (parser:identifier-src-name (parser:ty-predicate-class (parser:toplevel-define-instance-pred instance))) runtime-repr)
      (error 'tc:tc-error
             :err (se:source-error
                   :span (parser:toplevel-define-instance-head-src instance)
                   :file file
                   :message "Invalid instance"
                   :primary-note "RuntimeRepr instances cannot be written manually")))))

(defun check-for-orphan-instance (instance file)
  (declare (type parser:toplevel-define-instance instance) 
           (type se:file file)
           (values null))


  ;; Stage-1 packages skip the orphan instance check
  (when (find *package* (list (find-package "COALTON-LIBRARY/TYPES")
                              (find-package "COALTON-LIBRARY/FIXED-SIZE-NUMBERS")
                              (find-package "COALTON-LIBRARY/CLASSES")
                              (find-package "COALTON-LIBRARY/HASH")
                              (find-package "COALTON-LIBRARY/BUILTIN")
                              (find-package "COALTON-LIBRARY/FUNCTIONS")
                              (find-package "COALTON-LIBRARY/BOOLEAN")
                              (find-package "COALTON-LIBRARY/BITS")
                              (find-package "COALTON-LIBRARY/MATH/ARITH")
                              (find-package "COALTON-LIBRARY/MATH/NUM")
                              (find-package "COALTON-LIBRARY/MATH/BOUNDED")
                              (find-package "COALTON-LIBRARY/MATH/CONVERSIONS")
                              (find-package "COALTON-LIBRARY/MATH/FRACTION")
                              (find-package "COALTON-LIBRARY/MATH/INTEGRAL")
                              (find-package "COALTON-LIBRARY/MATH/REAL")
                              (find-package "COALTON-LIBRARY/MATH/COMPLEX")
                              (find-package "COALTON-LIBRARY/MATH/ELEMENTARY")
                              (find-package "COALTON-LIBRARY/MATH/DYADIC")
                              (find-package "COALTON-LIBRARY/CHAR")
                              (find-package "COALTON-LIBRARY/STRING")
                              (find-package "COALTON-LIBRARY/TUPLE")
                              (find-package "COALTON-LIBRARY/OPTIONAL")
                              (find-package "COALTON-LIBRARY/LIST")
                              (find-package "COALTON-LIBRARY/RESULT")))
    (return-from check-for-orphan-instance))
  
  (let ((instance-syms
          (cons
           (parser:identifier-src-name (parser:ty-predicate-class (parser:toplevel-define-instance-pred instance)))
           (loop :for type :in (parser:ty-predicate-types (parser:toplevel-define-instance-pred instance))
                 :append (mapcar #'parser:tycon-name (parser:collect-referenced-types type))))))

    (unless (find *package* instance-syms :key #'symbol-package)
      (error 'tc:tc-error
             :err (se:source-error
                   :span (parser:toplevel-define-instance-head-src instance)
                   :file file
                   :message "Invalid orphan instance"
                   :primary-note "instances must be defined in the same package as their class or reference one or more types in their defining package")))))
