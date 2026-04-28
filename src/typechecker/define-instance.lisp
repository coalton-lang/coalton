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
   #:coalton-impl/typechecker/tc-env
   #:tc-env-extend-type-variable-scope)
  (:import-from
   #:coalton-impl/typechecker/define
   #:make-tc-env
   #:check-bindings-for-invalid-recursion
   #:infer-expl-binding-type)
  (:local-nicknames
   (#:a #:alexandria)
   (#:settings #:coalton-impl/settings)
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker/stage-1)
   (#:type-string #:coalton-impl/typechecker/type-string))
  (:export
   #:toplevel-define-instance           ; FUNCTION
   #:toplevel-typecheck-instance        ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/define-instance)

(defun type-object-string (object env)
  (let ((settings:*coalton-print-unicode* nil))
    (type-string:type-to-string object env)))

(defun instance-scoped-method-substitutions (outer-method-types instance-scoped-tvars)
  (declare (type list outer-method-types)
           (type tc:tyvar-list instance-scoped-tvars)
           (values tc:substitution-list &optional))
  (let ((scoped-tvar-table (make-hash-table :test #'eq)))
    (loop :for scoped-tvar :in instance-scoped-tvars
          :for source-name := (tc:tyvar-source-name scoped-tvar)
          :when source-name
            :do (setf (gethash source-name scoped-tvar-table) scoped-tvar))
    (loop :for method-tvar :in (tc:type-variables outer-method-types)
          :for source-name := (tc:tyvar-source-name method-tvar)
          :for scoped-tvar := (and source-name
                                   (gethash source-name scoped-tvar-table))
          :when (and scoped-tvar
                     (not (tc:ty= method-tvar scoped-tvar)))
            :collect (tc:make-substitution
                      :from method-tvar
                      :to scoped-tvar))))

(defun check-instance-predicate-not-narrowed (method pred context subs env)
  "Reject instance methods that narrow the instance predicate.

The instance head and its context are implicitly universally quantified over
the whole instance. Method checking may alpha-rename those variables, but it
must not collapse distinct variables or specialize one to a concrete type.
"
  (declare (type instance-method-definition method)
           (type tc:ty-predicate pred)
           (type tc:ty-predicate-list context)
           (type tc:substitution-list subs)
           (type tc:environment env)
           (values null))
  (let* ((sentinel (tc:make-variable))
         (before (tc:predicates-to-scheme (cons pred context) sentinel))
         (narrowed-pred (tc:apply-substitution subs pred))
         (narrowed-context (tc:apply-substitution subs context))
         (after (tc:predicates-to-scheme (cons narrowed-pred narrowed-context)
                                         sentinel)))
    (unless (tc:ty-scheme= before after)
      (tc-error "Instance method type is too specific"
                (tc-location
                 (source:location (instance-method-definition-name method))
                 "The method definition narrows instance ~A to ~A."
                 (type-object-string pred env)
                 (type-object-string narrowed-pred env))))))

(defun toplevel-define-instance (instances env)
  (declare (type parser:toplevel-define-instance-list instances)
           (type tc:environment env)
           (values tc:ty-class-instance-list tc:environment))

  (values 
   (loop :for instance :in instances
         :collect (multiple-value-bind (instance env_)
                      (define-instance-in-environment instance env)
                    (setf env env_)
                    instance))
   env))

(defun toplevel-typecheck-instance (instances unparsed-instances env)
  (declare (type tc:ty-class-instance-list instances)
           (type parser:toplevel-define-instance-list unparsed-instances)
           (type tc:environment env)
           (values toplevel-define-instance-list))

  (loop :for instance :in instances
        :for unparsed-instance :in unparsed-instances
        :collect (typecheck-instance instance unparsed-instance env)))

(defun define-instance-in-environment (instance env)
  (declare (type parser:toplevel-define-instance instance)
           (type tc:environment env)
           (values tc:ty-class-instance tc:environment))

  (check-for-orphan-instance instance)
  (check-instance-valid instance)

  (let* ((unparsed-pred (parser:toplevel-define-instance-pred instance))

         (unparsed-context (parser:toplevel-define-instance-context instance))

         (partial-env (make-partial-type-env :env env)))

    ;; Define type variables in the environment
    (loop :for var :in (parser:collect-type-variables (list unparsed-pred unparsed-context))
          :do (partial-type-env-add-var partial-env
                                        (parser:tyvar-name var)
                                        (or (parser:tyvar-source-name var)
                                            (parser:tyvar-name var))))

    (let* ((ksubs nil)

           (pred (multiple-value-bind (pred ksubs_)
                     (infer-predicate-kinds unparsed-pred ksubs partial-env)
                   (setf ksubs ksubs_)
                   pred))

           (context (loop :for pred :in unparsed-context
                          :collect (multiple-value-bind (pred ksubs_)
                                       (infer-predicate-kinds pred ksubs partial-env)
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
               (a:format-symbol
                *package*
                "INSTANCE/~A"
                (with-output-to-string (s)
                  (tc:with-pprint-variable-context ()
                    (let ((*print-escape* t))
                      (tc:pprint-predicate s pred))))))

             (method-names (mapcar #'tc:ty-class-method-name
                                   (tc:ty-class-unqualified-methods class)))

             (method-codegen-syms (mapcar (lambda (method-name)
                                            (a:format-symbol *package* "~A-~S"
                                                             instance-codegen-sym
                                                             method-name))
                                          method-names))

             (method-codegen-inline-p
               (loop :for method-name :in method-names
                     :for method-codegen-sym :in method-codegen-syms
                     :for method-def := (find method-name (parser:toplevel-define-instance-methods instance)
                                              :key (lambda (method-def)
                                                     (parser:node-variable-name
                                                      (parser:instance-method-definition-name method-def))))
                     :unless (null method-def)
                       :collect (cons
                                 method-codegen-sym
                                 ;; Convert inline attribute to boolean.
                                 (and (parser:instance-method-definition-inline method-def) t))))

             (instance-entry
               (tc:make-ty-class-instance
                :constraints context
                :predicate pred
                :codegen-sym instance-codegen-sym
                :method-codegen-syms method-codegen-syms
                :method-codegen-inline-p method-codegen-inline-p
                :docstring (source:docstring instance)
                :location (parser:toplevel-define-instance-head-location instance))))

        (cond (context
               (setf env (tc:set-function env instance-codegen-sym (tc:make-function-env-entry
                                                                    :name instance-codegen-sym
                                                                    :arity (length context)
                                                                    :inline-p nil))))
              ((tc:lookup-function env instance-codegen-sym :no-error t)
               (setf env (tc:unset-function env instance-codegen-sym))))

        (when (consp (tc:ty-class-fundeps class))
          (handler-case
              (setf env (tc:update-instance-fundeps env pred context))
            (tc:fundep-ambiguity (e)
              ;; In this case, a instance definition is ambiguous
              ;; because a dependent type is too general.
              ;; For example,
              ;;   (define-class (C :a :b (:a -> :b)))
              ;;   (define-instance (C :c :d))
              ;; Here, the dependent type (:d) contains a type variable
              ;; that is not in the determinant type (:c), but the
              ;; underlying principle of functional dependencies requires
              ;; that if we know the determinant type, then we should also
              ;; know the dependent type, as we would in the following case,
              ;;   (define-instance (C :c (List :c))
              (tc-error "Instance fundep ambiguity"
                        (tc-location (parser:toplevel-define-instance-head-location instance)
                                     (let ((*print-escape* nil))
                                       (with-output-to-string (s)
                                         (print-object e s))))))
            (tc:fundep-conflict (e)
              ;; In thid case, an instance simply conflicts with another on
              ;; the basis of functional dependencies.
              ;; For example,
              ;;   (define-class (C :a :b (:a -> :b)))
              ;;   (define-instance (C T U))
              ;;   (define-instance (C T V)) ; Conflicts with the previous above.
              (tc-error "Instance fundep conflict"
                        (tc-location (parser:toplevel-define-instance-head-location instance)
                                     (let ((*print-escape* nil))
                                       (with-output-to-string (s)
                                         (print-object e s))))))))

        (handler-case
            (setf env (tc:add-instance env class-name instance-entry))
          (tc:overlapping-instance-error (e)
            (tc-error "Overlapping instance"
                      (tc-location (parser:toplevel-define-instance-head-location instance)
                                   "instance overlaps with ~A"
                                   (type-object-string (tc:overlapping-instance-error-inst2 e) env)))))

        (loop :for method-name :in method-names
              :for method-codegen-sym :in method-codegen-syms :do
                (setf env (tc:set-method-inline env method-name instance-codegen-sym method-codegen-sym)))

        (values instance-entry env)))))

(defun typecheck-instance (instance unparsed-instance env)
  (declare (type tc:ty-class-instance instance)
           (type parser:toplevel-define-instance unparsed-instance)
           (type tc:environment env))

  (let* ((pred (tc:ty-class-instance-predicate instance))

         (context (tc:ty-class-instance-constraints-expanded instance env))

         (class-name (tc:ty-predicate-class pred))

         (class (tc:lookup-class env class-name))

         (class-pred (tc:ty-class-predicate class))

         (instance-subs (tc:predicate-match class-pred pred))

         ;; HACK: store this as a hash table in the environment
         ;; HACK: these should *not* be stored as schemes
         (method-table
           (loop :with table := (make-hash-table :test #'eq)
                 :for method :in (tc:ty-class-unqualified-methods class)
                 :do (setf (gethash (tc:ty-class-method-name method) table)
                           method)
                 :finally (return table))))

    ;; Check for superclasses and check the context of superinstances
    (loop :for superclass_ :in (tc:ty-class-superclasses class)

          :for superclass := (tc:apply-substitution instance-subs superclass_)

          :unless (tc:entail env context superclass)

            :do (loop :for superclass-instance
                        := (or (tc:lookup-class-instance
                                env
                                superclass
                                :no-error t)
                               (tc-error "Instance missing context"
                                         (tc-location (parser:toplevel-define-instance-head-location unparsed-instance)
                                                      "No instance for ~A"
                                                      (type-object-string superclass env))))

                      :for additional-context
                        := (tc:apply-substitution
                            (tc:predicate-match
                             (tc:apply-substitution instance-subs (tc:ty-class-instance-predicate superclass-instance))
                             superclass)
                            (tc:ty-class-instance-constraints-expanded superclass-instance env))

                      :do (loop :for pred :in additional-context
                                :do (unless (tc:entail env context pred)
                                      (tc-error "Instance missing context"
                                                (tc-location (parser:toplevel-define-instance-head-location unparsed-instance)
                                                             "No instance for ~A arising from constraints of superclasses ~A"
                                                             (type-object-string pred env)
                                                             (type-object-string superclass env)))))))

    (check-duplicates
     (parser:toplevel-define-instance-methods unparsed-instance)
     (a:compose #'parser:node-variable-name #'parser:instance-method-definition-name)
     (lambda (first second)
       (tc-error "Duplicate method definition"
                 (tc-note first "first definition here")
                 (tc-note second "second definition here"))))

    ;; Ensure each method is part for the class
    (loop :for method :in (parser:toplevel-define-instance-methods unparsed-instance)
          :for name := (parser:node-variable-name (parser:instance-method-definition-name method))

          :unless (gethash name method-table)
            :do (tc-error "Unknown method"
                          (tc-note method
                                   (let ((*package* util:+keyword-package+))
                                     (format nil "The method ~S is not part of class ~S"
                                             name
                                             class-name)))))

    ;; Ensure each method is defined
    (loop :for name :being :the :hash-keys :of method-table
          :for method := (find name (parser:toplevel-define-instance-methods unparsed-instance)
                               :key (a:compose #'parser:node-variable-name
                                               #'parser:instance-method-definition-name))
          :unless method
            :do (tc-error "Missing method"
                          (tc-note unparsed-instance "The method ~S is not defined" name)))

    (let* ((methods (loop :with table := (make-hash-table :test #'eq)
                          :with instance-scoped-tvars := (tc:type-variables (cons pred context))

                          :for method :in (parser:toplevel-define-instance-methods unparsed-instance)
                          :for name := (parser:node-variable-name (parser:instance-method-definition-name method))

                          :for class-method := (gethash name method-table)
                          :for class-method-scheme := (tc:ty-class-method-type class-method)
                          :for class-method-outer-tvars := (tc:ty-class-method-outer-tvars class-method)
                          :for class-method-explicit-tvars := (tc:ty-class-method-explicit-tvars class-method)
                          :for class-method-explicit-p := (tc:ty-scheme-explicit-p class-method-scheme)
                          :for class-method-tvars := (and class-method-explicit-p
                                                          class-method-explicit-tvars)
                          :for class-method-qual-ty
                            := (if class-method-explicit-p
                                   (tc:instantiate class-method-tvars
                                                   (tc:ty-scheme-type class-method-scheme))
                                   (tc:fresh-inst class-method-scheme))
                          :for scoped-method-tvars
                            := (if class-method-explicit-p
                                   (mapcar (lambda (tvar)
                                             (tc:apply-substitution instance-subs tvar))
                                           class-method-tvars)
                                   nil)
                          :for scoped-outer-method-tvars
                            := (mapcar (lambda (tvar)
                                         (tc:apply-substitution instance-subs tvar))
                                       class-method-outer-tvars)
                          :for class-method-constraints := (tc:qualified-ty-predicates class-method-qual-ty)
                          :for class-method-ty := (tc:qualified-ty-type class-method-qual-ty)

                          ;; NOTE: Instance methods type still do not contain the class predicate
                          :for instance-method-context := (append context class-method-constraints)
                          :for instance-method-qual-ty
                            := (let* ((qual-ty (tc:apply-substitution
                                                instance-subs
                                                (tc:qualify instance-method-context class-method-ty)))
                                      (scoped-subs
                                        (instance-scoped-method-substitutions
                                         scoped-outer-method-tvars
                                         instance-scoped-tvars)))
                                 (tc:apply-substitution scoped-subs qual-ty))
                          :for instance-method-scheme
                            := (if class-method-explicit-p
                                   (tc:quantify-using-tvar-order
                                    scoped-method-tvars
                                    instance-method-qual-ty
                                    t)
                                   (tc:quantify
                                    (set-difference
                                     (tc:type-variables instance-method-qual-ty)
                                     instance-scoped-tvars
                                     :test #'tc:ty=)
                                    instance-method-qual-ty))
                          :for instance-method-env
                            := (tc-env-extend-type-variable-scope
                                (make-tc-env :env env)
                                (remove-duplicates
                                 (append instance-scoped-tvars scoped-method-tvars)
                                 :test #'tc:ty=))

                          :do (multiple-value-bind (preds method subs)
                                  (infer-expl-binding-type method
                                                          instance-method-scheme
                                                          (source:location method)
                                                          nil
                                                          instance-method-env)
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
                                      :do (let ((match-subs
                                                  (handler-case
                                                      (tc:predicate-match node-pred context-pred instance-subs)
                                                    (tc:coalton-internal-type-error (e)
                                                      (error e)))))
                                            (setf subs (tc:compose-substitution-lists
                                                        match-subs
                                                        subs))))
                                (check-instance-predicate-not-narrowed method
                                                                       pred
                                                                       context
                                                                       subs
                                                                       env)
                                (setf (gethash name table) (tc:apply-substitution subs method)))

	                          :finally (return table))))

      (check-bindings-for-invalid-recursion
       (parser:toplevel-define-instance-methods unparsed-instance)
       env
       :binding-function-p
       (lambda (binding)
         (let ((typed-method
                 (gethash (parser:node-variable-name (parser:binding-name binding))
                          methods)))
           (and typed-method
                (or (instance-method-definition-function-syntax-p typed-method)
                    (and (instance-method-definition-params typed-method) t)
                    (and (null (node-body-nodes (instance-method-definition-body typed-method)))
                         (node-abstraction-p
                          (node-body-last-node (instance-method-definition-body typed-method))))
                    (consp (tc:qualified-ty-predicates
                            (node-type (instance-method-definition-name typed-method)))))
                t))))

      (make-toplevel-define-instance
       :context context
       :pred pred
       :methods methods
       :location (source:location unparsed-instance)
       :head-location (parser:toplevel-define-instance-head-location unparsed-instance)))))

(defun check-instance-valid (instance)
  (declare (type parser:toplevel-define-instance instance))

  ;; Instance validation is disabled for compiler generated instances
  (when (parser:toplevel-define-instance-compiler-generated instance)
    (return-from check-instance-valid))

  (let* ((types-package (util:find-package "COALTON/TYPES"))
         (runtime-repr (util:find-symbol "RUNTIMEREPR" types-package)))

    ;; Instance validation is disabled in the types package
    (when (eq *package* types-package)
      (return-from check-instance-valid))

    ;; Allow definition of LispArray and Complex instances of RuntimeRepr
    (when (member *package* (list (find-package "COALTON/LISPARRAY")
                                  (find-package "COALTON/MATH/COMPLEX")))
      (let ((types (parser:ty-predicate-types (parser:toplevel-define-instance-pred instance))))
        (when (and (= 1 (length types))
                   (parser:tapp-p (first types))
                   (member (parser:tycon-name (parser:tapp-from (first types)))
                           (list (find-symbol "COMPLEX" *package*)
                                 (find-symbol "LISPARRAY" *package*)))))
        (return-from check-instance-valid)))

    (when (eq (parser:identifier-src-name (parser:ty-predicate-class (parser:toplevel-define-instance-pred instance))) runtime-repr)
      (tc-error "Invalid instance"
                (tc-location (parser:toplevel-define-instance-head-location instance)
                             "RuntimeRepr instances cannot be written manually")))))

(defun check-for-orphan-instance (instance)
  (declare (type parser:toplevel-define-instance instance)
           (values null))

  ;; Stage-1 packages skip the orphan instance check
  (when (find *package* (list (find-package "COALTON/TYPES")
                              (find-package "COALTON/FIXED-SIZE-NUMBERS")
                              (find-package "COALTON/CLASSES")
                              (find-package "COALTON/HASH")
                              (find-package "COALTON/BUILTIN")
                              (find-package "COALTON/FUNCTIONS")
                              (find-package "COALTON/BOOLEAN")
                              (find-package "COALTON/BITS")
                              (find-package "COALTON/MATH/ARITH")
                              (find-package "COALTON/MATH/NUM")
                              (find-package "COALTON/MATH/BOUNDED")
                              (find-package "COALTON/MATH/CONVERSIONS")
                              (find-package "COALTON/MATH/FRACTION")
                              (find-package "COALTON/MATH/INTEGRAL")
                              (find-package "COALTON/MATH/REAL")
                              (find-package "COALTON/MATH/COMPLEX")
                              (find-package "COALTON/MATH/ELEMENTARY")
                              (find-package "COALTON/MATH/DYADIC")
                              (find-package "COALTON/CHAR")
                              (find-package "COALTON/STRING")
                              (find-package "COALTON/TUPLE")
                              (find-package "COALTON/OPTIONAL")
                              (find-package "COALTON/LIST")
                              (find-package "COALTON/RESULT")))
    (return-from check-for-orphan-instance))

  (let ((instance-syms
          (cons
           (parser:identifier-src-name (parser:ty-predicate-class (parser:toplevel-define-instance-pred instance)))
           (loop :for type :in (parser:ty-predicate-types (parser:toplevel-define-instance-pred instance))
                 :append (mapcar #'parser:tycon-name (parser:collect-referenced-types type))))))

    (unless (find *package* instance-syms :key #'symbol-package)
      (tc-cerror "Invalid orphan instance"
                 (tc-location (parser:toplevel-define-instance-head-location instance)
                              "instances must be defined in the same package as their class or reference one or more types in their defining package"))
      (return-from check-for-orphan-instance))))
