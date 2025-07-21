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
   (#:a #:alexandria)
   (#:settings #:coalton-impl/settings)
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:toplevel-define-instance           ; FUNCTION
   #:toplevel-typecheck-instance        ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/define-instance)

(defun expand-constraint (base-constraint env)
  "This was implemented as a hack to make `derive' work on recursive
types.  It gets rid of recursive constraints in class instances, allowing you
to write an instance with signature `(Eq A => Eq A)'."
  (declare (type tc:ty-predicate base-constraint)
           (type tc:environment env)
           (values tc:ty-predicate-list &optional))

  (labels ((f (constraint env)
             (multiple-value-bind (inst subs) (tc:lookup-class-instance env constraint :no-error t)
               (if inst
                   (mapcan (a:rcurry #'f env)
                           (mapcar (a:curry #'tc:apply-substitution subs)
                                   (remove-if (a:curry #'tc:type-predicate= base-constraint)
                                              (tc:ty-class-instance-constraints inst))))
                   (list constraint)))))
    (f base-constraint env)))

(defun expand-context (context env)
  (declare (type tc:ty-predicate-list context)
           (type tc:environment env)
           (values tc:ty-predicate-list &optional))

  (remove-duplicates
   (a:mappend (a:rcurry #'expand-constraint env) context)
   :test #'tc:type-predicate=))

(defun toplevel-define-instance (instances env)
  (declare (type parser:toplevel-define-instance-list instances)
           (type tc:environment env)
           (values tc:ty-class-instance-list tc:environment))

  (values
   (loop :for instance :in 
           (loop :for instance :in instances
                 :collect (multiple-value-bind (instance env_)
                              (define-instance-in-environment instance env)
                            (setf env env_)
                            instance))
         :do
            ;; Expand the constraints, perhaps it should be done in a
            ;; different stage but this works.
            (setf (tc:ty-class-instance-constraints instance)
                  (expand-context (tc:ty-class-instance-constraints instance) env))
         :collect instance)

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
          :do (partial-type-env-add-var partial-env (parser:tyvar-name var)))

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
               (loop :with alist := ()
                     :for method-name :in method-names
                     :for method-codegen-sym :in method-codegen-syms
                     :for method-def := (find method-name (parser:toplevel-define-instance-methods instance)
                                              :key (lambda (method-def)
                                                     (parser:node-variable-name
                                                      (parser:instance-method-definition-name method-def))))
                     :for method-inline := (parser:instance-method-definition-inline method-def)
                     ;; Convert from attribute inline to boolean
                     :do (push (cons method-codegen-sym (if method-inline t nil)) alist)
                     :finally (return alist)))

             (instance-entry
               (tc:make-ty-class-instance
                :constraints context
                :predicate pred
                :codegen-sym instance-codegen-sym
                :method-codegen-syms method-codegen-syms
                :method-codegen-inline-p method-codegen-inline-p
                :docstring (source:docstring instance))))

        (cond (context
               (setf env (tc:set-function env instance-codegen-sym (tc:make-function-env-entry
                                                                    :name instance-codegen-sym
                                                                    :arity (length context)
                                                                    :inline-p nil))))
              ((tc:lookup-function env instance-codegen-sym :no-error t)
               (setf env (tc:unset-function env instance-codegen-sym))))

        (when (tc:ty-class-fundeps class)
          (handler-case
              (setf env (tc:update-instance-fundeps env pred))
            (tc:fundep-conflict (e)
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
                                   "instance overlaps with ~S" (tc:overlapping-instance-error-inst2 e)))))

        (loop :for method-name :in method-names
              :for method-codegen-sym :in method-codegen-syms :do
                (setf env (tc:set-method-inline env method-name instance-codegen-sym method-codegen-sym)))

        (values instance-entry env)))))

(defun typecheck-instance (instance unparsed-instance env)
  (declare (type tc:ty-class-instance instance)
           (type parser:toplevel-define-instance unparsed-instance)
           (type tc:environment env))

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
                 :for method :in (tc:ty-class-unqualified-methods class)
                 :do (setf (gethash (tc:ty-class-method-name method) table)
                           (tc:ty-class-method-type method))
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
                                                      "No instance for ~S" superclass)))

                      :for additional-context
                        := (tc:apply-substitution
                            (tc:predicate-match
                             (tc:apply-substitution instance-subs (tc:ty-class-instance-predicate superclass-instance))
                             superclass)
                            (tc:ty-class-instance-constraints superclass-instance))

                      :do (loop :for pred :in additional-context
                                :do (unless (tc:entail env context pred)
                                      (tc-error "Instance missing context"
                                                (tc-location (parser:toplevel-define-instance-head-location unparsed-instance)
                                                             "No instance for ~S arising from constraints of superclasses ~S"
                                                             pred
                                                             superclass))))))

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
                                                           (source:location method)
                                                           nil
                                                           (make-tc-env :env env))

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
                                                      (tc:predicate-match node-pred context-pred instance-subs)
                                                      subs)))

                                (setf (gethash name table) (tc:apply-substitution subs method)))

                          :finally (return table))))

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

  (let* ((types-package (util:find-package "COALTON-LIBRARY/TYPES"))

         (runtime-repr (util:find-symbol "RUNTIMEREPR" types-package)))

    ;; Instance validation is disabled in the types package
    (when (eq *package* types-package)
      (return-from check-instance-valid))

    ;; Allow definition of LispArray and Complex instances of RuntimeRepr
    (when (member *package* (list (find-package "COALTON-LIBRARY/LISPARRAY")
                                  (find-package "COALTON-LIBRARY/MATH/COMPLEX")))
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
      (tc-cerror "Invalid orphan instance"
                 (tc-location (parser:toplevel-define-instance-head-location instance)
                              "instances must be defined in the same package as their class or reference one or more types in their defining package"))
      (return-from check-for-orphan-instance))))
