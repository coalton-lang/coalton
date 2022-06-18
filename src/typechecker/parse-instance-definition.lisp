(in-package #:coalton-impl/typechecker)

;;;
;;; Parsing instance defintions
;;;

(defstruct instance-definition
  (class-name          (required 'class-name)          :type symbol             :read-only t)
  (predicate           (required 'predicate)           :type ty-predicate       :read-only t)
  (context             (required 'context)             :type ty-predicate-list  :read-only t)
  (methods             (required 'methods)             :type hash-table         :read-only t)
  (codegen-sym         (required 'codegen-sym)         :type symbol             :read-only t)
  (method-codegen-syms (required 'method-codegen-syms) :type hash-table         :read-only t))

(defun instance-definition-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'instance-definition-p x)))

(deftype instance-definition-list ()
  '(satisfies instance-definition-list-p))

(defun parse-instance-decleration (form env)
  (declare (type list form)
           (type environment env)
           (values ty-predicate ty-predicate-list list &optional))

    (unless (and (listp form)
                 (<= 2 (length form))
                 (eq (first form) 'coalton:define-instance))
      (error-parsing form "malformed DEFINE-INSTANCE form"))

    (multiple-value-bind (unparsed-predicate unparsed-context)
        (split-class-signature (second form) "malformed DEFINE-INSTANCE form")

      (let* ((methods (nthcdr 2 form))

             (tyvar-names (collect-type-vars unparsed-predicate))

             (tyvars
               (loop :for tyvar-name :in tyvar-names
                     :collect (list tyvar-name (make-variable (make-kvariable))))))

        ;; Check for type variables that appear in context but not in the predicate
        (with-parsing-context ("instance definition ~S" unparsed-predicate)
          (loop :for unparsed-ctx :in unparsed-context
                :for ctx-tyvar-names := (collect-type-vars unparsed-ctx)
                :do (loop :for ctx-tyvar :in ctx-tyvar-names
                          :do (unless (find ctx-tyvar tyvar-names :test #'equalp)
                                (error-parsing
                                 unparsed-ctx
                                 "type variable ~S appears in constraint but not in instance head"
                                 ctx-tyvar))))

          (let* ((ksubs nil)

                 (predicate
                   (multiple-value-bind (predicate new-ksubs)
                       (parse-type-predicate env unparsed-predicate tyvars ksubs)
                     (setf ksubs new-ksubs)
                     predicate))

                 (context
                   (loop :for unparsed-ctx :in unparsed-context
                         :collect (multiple-value-bind (predicate new-ksubs)
                                      (parse-type-predicate env unparsed-ctx tyvars ksubs)
                                    (setf ksubs new-ksubs)
                                    predicate)))

                 (predicate (apply-ksubstitution ksubs predicate))

                 (context (apply-ksubstitution ksubs context)))

            (values
             predicate
             context
             methods))))))

(defun check-for-orphan-instance (predicate package)
  ;; Instances defined on predeclared types violate the orphan rule
  (unless coalton-impl::*coalton-stage-1-complete*
    (return-from check-for-orphan-instance))

  (when (equalp (symbol-package (ty-predicate-class predicate)) package)
    (return-from check-for-orphan-instance))

  (let ((foreign-type nil))
    (loop :for ty :in (type-constructors (ty-predicate-types predicate))
          :when (equalp (symbol-package ty) package)
            :do (return-from check-for-orphan-instance)

          :do (setf foreign-type t))

    (when foreign-type
      (warn "Instance ~A defined in package ~A violates the orphan rule.~%    Instances can only be defined when the class~%    or at least one type is defined in the current package."
            predicate
            (package-name package)))))

(defun ty-underlying-type-name (ty)
  (declare (ty ty)
           (values symbol &optional))
  (etypecase ty
    (tcon (tycon-name (tcon-tycon ty)))
    (tapp (ty-underlying-type-name (tapp-from ty)))))

(defun ty-find-type-entry (env ty)
  (lookup-type env (ty-underlying-type-name ty)))

(defun check-for-addressable-on-non-repr-native-type (form predicate class-name env)
  "User code is not allowed to define instances of `Addressable' except for types which specify `repr :native'.

`process-coalton-toplevel' will bypass this check for compiler-generated `Addressable' instances returned from
`process-toplevel-type-definitions' will by passing `:compiler-generated t' through
`process-toplevel-instance-definitions' to `parse-instance-definition'."
  (when (eq class-name 'coalton-impl/early-library-defs:Addressable)
    (let* ((ty-args (ty-predicate-types predicate)))
      (unless (= (length ty-args) 1)
        (error-parsing
         form
         "Bad number of type arguments ~d for instance of Addressable"
         (length ty-args)))
      (let* ((type-entry (ty-find-type-entry env (first ty-args))))
        (unless (explicit-repr-explicit-addressable-p (type-entry-explicit-repr type-entry))
          (error-parsing
           form
           "Cannot explicitly define Addressable instance for type ~s with explicit repr ~s"
           (type-entry-name type-entry)
           (type-entry-explicit-repr type-entry)))))))

(defun parse-instance-definition (form package env &key compiler-generated)
  (multiple-value-bind (predicate context methods)
      (parse-instance-decleration form env)

    (check-for-orphan-instance predicate package)

    (with-pprint-variable-context ()
      (with-parsing-context ("definition of ~A" predicate)
        (let* (;; Lookup the predeclared instance-entry for this instance
               (instance-entry (lookup-class-instance env predicate))

               (class-name (ty-predicate-class predicate))

               (class-entry (lookup-class env class-name))

               (instance-subs (predicate-match (ty-class-predicate class-entry)
                                               predicate)))

          (unless compiler-generated
            (check-for-addressable-on-non-repr-native-type form predicate class-name env))

          ;; Check that constraints defined on the class are resolvable
          (loop :for superclass :in (ty-class-superclasses class-entry)
                :do (or (lookup-class-instance env (apply-substitution instance-subs superclass) :no-error t)
                        (error-unknown-instance
                         (apply-substitution instance-subs superclass))))

          (let ((method-bindings (make-hash-table)))

            ;; Parse and typecheck all method definitions
            (loop :for method :in methods
                  :do (multiple-value-bind (method-name parsed-method-form)
                          (coalton-impl::parse-define-form method package env :skip-inherited-symbol-checks t)

                        (when (gethash method-name method-bindings)
                          (error-parsing method "duplicate method definition for method ~S" method-name))

                        (let ((class-method (find method-name (ty-class-unqualified-methods class-entry) :key #'car :test #'equalp)))

                          (unless class-method 
                            (error-parsing method "unknown method ~A for class ~A" method-name class-name))

                          (let* ((class-method-scheme (cdr class-method))

                                 (class-method-qual-ty (fresh-inst class-method-scheme))

                                 (class-method-constraints (qualified-ty-predicates class-method-qual-ty))

                                 (class-method-ty (qualified-ty-type class-method-qual-ty))

                                 (instance-method-context (append context class-method-constraints))

                                 (instance-method-qual-type
                                   (apply-substitution instance-subs (qualify instance-method-context class-method-ty)))

                                 (instance-method-scheme
                                   (quantify
                                    (type-variables instance-method-qual-type)
                                    instance-method-qual-type)))

                            (multiple-value-bind (scheme binding preds env subs qual-type)
                                (derive-expl-type
                                 (cons method-name parsed-method-form)
                                 instance-method-scheme
                                 env
                                 nil
                                 nil
                                 :allow-deferred-predicates nil
                                 :allow-returns nil)
                              (declare (ignore scheme env))

                              ;; Predicates should never be here
                              (unless (null preds)
                                (coalton-impl::coalton-bug "Instance definition predicates should be nil"))

                              ;; Unify the resulting typed node
                              ;; type's predicates with our above
                              ;; predicates to ensure that the
                              ;; type variables match those in
                              ;; the context
                              (loop :for context-pred :in instance-method-context
                                    :for node-pred :in (qualified-ty-predicates qual-type)
                                    :do
                                       (setf subs
                                             (compose-substitution-lists (predicate-match node-pred context-pred) subs)))
 
                              (setf (gethash method-name method-bindings)
                                    (remove-static-preds (apply-substitution subs (cdr binding)))))))))

            ;; Check for missing method definitions
            (loop :for (name . type) :in (ty-class-unqualified-methods class-entry)
                  :do (unless (gethash name method-bindings)
                        (error-parsing form "instance definition is missing method ~S" name)))

            (make-instance-definition
             :class-name class-name
             :predicate predicate
             :context context
             :methods method-bindings
             :codegen-sym (ty-class-instance-codegen-sym instance-entry)
             :method-codegen-syms (ty-class-instance-method-codegen-syms instance-entry))))))))
