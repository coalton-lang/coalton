(in-package #:coalton-impl/typechecker)

;;;
;;; Parsing instance defintions
;;;

(defstruct instance-definition
  (class-name  (required 'class-name)  :type symbol             :read-only t)
  (predicate   (required 'predicate)   :type ty-predicate       :read-only t)
  (context     (required 'context)     :type ty-predicate-list  :read-only t)
  (methods     (required 'methods)     :type typed-binding-list :read-only t)
  (codegen-sym (required 'codegen-sym) :type symbol             :read-only t))

(defun instance-definition-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'instance-definition-p x)))

(deftype instance-definition-list ()
  '(satisfies instance-definition-list-p))

(defun parse-instance-definition (form env)
  (declare (type list form)
           (type environment env)
           (values instance-definition))
  (unless (and (listp form)
               (<= 2 (length form))
               (eql 'coalton:define-instance (first form)))
    (error "Malformed DEFINE-INSTANCE form ~A" form))

  (with-parsing-context ("instance definition ~A" form)
    ;; Parse out the instance predicate and context
    (multiple-value-bind (instance-context instance-predicate)
        (parse-class-signature env (second form) nil nil)

      ;; Ensure that all type variables in the context appear in the instance predicate
      (unless (subsetp (type-variables instance-context)
                       (type-variables instance-predicate))
        (error "Context of ~A is not simpler than predicate in instance definition ~A"
               instance-context
               instance-predicate))

      ;; Look up the instance in the current environment
      (let* ((class-name (ty-predicate-class instance-predicate))
             (class-entry (or (lookup-class env class-name)
                              (coalton-impl::coalton-bug "Class unknown in environment during method parsing for ~S" class-name)))

             (instance-method-defintions (cddr form))
             (instance-subs (predicate-match (ty-class-predicate class-entry)
                                             instance-predicate)))

        ;; Check the instance environment to make sure the context is resolvable
        (labels ((find-instance-entry (instance-predicate instance-context)
                   (let* ((pred-class (ty-predicate-class instance-predicate))
                          (instances (lookup-class-instances env pred-class :no-error t)))
                     (fset:do-seq (instance instances)
                       (handler-case
                           (let ((subs (predicate-match (ty-class-instance-predicate instance) instance-predicate)))
                             ;; Ensure that the context matches using the substitution
                             (unless (null (set-exclusive-or
                                            instance-context
                                            (apply-substitution subs (ty-class-instance-constraints instance))
                                            :test #'equalp))
                               (coalton-impl::coalton-bug "Incompatible context with environment for instance during method parsing for ~S ~A ~A ~A" pred-class (apply-substitution subs instance-context) (ty-class-instance-constraints instance) subs))
                             (return-from find-instance-entry instance))
                         (predicate-unification-error () nil)))
                     ;; If we didn't find anything then signal an error
                     (coalton-impl::coalton-bug "Instance unknown in environment during method parsing for ~S" pred-class))))

          ;; Lookup and verify the instance in the env
          (let ((instance-entry (find-instance-entry instance-predicate instance-context)))

            ;; Ensure that all predicates in the context have been declared
            (dolist (pred (ty-class-superclasses class-entry))
              (unless (lookup-class-instance env (apply-substitution instance-subs pred) :no-error t)
                (error "Missing instance definition for ~A in definition of ~A"
                       (apply-substitution instance-subs pred)
                       instance-predicate)))

            ;; Parse and typecheck all method definitons
            (let ((method-bindings nil))
              (loop :for method-definiton :in instance-method-defintions
                    :do
                       (push (multiple-value-bind (method-name parsed-method-form)
                                 (coalton-impl::parse-define-form method-definiton)

                               ;; Disallow duplicate method definitions
                               (when (member method-name method-bindings :key #'car)
                                 (error "Duplicate method definition for method ~S" method-name))

                               (let* (;;
                                      (class-method-scheme
                                        (cdr (or (find-if (lambda (method)
                                                            (eql method-name (car method)))
                                                          (ty-class-unqualified-methods class-entry))
                                                 (error "Unknown method ~A in instance definition of class ~A for ~A"
                                                        method-name
                                                        class-name
                                                        instance-predicate))))

                                      (fresh-class-method-scheme (fresh-inst class-method-scheme))
                                      (class-method-constraints (qualified-ty-predicates fresh-class-method-scheme))
                                      (class-method-type (qualified-ty-type fresh-class-method-scheme))

                                      (instance-method-context (append instance-context class-method-constraints))
                                      (instance-method-qual-type (qualify instance-method-context
                                                                          class-method-type))
                                      (instance-method-type
                                        (quantify (type-variables
                                                   (apply-substitution instance-subs instance-method-qual-type))
                                                  (apply-substitution instance-subs instance-method-qual-type))))
                                 (multiple-value-bind (scheme binding preds env subs qual-type)
                                     (derive-expl-type (cons method-name parsed-method-form)
                                                       instance-method-type
                                                       env
                                                       instance-subs nil
                                                       :allow-deferred-predicates nil)
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
                                   ;; Return the typed method
                                   (cons (car binding)
                                         (apply-substitution subs (cdr binding))))))
                             method-bindings))
              (let ((missing-methods (set-difference (ty-class-unqualified-methods class-entry)
                                                     method-bindings
                                                     :key #'car)))
                (unless (null missing-methods)
                  (coalton-impl/ast::error-parsing form "Incomplete instance definition for instance ~A. Missing methods ~S" instance-predicate (mapcar #'car missing-methods))))

              (make-instance-definition
               :class-name class-name
               :predicate instance-predicate
               :context instance-context
               :methods method-bindings
               :codegen-sym (ty-class-instance-codegen-sym instance-entry)))))))))
