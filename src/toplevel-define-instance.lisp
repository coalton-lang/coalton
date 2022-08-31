(in-package #:coalton-impl)

;;; Handling of toplevel COALTON:DEFINE-INSTANCE.

(defun process-toplevel-instance-definitions (definstance-forms package env &key compiler-generated)
  (declare  (values tc:instance-definition-list))

  (mapcar
     (lambda (form)
       (tc:parse-instance-definition form package env :compiler-generated compiler-generated))
     definstance-forms))

(defun predeclare-toplevel-instance-definitions (definstance-forms package env)
  "Predeclare all instance definitions in the environment so values can be typechecked"
  (declare (type list definstance-forms)
           (type package package)
           (type tc:environment env)
           (values tc:environment))

  (loop :for form :in definstance-forms
        :do (multiple-value-bind (predicate context methods)
                (tc:parse-instance-decleration form env)
              (declare (ignore methods))
              (let* ((class-name (tc:ty-predicate-class predicate))

                     (class (tc:lookup-class env class-name))

                     (instance-codegen-sym
                       (alexandria:format-symbol
                        package "INSTANCE/~A"
                        (with-output-to-string (s)
                          (tc:with-pprint-variable-context ()
                            (let* ((*print-escape* t))
                              (tc:pprint-predicate s predicate))))))

                     (method-names (mapcar #'car (tc:ty-class-unqualified-methods class)))

                     (method-codegen-syms
                       (let ((table (make-hash-table)))
                         (loop :for method-name :in method-names
                               :do (setf (gethash method-name table)
                                         (alexandria:format-symbol
                                          package
                                          "~A-~S"
                                          instance-codegen-sym
                                          method-name)))
                         table))

                     (instance
                       (tc:make-ty-class-instance
                        :constraints context
                        :predicate predicate
                        :codegen-sym instance-codegen-sym
                        :method-codegen-syms method-codegen-syms)))

                (loop :for key :being :the :hash-keys :of method-codegen-syms
                      :for value :being :the :hash-values :of method-codegen-syms
                      :for codegen-sym := (tc:ty-class-instance-codegen-sym instance)
                      :do (setf env (tc:set-method-inline env key codegen-sym value)))

                (when context
                  (setf env (tc:set-function
                             env
                             instance-codegen-sym
                             (tc:make-function-env-entry
                              :name instance-codegen-sym
                              :arity (length context)))))

                (when (tc:ty-class-fundeps class)
                  ;; Validate that all fundeps have contain on the left side a superset of the type variables on the right side.
                  ;; This means that fundep substitutions will be strictly improving (will not introduce new type variables).
                  (loop :with map := (tc:ty-class-class-variable-map class)

                        ;; Compute the types of the from side of each fundep
                        :for fundep :in (tc:ty-class-fundeps class)
                        :for fundep-tys := (util:project-map (tc:fundep-from fundep) map (tc:ty-predicate-types predicate))
                        :for fundep-tyvars := (tc:type-variables fundep-tys)


                        ;; Compute the transitive closure of the from side of each fundep
                        :for closure := (tc:closure (tc:fundep-from fundep) (tc:ty-class-fundeps class))

                        ;; Compute the type variables of the transitive closure
                        :for closure-tys := (util:project-map closure map (tc:ty-predicate-types predicate))
                        :for closure-tyvars := (tc:type-variables closure-tys)

                        ;; Check that the type variables of each fundep is a subset of the type variables of it's closure
                        :unless (subsetp closure-tyvars fundep-tyvars :test #'equalp)
                          :do (let ((diff-vars (set-difference closure-tyvars fundep-tyvars :test #'equalp)))
                                (error 'tc:fundep-variable-error
                                       :pred predicate
                                       :vars diff-vars
                                       :type fundep-tys
                                       :class class-name
                                       :class-vars (tc:ty-class-class-variables class)
                                       :class-fundeps (tc:ty-class-fundeps class)
                                       :fundep fundep
                                       :closure closure)))
                  
                  (setf env (tc:update-instance-fundeps env predicate)))

                (setf env (tc:add-instance env class-name instance)))))

  env)
