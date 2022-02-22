(in-package #:coalton-impl)

;;; Handling of toplevel COALTON:DEFINE-INSTANCE.

(defun process-toplevel-instance-definitions (definstance-forms package env)
  (declare (values instance-definition-list))
  (mapcar
   (lambda (form)
     (parse-instance-definition form package env))
   definstance-forms))

(defun predeclare-toplevel-instance-definitions (definstance-forms package env)
  "Predeclare all instance definitions in the environment so values can be typechecked"
  (declare (type list definstance-forms)
           (type package package)
           (type environment env)
           (values environment))
  (let ((parsed-instances
          (mapcar (lambda (form)
                    (unless (and (listp form)
                                 (<= 2 (length form))
                                 (eql 'coalton:define-instance (first form)))
                      (error "Malformed DEFINE-INSTANCE form ~A" form))
                    (multiple-value-list (coalton-impl/typechecker::parse-class-signature env (cadr form) nil nil)))
                  definstance-forms)))
    (dolist (parsed-instance parsed-instances)
      (let* ((class-name (coalton-impl/typechecker::ty-predicate-class
                          (second parsed-instance)))
             (instance-codegen-sym (alexandria:format-symbol
                                    package "INSTANCE/~A"
                                    (with-output-to-string (s)
                                      (with-pprint-variable-context ()
                                        (pprint-predicate s (second parsed-instance))))))
             (method-names (mapcar
                            #'car
                            (coalton-impl/typechecker::ty-class-unqualified-methods
                             (coalton-impl/typechecker::lookup-class env class-name))))

             (method-codegen-syms
               (let ((table (make-hash-table)))
                 (loop :for method-name :in method-names
                       :do (setf (gethash method-name table)
                                 (alexandria:format-symbol
                                  package
                                  "~A-~A"
                                  instance-codegen-sym
                                  method-name)))
                 table))
             
             (instance (coalton-impl/typechecker::ty-class-instance
                        (first parsed-instance)
                        (second parsed-instance)
                        instance-codegen-sym
                        method-codegen-syms)))


        (loop :for key :being :the :hash-keys :of method-codegen-syms
              :for value :being :the :hash-values :of method-codegen-syms
              :for codegen-sym := (coalton-impl/typechecker::ty-class-instance-codegen-sym instance)
              :do (setf env (coalton-impl/typechecker::set-method-inline env key codegen-sym value)))

        (setf env (coalton-impl/typechecker::add-instance env class-name instance))))
    env))
