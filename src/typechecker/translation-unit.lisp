(in-package #:coalton-impl/typechecker)

(defstruct translation-unit
  (types       (required 'types)       :type type-definition-list     :read-only t)
  (definitions (required 'definitions) :type typed-binding-list       :read-only t)
  (instances   (required 'instances)   :type instance-definition-list :read-only t)
  (classes     (required 'classes)     :type ty-class-list            :read-only t))

;; FUNCTION ENV

(defun generate-type-diff (types env)
  (declare (type type-definition-list types))
  (loop :for def :in types
        :for name := (type-definition-name def)
        :collect `(set-type env ',name ,(lookup-type env name))
        :append (loop :for ctor :in (type-definition-constructors def)
                      :for name := (constructor-entry-name ctor)
                      :for function-entry := (lookup-function env name :no-error t)
                      :collect `(set-value-type env ',name ,(lookup-value-type env name))
                      :collect `(set-constructor env ',name ,(lookup-constructor env name))
                      :collect `(set-name env ',name ,(lookup-name env name))
                      :collect (if function-entry
                                   `(set-function env ',name ,function-entry)
                                   `(unset-function env ',name)))))

(defun generate-definition-diff (defs env)
  (declare (type typed-binding-list defs))
  (loop :for (name . def) :in defs
        :for function-entry := (lookup-function env name :no-error t)
        :collect `(set-value-type env ',name ,(lookup-value-type env name))
        :collect `(set-name env ',name ,(lookup-name env name))
        :collect (if function-entry
                     `(set-function env ',name ,function-entry)
                     `(unset-function env ',name))))

(defun generate-instance-diff (instances env)
  (declare (type instance-definition-list instances))
  (loop :for inst :in instances
        :for class-name := (instance-definition-class-name inst)
        :for pred := (instance-definition-predicate inst)
        :for codegen-sym := (instance-definition-codegen-sym inst)
        :for method-codegen-syms := (instance-definition-method-codegen-syms inst)
        :collect `(add-instance env ',class-name ,(lookup-class-instance env pred))
        :append (loop :for key :being :the :hash-keys :of method-codegen-syms
                      :for value :being :the :hash-values :of method-codegen-syms
                      :for function-entry := (lookup-function env value :no-error t)
                      :collect `(set-method-inline env ',key ',codegen-sym ',value)
                      :collect (if function-entry
                                 `(set-function env ',value ,function-entry)
                                 `(unset-function env ',value)))))

(defun generate-class-diff (classes env)
  (declare (type ty-class-list classes))
  (loop :for class :in classes
        :for name := (ty-class-name class)
        :collect `(set-class env ',name ,(lookup-class env name))
        :append (loop :for (name . node) :in (ty-class-unqualified-methods class)
                      :for function-entry := (lookup-function env name :no-error t)
                      :collect `(set-value-type env ',name ,(lookup-value-type env name))
                      :collect `(set-name env ',name ,(lookup-name env name))
                      :collect (if function-entry
                                   `(set-function env ',name ,function-entry)
                                   `(unset-function env ',name)))))

(defun generate-diff (translation-unit env env_name)
  `(eval-when (:load-toplevel)
     (let ((env ,env_name))
       ,@(loop :for elem :in (generate-type-diff
                              (translation-unit-types translation-unit)
                              env)
               :collect `(setf env ,elem))
       ,@(loop :for elem :in (generate-class-diff
                              (translation-unit-classes translation-unit)
                              env)
               :collect `(setf env ,elem))
       ,@(loop :for elem :in (generate-instance-diff
                              (translation-unit-instances translation-unit)
                              env)
               :collect `(setf env ,elem))
       ,@(loop :for elem :in (generate-definition-diff
                              (translation-unit-definitions translation-unit)
                              env)
               :collect `(setf env ,elem))
       (setf ,env_name env))))

