(defpackage #:coalton-impl/typechecker/translation-unit
  (:use
   #:cl
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/environment
   #:coalton-impl/typechecker/ast
   #:coalton-impl/typechecker/define
   #:coalton-impl/typechecker/define-type)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:translation-unit                   ; STRUCT
   #:make-translation-unit              ; CONSTRUCTOR
   #:translation-unit-types             ; ACCESSOR
   #:translation-unit-definitions       ; ACCESSOR
   #:translation-unit-instances         ; ACCESSOR
   #:translation-unit-classes           ; ACCESSOR
   #:translation-unit-attr-table        ; ACCESSOR
   #:translation-unit-package           ; ACCESSOR
   #:translation-unit-specializations   ; ACCESSOR
   #:generate-diff                      ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/translation-unit)

;; TODO: This is missing instances.

(defstruct translation-unit
  (types           nil                      :type type-definition-list      :read-only t)
  (definitions     nil                      :type toplevel-define-list      :read-only t)
  ;(instances       nil                      :type instance-definition-list  :read-only t)
  (classes         nil                      :type ty-class-list             :read-only t)
  (attr-table      (make-hash-table)        :type hash-table                :read-only t)
  (package         (util:required 'package) :type package                   :read-only t)
  (specializations nil                      :type specialization-entry-list :read-only t))

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
  (declare (type toplevel-define-list defs))
  (loop :for (name . def) :in defs
        :for function-entry := (lookup-function env name :no-error t)
        :collect `(set-value-type env ',name ,(lookup-value-type env name))
        :collect `(set-name env ',name ,(lookup-name env name))
        :collect `(set-code env ',name ,(lookup-code env name))
        :collect (if function-entry
                     `(set-function env ',name ,function-entry)
                     `(unset-function env ',name))
        :collect (if (node-abstraction-p def)
                     (error "not impl")
                     #+broken
                     `(set-function-source-parameter-names env
                                                           ',name
                                                           ',(node-abstraction-source-parameter-names def))
                     `(unset-function-source-parameter-names env ',name))))

(defun generate-instance-diff (instances env)
  (declare (type instance-definition-list instances))
  (loop :for inst :in instances
        :for class-name := (instance-definition-class-name inst)
        :for class := (lookup-class env class-name)
        :for pred := (instance-definition-predicate inst)
        :for codegen-sym := (instance-definition-codegen-sym inst)
        :for method-codegen-syms := (instance-definition-method-codegen-syms inst)
        :collect `(add-instance env ',class-name ,(lookup-class-instance env pred))
        :if (instance-definition-context inst)
          :collect `(set-function env ',codegen-sym ,(lookup-function env codegen-sym))
        :if (ty-class-fundeps class)
          :collect `(update-instance-fundeps env ,pred)
        :append (loop :for key :being :the :hash-keys :of method-codegen-syms
                      :for value :being :the :hash-values :of method-codegen-syms
                      :for function-entry := (lookup-function env value :no-error t)
                      :collect `(set-method-inline env ',key ',codegen-sym ',value)
                      :collect `(set-code env ',value ,(lookup-code env value))
                      :collect (if function-entry
                                   `(set-function env ',value ,function-entry)
                                   `(unset-function env ',value)))))

(defun generate-class-diff (classes env)
  (declare (type ty-class-list classes))
  (loop :for class :in classes
        :for name := (ty-class-name class)
        :collect `(set-class env ',name ,(lookup-class env name))
        :if (lookup-function env (ty-class-codegen-sym class) :no-error t)
          :collect `(set-function env ',name ,(lookup-function env (ty-class-codegen-sym class)))
        :if (ty-class-fundeps class)
          :collect `(initialize-fundep-environment env ',name)
        :append (loop :for (name . node) :in (ty-class-unqualified-methods class)
                      :for function-entry := (lookup-function env name :no-error t)
                      :collect `(set-value-type env ',name ,(lookup-value-type env name))
                      :collect `(set-name env ',name ,(lookup-name env name))
                      :collect (if function-entry
                                   `(set-function env ',name ,function-entry)
                                   `(unset-function env ',name)))))

(defun generate-specialization-diff (specializations env)
  (declare (type specialization-entry-list specializations))
  (loop :for elem :in specializations
        :collect `(add-specialization env ',(lookup-specialization env (specialization-entry-from elem) (specialization-entry-to elem)))))

(defun generate-diff (translation-unit env env_name)
  `(eval-when (:load-toplevel :execute ;; TODO: ugh
                              )
     (let ((env ,env_name))
       ,@(loop :for elem :in (generate-type-diff
                              (translation-unit-types translation-unit)
                              env)
               :collect `(setf env ,elem))
       ,@(loop :for elem :in (generate-class-diff
                              (translation-unit-classes translation-unit)
                              env)
               :collect `(setf env ,elem))
       ;; TODO: Add instances
       #+broken
       ,@(loop :for elem :in (generate-instance-diff
                              (translation-unit-instances translation-unit)
                              env)
               :collect `(setf env ,elem))
       ,@(loop :for elem :in (generate-definition-diff
                              (translation-unit-definitions translation-unit)
                              env)
               :collect `(setf env ,elem))
       (setf ,env_name env))))
