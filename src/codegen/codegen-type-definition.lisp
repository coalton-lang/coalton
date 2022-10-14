(defpackage #:coalton-impl/codegen/codegen-type-definition
  (:use
   #:cl)
  (:import-from
   #:coalton-impl/codegen/struct-or-class
   #:struct-or-class
   #:make-struct-or-class-field
   #:struct-or-class-field-name)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:global-lexical #:coalton-impl/global-lexical)
   (#:tc #:coalton-impl/typechecker)
   (#:rt #:coalton-impl/runtime))
  (:export
   #:codegen-type-definition
   #:constructor-slot-name))

(in-package #:coalton-impl/codegen/codegen-type-definition)

(defun constructor-slot-name (constructor-entry i)
  (alexandria:format-symbol (symbol-package (tc:constructor-entry-classname constructor-entry))
                            "_~D" i))

(defun codegen-type-definition (def env)
  (append
   (cond
     ((tc:type-definition-enum-repr def)
      (loop :for constructor :in (tc:type-definition-constructors def)
            :append
            `((global-lexical:define-global-lexical ,(tc:constructor-entry-name constructor)
                  (member ,(tc:constructor-entry-compressed-repr constructor)))
              (setf ,(tc:constructor-entry-name constructor) ',(tc:constructor-entry-compressed-repr constructor))
              (deftype ,(tc:constructor-entry-classname constructor) ()
                (quote (member ,(tc:constructor-entry-compressed-repr constructor)))))))

     ((tc:type-definition-newtype def)
      (let ((constructor (first (tc:type-definition-constructors def))))
        `((declaim (inline ,(tc:constructor-entry-name constructor)))
          (defun ,(tc:constructor-entry-name constructor) (x) x)
          (global-lexical:define-global-lexical ,(tc:constructor-entry-name constructor) rt:function-entry)
          (setf ,(tc:constructor-entry-name constructor)
                ,(rt:construct-function-entry `#',(tc:constructor-entry-name constructor) 1)))))

     (t
      `(,(if (settings:coalton-release-p)
             `(defstruct (,(tc:type-definition-name def)
                          (:constructor nil)
                          (:predicate nil))
                ,@(when (tc:type-definition-docstring def)
                    (list (tc:type-definition-docstring def))))

             `(defclass ,(tc:type-definition-name def) ()
                ()
                ,@(when (tc:type-definition-docstring def)
                    `((:documentation ,(tc:type-definition-docstring def))))))

        (defmethod make-load-form ((,(intern "OBJ") ,(tc:type-definition-name def)) &optional ,(intern "ENV"))
          (make-load-form-saving-slots ,(intern "OBJ") :environment ,(intern "ENV")))

        ,@(loop
            :for constructor :in (tc:type-definition-constructors def)
            :for classname := (tc:constructor-entry-classname constructor)
            :for superclass := (tc:type-definition-name def)
            :for constructor-name :=  (tc:constructor-entry-name constructor)
            :for fields
              := (loop :for field :in (tc:constructor-arguments constructor-name env)
                       :for runtime-type := (tc:lisp-type field env)
                       :for i :from 0
                       :for name := (constructor-slot-name constructor i)
                       :collect (make-struct-or-class-field
                                 :name name
                                 :type runtime-type))

            :for field-names := (mapcar #'struct-or-class-field-name fields)

            ;; Declare the constructor as inline in release mode
            :append
            (when (settings:coalton-release-p)
              (list `(declaim (inline ,constructor-name))))

            :append (struct-or-class
                     :classname classname
                     :constructor constructor-name
                     :superclass superclass
                     :fields fields
                     :mode (if (settings:coalton-release-p)
                               :struct
                               :class))

            :collect (cond
                       ((zerop (tc:constructor-entry-arity constructor))
                        `(defmethod print-object ((self ,classname) stream)
                           (declare (type stream stream)
                                    (type ,classname self)
                                    (values ,classname))
                           (format stream "#.~s" ',(tc:constructor-entry-name constructor))
                           self))
                       (t
                        `(defmethod print-object ((self ,classname) stream)
                           (declare (type stream stream)
                                    (type ,classname self)
                                    (values ,classname))
                           (format stream "#.(~s~{ ~s~})"
                                   ',(tc:constructor-entry-name constructor)
                                   (list ,@(mapcar (lambda (slot) `(slot-value self ',slot)) field-names)))
                           self))))

        ,@(when (settings:coalton-release-p)
            (list
             #+sbcl
             `(declaim (sb-ext:freeze-type ,(tc:type-definition-name def))))))))

   (loop :for constructor :in (tc:type-definition-constructors def)
         :for name := (tc:constructor-entry-name constructor)
         :for ty := (tc:lookup-value-type env name) 
         :collect `(setf (documentation ',name 'variable)
                         ,(format nil "~A :: ~A" name ty))
         :when (> (tc:constructor-entry-arity constructor) 0)
           :collect `(setf (documentation ',name 'function)
                           ,(format nil "~A :: ~A" name ty)))))
