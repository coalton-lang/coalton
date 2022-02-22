(defpackage #:coalton-impl/codegen/codegen-type-definition
  (:use
   #:cl
   #:coalton-impl/util)
  (:import-from
   #:coalton-impl/codegen/lisp-type
   #:lisp-type)
  (:import-from
   #:coalton-impl/codegen/struct-or-class
   #:struct-or-class
   #:make-struct-or-class-field
   #:struct-or-class-field-name)
  (:import-from
   #:coalton-impl/codegen/function-entry
   #:construct-function-entry
   #:F1)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:codegen-type-definition))

(in-package #:coalton-impl/codegen/codegen-type-definition)

(defun codegen-type-definition (def env)
  (let ((package (symbol-package (tc:type-definition-name def))))

    (append
     (cond
       ((tc:type-definition-enum-repr def)
        (loop :for constructor :in (tc:type-definition-constructors def)
              :append
              (list `(coalton-impl:define-global-lexical
                         ,(tc:constructor-entry-name constructor)
                         ',(tc:constructor-entry-compressed-repr constructor))
                    `(deftype ,(tc:constructor-entry-classname constructor) ()
                       (quote (member ,(tc:constructor-entry-compressed-repr constructor)))))))

       ((tc:type-definition-newtype def)
        (let ((constructor (first (tc:type-definition-constructors def))))
          (list `(declaim (inline ,(tc:constructor-entry-name constructor)))
                `(defun ,(tc:constructor-entry-name constructor) (x) x)
                `(coalton-impl:define-global-lexical
                     ,(tc:constructor-entry-name constructor)
                     (F1 #',(tc:constructor-entry-name constructor))))))

       (t
        `(,(if (coalton-impl:coalton-release-p)
               `(defstruct (,(tc:type-definition-name def)
                            (:constructor nil)
                            (:predicate nil))
                  ,@(when (tc:type-definition-docstring def)
                      (list (tc:type-definition-docstring def))))

               `(defclass ,(tc:type-definition-name def) ()
                  ()
                  ,@(when (tc:type-definition-docstring def)
                      `((:documentation ,(tc:type-definition-docstring def))))))

          ,@(loop
              :for constructor :in (tc:type-definition-constructors def)
              :for classname := (tc:constructor-entry-classname constructor)
              :for superclass := (tc:type-definition-name def)
              :for constructor-name :=  (tc:constructor-entry-name constructor)
              :for fields
                := (loop :for field :in (tc:constructor-arguments constructor-name env)
                         :for runtime-type := (lisp-type field env)
                         :for i :from 0
                         :for name := (alexandria:format-symbol package "_~D" i)
                         :collect (make-struct-or-class-field
                                   :name name
                                   :type runtime-type))

              :for field-names := (mapcar #'struct-or-class-field-name fields)

              ;; Declare the constructor as inline in release mode
              :append
              (when (coalton-impl:coalton-release-p)
                (list `(declaim (inline ,constructor-name))))

              :append (struct-or-class
                       :classname classname
                       :constructor constructor-name
                       :superclass superclass
                       :fields fields
                       :mode (if (coalton-impl:coalton-release-p)
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
                             self)))

              :append (cond
                        ((zerop (tc:constructor-entry-arity constructor))
                         (list `(coalton-impl:define-global-lexical
                                    ,(tc:constructor-entry-name constructor)
                                    (,(tc:constructor-entry-name constructor)))))
                        (t
                         (let* ((arity (length field-names))
                                (entry (construct-function-entry
                                        `#',(tc:constructor-entry-name constructor)
                                        arity)))
                           (list `(coalton-impl:define-global-lexical
                                      ,(tc:constructor-entry-name constructor)
                                    ,entry))))))

          ,@(when (coalton-impl:coalton-release-p)
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
                             ,(format nil "~A :: ~A" name ty))))))
