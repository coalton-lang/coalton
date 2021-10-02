(in-package #:coalton-impl/codegen)

(defun compile-type-definition (def env)
  (let ((package (symbol-package (type-definition-name def))))

    (cond
      ((type-definition-enum-repr def)
       (loop :for constructor :in (type-definition-constructors def)
             :append
             (list `(coalton-impl::define-global-lexical
                        ,(constructor-entry-name constructor)
                        ',(constructor-entry-compressed-repr constructor))
                   `(deftype ,(constructor-entry-classname constructor) ()
                      (quote (member ,(constructor-entry-compressed-repr constructor)))))))

      ((type-definition-newtype def)
       (let ((constructor (first (type-definition-constructors def))))
         (list `(declaim (inline ,(constructor-entry-name constructor)))
               `(defun ,(constructor-entry-name constructor) (x) x)
               `(coalton-impl::define-global-lexical
                    ,(constructor-entry-name constructor)
                    (coalton-impl/codegen::F1 #',(constructor-entry-name constructor))))))

      (t
       `(,(if (eql coalton-impl::*interaction-mode* ':release)
              `(defstruct (,(type-definition-name def)
                           (:constructor nil)
                           (:predicate nil)))

              `(defclass ,(type-definition-name def) ()
                 ()))

         ,@(loop
             :for constructor :in (type-definition-constructors def)
             :for classname := (constructor-entry-classname constructor)
             :for superclass := (type-definition-name def)
             :for constructor-name :=  (constructor-entry-name constructor)
             :for fields
               := (loop :for field :in (constructor-entry-arguments constructor)
                        :for type := (lisp-type field env)
                        :for i :from 0
                        :for name := (alexandria:format-symbol package "_~D" i)
                        :collect (make-struct-or-class-field
                                  :name name
                                  :type type))

             :for field-names := (mapcar #'struct-or-class-field-name fields)

             ;; Declare the constructor as inline in release mode
             :append
             (when (eql coalton-impl::*interaction-mode* :release)
               (list `(declaim (inline ,constructor-name))))

             :append (struct-or-class
                      :classname classname
                      :constructor constructor-name
                      :superclass superclass
                      :fields fields
                      :mode (if (eql coalton-impl::*interaction-mode* :release)
                                :struct
                                :class))

             :collect (cond
                        ((zerop (constructor-entry-arity constructor))
                         `(defmethod print-object ((self ,classname) stream)
                            (declare (type stream stream)
                                     (type ,classname self)
                                     (values ,classname))
                            (format stream "#.~s" ',(constructor-entry-name constructor))
                            self))
                        (t
                         `(defmethod print-object ((self ,classname) stream)
                            (declare (type stream stream)
                                     (type ,classname self)
                                     (values ,classname))
                            (format stream "#.(~s~{ ~s~})"
                                    ',(constructor-entry-name constructor)
                                    (list ,@(mapcar (lambda (slot) `(slot-value self ',slot)) field-names)))
                            self)))

             :append (cond
                       ((zerop (constructor-entry-arity constructor))
                        (list `(coalton-impl::define-global-lexical
                                   ,(constructor-entry-name constructor)
                                   (,(constructor-entry-name constructor)))))
                       (t
                        (let* ((arity (length field-names))
                               (entry (construct-function-entry
                                       `#',(constructor-entry-name constructor)
                                       arity)))
                          (list `(coalton-impl::define-global-lexical
                                     ,(constructor-entry-name constructor)
                                   ,entry))))))

         ,@(when (eql coalton-impl::*interaction-mode* ':release)
             (list
              #+sbcl
              `(declaim (sb-ext:freeze-type ,(type-definition-name def))))))))))
