(in-package #:coalton-impl/codegen)

(defun compile-type-definition (def env)
  (let ((package (symbol-package (type-definition-name def))))
    `((defstruct (,(type-definition-name def)
                  (:constructor nil)
                  (:predicate nil)
                  (:copier nil)))

      ,@(loop
          :for constructor :in (type-definition-constructors def)
          :for classname := (constructor-entry-classname constructor)
          :for slot-types := (mapcar #'coalton-impl/typechecker::fresh-inst (constructor-entry-arguments constructor))
          :for slot-names := (ctor-make-slot-names (length slot-types) package)
          :append
          `((declaim (inline ,(constructor-entry-name constructor)))
            (defstruct (,classname
                        (:include ,(type-definition-name def))
                        (:constructor ,(constructor-entry-name constructor) ,slot-names)
                        (:predicate nil)
                        (:copier nil))
              ,@(ctor-make-slots slot-names slot-types env))
            #+sbcl (declaim (sb-ext:freeze-type ,classname)))
          :collect (cond
                     ((= 0 (constructor-entry-arity constructor))
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
                                 (list ,@(mapcar (lambda (slot) `(slot-value self ',slot)) slot-names)))
                         self)))

          :append (cond
                    ((= 0 (constructor-entry-arity constructor))
                     `((coalton-impl::define-global-lexical
                           ,(constructor-entry-name constructor)
                           (,(constructor-entry-name constructor)))))
                    (t
                     (let* ((arity (length slot-names))
                            (entry (construct-function-entry
                                    `#',(constructor-entry-name constructor)
                                    arity)))
                       `((coalton-impl::define-global-lexical
                             ,(constructor-entry-name constructor)
                           ,entry))))))
      #+sbcl
      (declaim (sb-ext:freeze-type ,(type-definition-name def))))))

(defun ctor-make-slot-names (count package)
  (loop :for i :below count
        :collect (alexandria:format-symbol package "_~D" i)))

(defun ctor-make-slot (name type env)
  `(,name (error "") :type ,(lisp-type type env) :read-only t))

(defun ctor-make-slots (slot-names slot-types env)
  (loop :for slot-name :in slot-names
        :for slot-type :in slot-types
        :collect (ctor-make-slot slot-name slot-type env)))
