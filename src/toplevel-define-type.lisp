(in-package #:coalton-impl)

;;; Handling of toplevel COALTON:DEFINE-TYPE.

(defun make-auto-addressable-instance (type-def)
  (declare (type-definition type-def)
           (values list &optional))
  (let* ((name (type-definition-name type-def))
         (tvars (loop :for i :below (kind-arity (tycon-kind (tcon-tycon (type-definition-type type-def))))
                      :collect (alexandria:format-symbol :keyword "~d" i)))
         (full-type (if tvars
                        `(,name ,@tvars)
                        name))
         (addressable-class
           (alexandria:ensure-symbol "ADDRESSABLE" (find-package "COALTON-LIBRARY/CLASSES")))

         (eq?
           (alexandria:ensure-symbol "EQ?" (find-package "COALTON-LIBRARY/CLASSES"))))
    `(coalton:define-instance (,addressable-class ,full-type)
       (coalton:define (,eq? a b)
         (coalton:lisp coalton:Boolean (a b)
           (eq a b))))))

(defun maybe-auto-addressable-instance (type-def)
  (declare (type-definition type-def)
           (values list &optional))
  (when (explicit-repr-auto-addressable-p (type-definition-explicit-repr type-def))
    (make-auto-addressable-instance type-def)))

(defun process-parsed-toplevel-type-definition (env parsed-deftype &aux (type-name (type-definition-name parsed-deftype)))
  (declare (environment env)
           (type-definition parsed-deftype)
           (values environment list &optional))
  ;; Add the type to the type environment
  (setf env
        (set-type
         env
         type-name
         (type-entry
          :name type-name
          :runtime-type (type-definition-runtime-type parsed-deftype)
          :type (type-definition-type parsed-deftype)
          :explicit-repr (type-definition-explicit-repr parsed-deftype)
          :enum-repr (type-definition-enum-repr parsed-deftype)
          :newtype (type-definition-newtype parsed-deftype)
          :docstring (type-definition-docstring parsed-deftype)
          :location (or *compile-file-pathname* *load-truename*))))
  (loop :for ctor :in (type-definition-constructors parsed-deftype)
        :for ctor-type :in (type-definition-constructor-types parsed-deftype)
        :for ctor-name := (constructor-entry-name ctor)

        ;; Add the constructors to the constructor environment
        :do
           (setf env
                 (set-constructor
                  env
                  (constructor-entry-name ctor) ctor))

           ;; Add the constructor as a value to the value environment
           (setf env
                 (set-value-type
                  env
                  ctor-name
                  ctor-type))

           ;; Register the constructor in the name environment
           (setf env
                 (set-name
                  env
                  (constructor-entry-name ctor)
                  (make-name-entry
                   :name (constructor-entry-name ctor)
                   :type :constructor
                   :docstring nil
                   :location (or *compile-file-pathname*
                                 *load-truename*))))

           ;; If the constructor takes paramaters then add it to the function environment
           (if (not (= (constructor-entry-arity ctor) 0))
               (setf env
                     (set-function
                      env
                      (constructor-entry-name ctor)
                      (make-function-env-entry
                       :name (constructor-entry-name ctor)
                       :arity (constructor-entry-arity ctor))))

               ;; If the constructor does not take parameters then remove it from the function environment
               (setf env
                     (unset-function
                      env
                      (constructor-entry-name ctor)))))
  (values env (maybe-auto-addressable-instance parsed-deftype)))

(defun process-toplevel-type-definitions (deftype-forms declares repr-table env)
  "Returns a list of TYPE-DEFINITIONs, a new ENVIRONMENT, and a list of INSTANCE-DEFINITIONs for auto-defined instances."
  (declare (type list deftype-forms)
           (type hash-table declare-forms)
           (type environment env)
           (values type-definition-list
                   environment
                   list
                   &optional))

  ;; Parse type definitions into a list of TYPE-DEFINITION objects
  (let ((parsed-deftypes (parse-type-definitions deftype-forms declares repr-table env))
        all-instances)
    (declare (type type-definition-list parsed-deftypes))
    (dolist (parsed-deftype parsed-deftypes (values parsed-deftypes env all-instances))
      (multiple-value-bind (new-env instance)
          (process-parsed-toplevel-type-definition env parsed-deftype)
        (setf env new-env)
        (when instance
          (push instance all-instances))))))
