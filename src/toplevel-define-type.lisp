(in-package #:coalton-impl)

;;; Handling of toplevel COALTON:DEFINE-TYPE.

(defun process-toplevel-type-definitions (deftype-forms repr-table env)
  (declare (type list deftype-forms)
           (type environment env)
           (values type-definition-list
                   environment))

  ;; Parse type definitions into a list of TYPE-DEFINITION objects
  (let ((parsed-deftypes (parse-type-definitions deftype-forms repr-table env)))
    (declare (type type-definition-list parsed-deftypes))

    (dolist (parsed-deftype parsed-deftypes)
      (let ((type-name (type-definition-name parsed-deftype)))

        ;; Add the type to the type environment
        (setf env
              (set-type
               env
               type-name
               (type-entry
                :name type-name
                :runtime-type (type-definition-runtime-type parsed-deftype)
                :type (type-definition-type parsed-deftype)
                :enum-repr (type-definition-enum-repr parsed-deftype)
                :newtype (type-definition-newtype parsed-deftype)
                :docstring (type-definition-docstring parsed-deftype))))

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
                            (constructor-entry-name ctor)))))))

    (values parsed-deftypes env)))
