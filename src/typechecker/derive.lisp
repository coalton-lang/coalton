(in-package #:coalton-impl/typechecker/define-type)

(defun lookup-type-instance (type class env)
  (fset:find-if
   (lambda (inst)
     (let* ((pred (coalton-impl/typechecker/environment:ty-class-instance-predicate inst))
            (types (coalton-impl/typechecker/predicate:ty-predicate-types pred)))
       (ignore-errors 
        (eql (coalton-impl/typechecker/types:tycon-name (car types)) type))))
   (algo:immutable-listmap-lookup
    (tc:instance-environment-instances 
     (tc:environment-instance-environment 
      (coalton-impl/typechecker/partial-type-env:partial-type-env-env env)))
    class)))

(defmethod derive-class-instance ((type parser:toplevel-define-struct) (class (eql 'coalton-library/classes:eq)) env)
  (let ((ty-constraints)) 
    ;; Collect context constraints, and try to signal errors if we can't derive
    ;; TODO: improve error reporting for tapps, it only works well for simple tycons
    (loop :for field :in (parser:toplevel-define-struct-fields type)
          :for type := (parser:struct-field-type field)
          :do (etypecase type
                (parser:tycon
                 (unless (lookup-type-instance (parser:tycon-name type) class env)
                   (error "Field ~A does not implement EQ, cannot derive." (parser:struct-field-name field))))
                (parser:ty
                 (push type ty-constraints))))

    (let* ((location (source:location type))
           (classes-package (util:find-package "COALTON-LIBRARY/CLASSES"))
           (eq-class (parser:make-identifier-src :location location :name (util:find-symbol "EQ" classes-package)))
           (eq-method (parser:make-node-variable :location location :name (util:find-symbol "==" classes-package)))
           (struct-ty (loop :with ty := (parser:make-tycon :location location :name (parser:identifier-src-name (parser:toplevel-define-struct-name type)))
                            :for var :in (parser:toplevel-define-struct-vars type)
                            :for tyvar := (parser:make-tyvar :location location :name (parser:keyword-src-name var))
                            :do (setf ty (parser:make-tapp :location location :from ty :to tyvar))
                            :finally (return ty))))

      (parser:make-toplevel-define-instance
       :context (mapcar (lambda (constraint)
                          (parser:make-ty-predicate
                           :class eq-class
                           :types (list constraint)
                           :location location))
                        ty-constraints)
       :pred (parser:make-ty-predicate
              :class eq-class
              :types (list struct-ty)
              :location location)
       :docstring nil
       :methods (list
                 (parser:make-instance-method-definition
                  :name eq-method
                  :params (list
                           (parser:make-pattern-var
                            :location location
                            :name 'a
                            :orig-name 'a)
                           (parser:make-pattern-var
                            :location location
                            :name 'b
                            :orig-name 'b))
                  :body (parser:make-node-body
                         :nodes nil
                         :last-node (parser:make-node-and
                                     :location location
                                     :nodes (mapcar
                                             (lambda (field)
                                               (parser:make-node-application
                                                :location location
                                                :rator eq-method
                                                :rands (list
                                                        (parser:make-node-application
                                                         :location location
                                                         :rator (parser:make-node-accessor
                                                                 :location location
                                                                 :name (parser:struct-field-name field))
                                                         :rands (list
                                                                 (parser:make-node-variable
                                                                  :location location
                                                                  :name 'a)))
                                                        
                                                        (parser:make-node-application
                                                         :location location
                                                         :rator (parser:make-node-accessor
                                                                 :location location
                                                                 :name (parser:struct-field-name field))
                                                         :rands (list
                                                                 (parser:make-node-variable
                                                                  :location location
                                                                  :name 'b))))))
                                             (parser:toplevel-define-struct-fields type))))
                  :location location
                  :inline nil))
       :location location
       :head-location location
       :compiler-generated t))))
