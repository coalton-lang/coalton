(in-package #:coalton-impl/typechecker/define-type)

(defun class-instances (class env)
  (algo:immutable-listmap-lookup 
   (tc:instance-environment-instances 
    (tc:environment-instance-environment 
     (partial-type-env-env env)))
   class))

(defgeneric lookup-type-constraint (type class env)
  (:method ((type parser:tycon) class env)
    (fset:do-seq (inst (class-instances class env))
      (let* ((pred (coalton-impl/typechecker/environment:ty-class-instance-predicate inst))
              (tys (coalton-impl/typechecker/predicate:ty-predicate-types pred))
              (ty (first tys)))
         (if (and (= 1 (length tys))
                  (tc:tycon-p ty)
                  (eq (parser:tycon-name type)
                      (tc:tycon-name ty)))
             (return-from lookup-type-constraint
               (parser:make-ty-predicate
                :location (source:location type)
                :class (parser:make-identifier-src
                        :location (source:location type)
                        :name class)
                :types (list type)))
             nil))))
  (:method ((type parser:tyvar) class env)
    (parser:make-ty-predicate
     :location (source:location type)
     :class (parser:make-identifier-src
             :location (source:location type)
             :name class)
     :types (list type)))
  (:method ((type parser:tapp) class env)
    (parser:make-ty-predicate
     :location (source:location type)
     :class (parser:make-identifier-src
             :location (source:location type)
             :name class)
     :types (list type))))

(defparameter dci-type nil)
(defparameter dci-env nil)

(defmethod derive-class-instance ((type parser:toplevel-define-type) (class (eql 'coalton-library/classes:eq)) env)
  (setq dci-type type)
  (setq dci-env env)
  (let ((ty-constraints)) 
    ;; Collect context constraints, and try to signal errors if we can't derive
    ;; TODO: improve error reporting for tapps, it only works well for simple tycons
    (loop :for ctor :in (parser:toplevel-define-type-ctors type)
          :do (loop :for type :in (parser:constructor-fields ctor)
                    :for typred := (lookup-type-constraint type class env)
                    :do (if typred
                            (unless (parser:tycon-p type) 
                              (push typred ty-constraints))
                            (error "Type ~A does not implement ~A, cannot derive." (parser:tycon-name type) class))))

    (let* ((location (source:location type))
           (classes-package (util:find-package "COALTON-LIBRARY/CLASSES"))
           (eq-class (parser:make-identifier-src :location location :name (util:find-symbol "EQ" classes-package)))
           (eq-method (parser:make-node-variable :location location :name (util:find-symbol "==" classes-package)))
           (tuple-cons (parser:make-node-variable :location location :name (util:find-symbol "TUPLE" classes-package)))
           (struct-ty (loop :with ty := (parser:make-tycon :location location :name (parser:identifier-src-name (parser:toplevel-define-type-name type)))
                            :for var :in (parser:toplevel-define-type-vars type)
                            :for tyvar := (parser:make-tyvar :location location :name (parser:keyword-src-name var))
                            :do (setf ty (parser:make-tapp :location location :from ty :to tyvar))
                            :finally (return ty))))

      (print ty-constraints)

      (parser:make-toplevel-define-instance
       :context ty-constraints
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
                         :last-node (parser:make-node-match
                                     :location location
                                     :expr (parser:make-node-application
                                            :location location
                                            :rator tuple-cons
                                            :rands (list
                                                    (parser:make-node-variable
                                                     :location location
                                                     :name 'a)
                                                    (parser:make-node-variable
                                                     :location location
                                                     :name 'b)))
                                     :branches (append
                                                (mapcar
                                                 (lambda (ctor)
                                                   (let ((cfields-a
                                                           (mapcar (lambda (_)
                                                                     (declare (ignore _))
                                                                     (gensym "ctor-field"))
                                                                   (parser:constructor-fields ctor)))
                                                         (cfields-b
                                                           (mapcar (lambda (_)
                                                                     (declare (ignore _))
                                                                     (gensym "ctor-field"))
                                                                   (parser:constructor-fields ctor)))) 
                                                     (parser:make-node-match-branch
                                                      :location location
                                                      :pattern (parser:make-pattern-constructor
                                                                :location location
                                                                :name (util:find-symbol "TUPLE" classes-package)
                                                                :patterns (list
                                                                           (parser:make-pattern-constructor
                                                                            :location location
                                                                            :name (parser:identifier-src-name (parser:constructor-name ctor))
                                                                            :patterns (mapcar
                                                                                       (lambda (cfield)
                                                                                         (parser:make-pattern-var
                                                                                          :location location
                                                                                          :name cfield
                                                                                          :orig-name cfield))
                                                                                       cfields-a))
                                                                           (parser:make-pattern-constructor
                                                                            :location location
                                                                            :name (parser:identifier-src-name (parser:constructor-name ctor))
                                                                            :patterns (mapcar
                                                                                       (lambda (cfield)
                                                                                         (parser:make-pattern-var
                                                                                          :location location
                                                                                          :name cfield
                                                                                          :orig-name cfield))
                                                                                       cfields-b))))
                                                      :body (parser:make-node-body
                                                             ;; :location location
                                                             :nodes nil
                                                             :last-node (parser:make-node-and
                                                                         :location location
                                                                         :nodes (append
                                                                                 (mapcar 
                                                                                  (lambda (cfield-a cfield-b)
                                                                                    (parser:make-node-application
                                                                                     :location location
                                                                                     :rator eq-method
                                                                                     :rands (list
                                                                                             (parser:make-node-variable
                                                                                              :location location
                                                                                              :name cfield-a)
                                                                                             (parser:make-node-variable
                                                                                              :location location
                                                                                              :name cfield-b))))
                                                                                  cfields-a
                                                                                  cfields-b)
                                                                                 (list 
                                                                                  (parser:make-node-variable
                                                                                   :location location
                                                                                   :name 'coalton:True))))))))
                                                 (parser:toplevel-define-type-ctors type))
                                                (list
                                                 (parser:make-node-match-branch
                                                  :location location
                                                  :pattern (parser:make-pattern-wildcard
                                                            :location location)
                                                  :body (parser:make-node-body
                                                         :nodes nil
                                                         :last-node (parser:make-node-variable
                                                                     :location location
                                                                     :name 'coalton:False)))))))
                  :location location
                  :inline nil))
       :location location
       :head-location location
       :compiler-generated t))))

(defmethod derive-class-instance ((type parser:toplevel-define-struct) (class (eql 'coalton-library/classes:eq)) env)
  (let ((ty-constraints)) 
    ;; Collect context constraints, and try to signal errors if we can't derive
    ;; TODO: improve error reporting for tapps, it only works well for simple tycons
    (loop :for field :in (parser:toplevel-define-struct-fields type)
          :for type := (parser:struct-field-type field)
          :for typred := (lookup-type-constraint type class env)
          :do (if typred
                  (push typred ty-constraints)
                  (error "Type ~A does not implement ~A, cannot derive." (parser:tycon-name type) class)))

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
       :context ty-constraints
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
