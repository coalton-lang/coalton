(in-package #:coalton-impl/typechecker/define-type)

(defun class-instances (class env)
  (algo:immutable-listmap-lookup
   (tc:instance-environment-instances
    (tc:environment-instance-environment
     (partial-type-env-env env)))
   class))

(defgeneric tcty->parserty (tcty location)
  (:method ((tcty tc:tycon) location)
    (loop :with ty := (parser:make-tycon :location location :name (tc:tycon-name tcty))
          :for i :below (tc:kind-arity (tc:tycon-kind tcty))
          :for tyvar := (parser:make-tyvar :location location :name (alexandria:format-symbol util:+keyword-package+ "~d" i))
          :do (setf ty (parser:make-tapp :location location :from ty :to tyvar))
          :finally (return ty)))
  (:method ((tcty tc:tyvar) location)
    (parser:make-tyvar
     :location location
     :name (alexandria:format-symbol util:+keyword-package+ "~d" (tc:tyvar-id tcty))))
  (:method ((tcty tc:tapp) location)
    (parser:make-tapp
     :location location
     :from (tcty->parserty (tc:tapp-from tcty) location)
     :to (tcty->parserty (tc:tapp-to tcty) location))))

(defgeneric lookup-type-constraint (type class env location)
  (:method ((type tc:tycon) class env location)
    (fset:do-seq (inst (class-instances class env))
      (let* ((pred (coalton-impl/typechecker/environment:ty-class-instance-predicate inst))
             (tys (coalton-impl/typechecker/predicate:ty-predicate-types pred))
             (ty (first tys)))
        (if (and (= 1 (length tys))
                 (tc:tycon-p ty)
                 (tc:ty= type ty))
            (return-from lookup-type-constraint
              (parser:make-ty-predicate
               :location location
               :class (parser:make-identifier-src
                       :location location
                       :name class)
               :types (list (tcty->parserty type location))))
            nil))))
  (:method ((type tc:tyvar) class env location)
    (parser:make-ty-predicate
     :location location
     :class (parser:make-identifier-src
             :location location
             :name class)
     :types (list (tcty->parserty type location))))
  (:method ((type tc:tapp) class env location)
    (parser:make-ty-predicate
     :location location
     :class (parser:make-identifier-src
             :location location
             :name class)
     :types (list (tcty->parserty type location)))))

(defun collect-type-constraints (type-definition class env)
  (loop :for ctor-args :in (type-definition-constructor-args type-definition)
        :nconc (loop :for type :in ctor-args
                     :for typred := (lookup-type-constraint type class env (source:location type-definition))
                     :unless typred
                       :do (error "Type ~A does not implement ~A, cannot derive." (parser:tycon-name type) class)
                     :end
                     ;; :when (parser:tycon-p type)
                     :collect typred)))

(defparameter dci-type nil)
(defparameter dci-env nil)

(defmethod derive-class-instance ((type type-definition) (class (eql 'coalton-library/classes:eq)) env)
  (setq dci-type type)
  (setq dci-env env)

  (let* ((location (source:location type))
         (classes-package (util:find-package "COALTON-LIBRARY/CLASSES"))
         (eq-class (parser:make-identifier-src :location location :name (util:find-symbol "EQ" classes-package)))
         (eq-method (parser:make-node-variable :location location :name (util:find-symbol "==" classes-package)))
         (tuple-cons (parser:make-node-variable :location location :name (util:find-symbol "TUPLE" classes-package)))
         (struct-ty (loop :with ty := (parser:make-tycon :location location :name (type-definition-name type))
                          :for i :below (tc:kind-arity (tc:tycon-kind (type-definition-type type)))
                          :for tyvar := (parser:make-tyvar :location location :name (alexandria:format-symbol util:+keyword-package+ "~d" i))
                          :do (setf ty (parser:make-tapp :location location :from ty :to tyvar))
                          :finally (return ty))))

    (parser:make-toplevel-define-instance
     :context (collect-type-constraints type class env)
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
                                                         (loop :for i :below (tc:constructor-entry-arity ctor)
                                                               :collect (gensym "ctor-field")))
                                                       (cfields-b
                                                         (loop :for i :below (tc:constructor-entry-arity ctor)
                                                               :collect (gensym "ctor-field"))))
                                                   (parser:make-node-match-branch
                                                    :location location
                                                    :pattern (parser:make-pattern-constructor
                                                              :location location
                                                              :name (util:find-symbol "TUPLE" classes-package)
                                                              :patterns (list
                                                                         (parser:make-pattern-constructor
                                                                          :location location
                                                                          :name (tc:constructor-entry-name ctor)
                                                                          :patterns (mapcar
                                                                                     (lambda (cfield)
                                                                                       (parser:make-pattern-var
                                                                                        :location location
                                                                                        :name cfield
                                                                                        :orig-name cfield))
                                                                                     cfields-a))
                                                                         (parser:make-pattern-constructor
                                                                          :location location
                                                                          :name (tc:constructor-entry-name ctor)
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
                                               (type-definition-constructors type))
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
     :compiler-generated t)))

(defmethod derive-class-instance ((type parser:toplevel-define-type) (class (eql 'coalton-library/classes:eq)) env)
  (setq dci-type type)
  (setq dci-env env)
  (let ((ty-constraints (collect-type-constraints type class env)))
    ;; Collect context constraints, and try to signal errors if we can't derive
    ;; TODO: improve error reporting for tapps, it only works well for simple tycons


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
