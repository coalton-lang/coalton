(defpackage #:coalton-impl/typechecker/derive
  (:use #:cl)
  (:local-nicknames
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker/stage-1)
   (#:env #:coalton-impl/typechecker/environment)
   (#:penv #:coalton-impl/typechecker/partial-type-env)
   )
  (:export
   #:derive-class-instance ))
(in-package #:coalton-impl/typechecker/derive)

(defstruct constructor
  "Abstract representation of a type constructor."
  (name   (util:required 'name)   :type parser:identifier-src :read-only t)
  (fields (util:required 'fields) :type parser:ty-list        :read-only t))

(defun constructor-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'constructor-p x)))

(deftype constructor-list ()
  '(satisfies constructor-list-p))

(defstruct abstract-type-definition
  "Abstract representation of a type definition to apply the same method
to `define-type' and `define-struct' nodes."
  (name     (util:required 'name)     :type symbol           :read-only t)
  (type     (util:required 'type)     :type parser:ty        :read-only t)
  (ctors    (util:required 'ctors)    :type constructor-list :read-only t)
  (location (util:required 'location) :type source:location  :read-only t))

(defun class-instances (class env)
  (algo:immutable-listmap-lookup
   (tc:instance-environment-instances
    (tc:environment-instance-environment
     (penv:partial-type-env-env env)))
   class))

(defgeneric lookup-type-constraint (type class env)
  (:method ((type parser:tycon) class env)
    (fset:do-seq (inst (class-instances class env))
      (let* ((pred (env:ty-class-instance-predicate inst))
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

;; Collect context constraints, and try to signal errors if we can't derive
;; TODO: improve error reporting for tapps, it only works well for simple tycons
(defgeneric collect-type-constraints (type class env)
  (:method ((type parser:toplevel-define-type) class env)
    (loop :for ctor :in (parser:toplevel-define-type-ctors type)
          :nconc (loop :for type :in (parser:constructor-fields ctor)
                       :for typred := (lookup-type-constraint type class env)
                       :unless typred
                         :do (error "Type ~A does not implement ~A, cannot derive."
                                    (parser:tycon-name type)
                                    class)
                       :end
                       :unless (parser:tycon-p type)
                         :collect typred)))
  (:method ((type parser:toplevel-define-struct) class env)
    (loop :for field :in (parser:toplevel-define-struct-fields type)
          :for type := (parser:struct-field-type field)
          :for typred := (lookup-type-constraint type class env)
          :unless typred
            :do (error "Type ~A does not implement ~A, cannot derive."
                       (parser:tycon-name type)
                       class)
          :end
          :unless (parser:tycon-p type)
            :collect typred)))

(defgeneric parser-definition-type (def)
  (:method ((def parser:toplevel-define-struct))
    (loop :with ty := (parser:make-tycon
                       :location (source:location def)
                       :name (parser:identifier-src-name
                              (parser:toplevel-define-struct-name def)))
          :for var :in (parser:toplevel-define-struct-vars def)
          :for tyvar := (parser:make-tyvar
                         :location (source:location def)
                         :name (parser:keyword-src-name var))
          :do (setf ty (parser:make-tapp
                        :location (source:location def)
                        :from ty
                        :to tyvar))
          :finally (return ty)))
  (:method ((def parser:toplevel-define-type))
    (loop :with ty := (parser:make-tycon
                       :location (source:location def)
                       :name (parser:identifier-src-name
                              (parser:toplevel-define-type-name def)))
          :for var :in (parser:toplevel-define-type-vars def)
          :for tyvar := (parser:make-tyvar
                         :location (source:location def)
                         :name (parser:keyword-src-name var))
          :do (setf ty (parser:make-tapp
                        :location (source:location def)
                        :from ty
                        :to tyvar))
          :finally (return ty))))

(defgeneric parser-definition-abstract-type-definition (def)
  (:method ((def parser:toplevel-define-struct))
    (make-abstract-type-definition
     :location (source:location def)
     :name (parser:identifier-src-name (parser:toplevel-define-struct-name def))
     :type (parser-definition-type def)
     :ctors
     (list
      (make-constructor
       :name (parser:toplevel-define-struct-name def)
       :fields (mapcar
                #'parser:struct-field-type
                (parser:toplevel-define-struct-fields def))))))
  (:method ((def parser:toplevel-define-type))
    (make-abstract-type-definition
     :location (source:location def)
     :name (parser:identifier-src-name (parser:toplevel-define-type-name def))
     :type (parser-definition-type def)
     :ctors
     (mapcar
      (lambda (ctor)
        (make-constructor
         :name (parser:constructor-name ctor)
         :fields (parser:constructor-fields ctor)))
      (parser:toplevel-define-type-ctors def)))))

(defgeneric derive-methods (class type-definition env))

(defun derive-class-instance (class type env)
  (declare (type symbol class)
    (type (or parser:toplevel-define-type parser:toplevel-define-struct))
    (type penv:partial-type-env env)
    (values parser:toplevel-define-instance &optional))

  (let ((type-definition (parser-definition-abstract-type-definition type)))
    (handler-case
        (parser:make-toplevel-define-instance
         :location (source:location type)
         :head-location (source:location type)
         :context (collect-type-constraints type class env)
         :pred (parser:make-ty-predicate
                :location (source:location type)
                :class (parser:make-identifier-src
                        :location (source:location type)
                        :name class)
                :types (list (abstract-type-definition-type type-definition)))
         :docstring nil
         :compiler-generated t
         :methods (derive-methods class type-definition env))
      (error (c)
        (error "Cannot derive ~A for type ~A~%~A"
               class
               (abstract-type-definition-name type-definition)
               c)))))

#+#:eq-deriver
(defmethod derive-methods ((class (eql 'coalton-library/classes:eq)) type-definition env)
  (let ((location (abstract-type-definition-location type-definition)))
    (list
     (parser:make-instance-method-definition
      :name (parser:make-node-variable
             :location location
             :name 'coalton-library/classes:==)
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
                                :rator (parser:make-node-variable
                                        :location location
                                        :name 'coalton-library/classes:Tuple)
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
                                                       (constructor-fields ctor)))
                                             (cfields-b
                                               (mapcar (lambda (_)
                                                         (declare (ignore _))
                                                         (gensym "ctor-field"))
                                                       (constructor-fields ctor))))
                                         (parser:make-node-match-branch
                                          :location location
                                          :pattern (parser:make-pattern-constructor
                                                    :location location
                                                    :name 'coalton-library/classes:Tuple
                                                    :patterns (list
                                                               (parser:make-pattern-constructor
                                                                :location location
                                                                :name (parser:identifier-src-name (constructor-name ctor))
                                                                :patterns (mapcar
                                                                           (lambda (cfield)
                                                                             (parser:make-pattern-var
                                                                              :location location
                                                                              :name cfield
                                                                              :orig-name cfield))
                                                                           cfields-a))
                                                               (parser:make-pattern-constructor
                                                                :location location
                                                                :name (parser:identifier-src-name (constructor-name ctor))
                                                                :patterns (mapcar
                                                                           (lambda (cfield)
                                                                             (parser:make-pattern-var
                                                                              :location location
                                                                              :name cfield
                                                                              :orig-name cfield))
                                                                           cfields-b))))
                                          :body (parser:make-node-body
                                                 :nodes nil
                                                 :last-node (parser:make-node-and
                                                             :location location
                                                             :nodes (append
                                                                     (mapcar
                                                                      (lambda (cfield-a cfield-b)
                                                                        (parser:make-node-application
                                                                         :location location
                                                                         :rator (parser:make-node-variable
                                                                                 :location location
                                                                                 :name 'coalton-library/classes:==)
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
                                     (abstract-type-definition-ctors type-definition))
                                    (if (= 1 (length (abstract-type-definition-ctors type-definition)))
                                        nil
                                        (list
                                         (parser:make-node-match-branch
                                          :location location
                                          :pattern (parser:make-pattern-wildcard
                                                    :location location)
                                          :body (parser:make-node-body
                                                 :nodes nil
                                                 :last-node (parser:make-node-variable
                                                             :location location
                                                             :name 'coalton:False))))))))
      :location location
      :inline nil))))
