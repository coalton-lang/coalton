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
   (#:pred #:coalton-impl/typechecker/predicate))
  (:export
   #:derive-class-instance             ;; FUNCTION
   #:derive-methods                    ;; GENERIC FUNCTION
   #:constructor                       ;; STRUCT
   #:constructor-name                  ;; ACCESSOR
   #:constructor-fields                ;; ACCESSOR
   #:abstract-type-definition          ;; STRUCT
   #:abstract-type-definition-name     ;; ACCESSOR
   #:abstract-type-definition-type     ;; ACCESSOR
   #:abstract-type-definition-ctors    ;; ACCESSOR
   #:abstract-type-definition-location ;; ACCESSOR
   ))
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

(defun instance-main-pred-ty (inst)
  (first 
   (pred:ty-predicate-types 
    (env:ty-class-instance-predicate inst))))

(defun ty-name (ty)
  (etypecase ty
    (tc:tycon (tc:tycon-name ty))
    (tc:tyvar (tc:tyvar-id ty))
    (tc:tapp (let ((from (tc:tapp-from tapp)))
               (if (tc:tapp-p from)
                   (tapp-rator-name from)
                   (ty-name from))))
    (parser:tycon (parser:tycon-name ty))
    (parser:tyvar (parser:tyvar-name ty))
    (parser:tapp (let ((from (parser:tapp-from tapp)))
                   (if (parser:tapp-p from)
                       (tapp-rator-name from)
                       (ty-name from))))))

(defun tapp-rator-name (tapp)
  (etypecase tapp
    (parser:tapp
     )
    (tc:tapp 
     )))

(defgeneric resolve-type-constraint (type class env)
  (:method ((type parser:tycon) class env)
    (fset:do-seq (inst (class-instances class env))
      (let ((ty (instance-main-pred-ty inst)))
        (typecase ty
          (tc:tycon
           (when (eq (parser:tycon-name type) (tc:tycon-name ty))
             (return-from resolve-type-constraint '())))
          (tc:tapp
           (when (and (tc:tycon-p (tc:tapp-from ty))
                      (eq (parser:tycon-name type) (tc:tycon-name (tc:tapp-from ty))))
             (return-from resolve-type-constraint '()))))
        (when (and (tc:tycon-p ty)
                   (eq (parser:tycon-name type)
                       (tc:tycon-name ty))))))
    (error "Type ~a does not implement ~a, cannot derive." (parser:tycon-name type) class))
  (:method ((type parser:tyvar) class env)
    (list 
     (parser:make-ty-predicate
      :location (source:location type)
      :class (parser:make-identifier-src
              :location (source:location type)
              :name class)
      :types (list type))))
  (:method ((type parser:tapp) class env)
    (append
     (resolve-type-constraint (parser:tapp-from type) class env)
     (resolve-type-constraint (parser:tapp-to type) class env)
     )
    ))

;; Collect context constraints, and try to signal errors if we can't derive
;; TODO: improve error reporting for tapps, it only works well for simple tycons
(defgeneric collect-type-constraints (type class env)
  (:method ((type parser:toplevel-define-type) class env)
    (loop :for ctor :in (parser:toplevel-define-type-ctors type)
          :nconc (loop :for type :in (parser:constructor-fields ctor)
                       :nconc (resolve-type-constraint type class env))))
  (:method ((type parser:toplevel-define-struct) class env)
    (loop :for field :in (parser:toplevel-define-struct-fields type)
          :for type := (parser:struct-field-type field)
          :nconc (resolve-type-constraint type class env))))

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

(defparameter *env* nil)

(defun derive-class-instance (class type env)
  (declare (type symbol class)
    (type (or parser:toplevel-define-type parser:toplevel-define-struct))
    (type penv:partial-type-env env)
    (values parser:toplevel-define-instance &optional))

  (setf *env* env)

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
