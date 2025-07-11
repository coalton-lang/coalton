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
  "Abstract representation of a type constructor used for implementing derivers."
  (name   (util:required 'name)   :type parser:identifier-src :read-only t)
  (fields (util:required 'fields) :type parser:ty-list        :read-only t))

(defun constructor-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'constructor-p x)))

(deftype constructor-list ()
  '(satisfies constructor-list-p))

(defstruct abstract-type-definition
  "Abstract representation of a type definition to apply the same method
to `parser:toplevel-define-type' and `parser:toplevel-define-struct' nodes.
Used for implementing derivers."
  (name     (util:required 'name)     :type symbol           :read-only t)
  (type     (util:required 'type)     :type parser:ty        :read-only t)
  (ctors    (util:required 'ctors)    :type constructor-list :read-only t)
  (location (util:required 'location) :type source:location  :read-only t))

(defun parser-definition-type-constraints (def class)
  (declare (type (or parser:toplevel-define-struct parser:toplevel-define-type) def)
           (type symbol class))

  (mapcar (lambda (type)
            (parser:make-ty-predicate
             :location (source:location type)
             :class (parser:make-identifier-src
                     :location (source:location type)
                     :name class)
             :types (list type)))
          (alexandria:mappend #'parser:type-definition-ctor-field-types
                              (parser:type-definition-ctors def))))

(defun parser-definition-type (def)
  (etypecase def
    (parser:toplevel-define-struct
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
    (parser:toplevel-define-type
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
           :finally (return ty)))))

(defun parser-definition-abstract-type-definition (def)
  (etypecase def
    (parser:toplevel-define-struct
     (make-abstract-type-definition
      :location (source:location def)
      :name (parser:identifier-src-name (parser:toplevel-define-struct-name def))
      :type (parser-definition-type def)
      :ctors (list
              (make-constructor
               :name (parser:toplevel-define-struct-name def)
               :fields (parser:type-definition-ctor-field-types def)))))
    (parser:toplevel-define-type
     (make-abstract-type-definition
      :location (source:location def)
      :name (parser:identifier-src-name (parser:toplevel-define-type-name def))
      :type (parser-definition-type def)
      :ctors (mapcar
              (lambda (ctor)
                (make-constructor
                 :name (parser:constructor-name ctor)
                 :fields (parser:type-definition-ctor-field-types ctor)))
              (parser:toplevel-define-type-ctors def))))))

(defgeneric derive-methods (class type-definition env)
  (:documentation "User-defined methods for implementing derivers.
Specialize on `class'."))

(defun derive-class-instance (class type env)
  "Entrypoint for deriver implementations.
This function creates an abstraction over `parser:toplevel-define-type' and
`parser:toplevel-define-struct', Creates a `parser:toplevel-define-instance'
node with context constraints, and then calls `derive-methods' to generate
the actual methods."
  (declare (type symbol class)
           (type (or parser:toplevel-define-type parser:toplevel-define-struct))
           (type penv:partial-type-env env)
           (values parser:toplevel-define-instance &optional))

  (let ((type-definition (parser-definition-abstract-type-definition type)))
    (handler-case
        (parser:make-toplevel-define-instance
         :location (source:location type)
         :head-location (source:location type)
         :context (parser-definition-type-constraints type class)
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
        (error "Failed to derive ~A for type ~A:~%~A"
               class
               (abstract-type-definition-name type-definition)
               c)))))
