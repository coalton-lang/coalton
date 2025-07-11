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

(defun parser-definition-type-constraints (def class)
  (declare (type (or parser:toplevel-define-struct parser:toplevel-define-type) def)
           (type symbol class)
           (values parser:ty-predicate-list &optional))

  (mapcar (lambda (type)
            (parser:make-ty-predicate
             :location (source:location type)
             :class (parser:make-identifier-src
                     :location (source:location type)
                     :name class)
             :types (list type)))
          (alexandria:mappend #'parser:type-definition-ctor-field-types
                              (parser:type-definition-ctors def))))

(defun parser-definition-type-signature (def)
  (declare (type (or parser:toplevel-define-struct parser:toplevel-define-type) def)
           (values parser:ty &optional))

  (labels ((apply-type-argument-list (ty args)
             (if (endp args)
                 ty
                 (apply-type-argument-list
                  (parser:make-tapp
                   :location (source:location def)
                   :from ty
                   :to (first args))
                  (rest args)))))
    (apply-type-argument-list
     (parser:make-tycon
      :location (source:location def)
      :name (parser:identifier-src-name (parser:type-definition-name def)))
     (mapcar (lambda (var)
               (parser:make-tyvar
                :location (source:location def)
                :name (parser:keyword-src-name var)))
             (parser:type-definition-vars def)))))

(defgeneric derive-methods (class def env)
  (:documentation "User-defined methods for implementing derivers.
Specialize on `class'."))

(defun derive-class-instance (class def env)
  "Entrypoint for deriver implementations.
This function creates an abstraction over `parser:toplevel-define-type' and
`parser:toplevel-define-struct', Creates a `parser:toplevel-define-instance'
node with context constraints, and then calls `derive-methods' to generate
the actual methods."
  (declare (type symbol class)
           (type (or parser:toplevel-define-type parser:toplevel-define-struct) def)
           (type penv:partial-type-env env)
           (values parser:toplevel-define-instance &optional))

  (handler-case
      (parser:make-toplevel-define-instance
       :location (source:location def)
       :head-location (source:location def)
       :context (parser-definition-type-constraints def class)
       :pred (parser:make-ty-predicate
              :location (source:location def)
              :class (parser:make-identifier-src
                      :location (source:location def)
                      :name class)
              :types (list (parser-definition-type-signature def)))
       :docstring nil
       :compiler-generated t
       :methods (derive-methods class def env))
    (error (c)
      (error "Failed to derive ~A for type ~A:~%~A"
             class
             (parser:type-definition-name def)
             c))))
