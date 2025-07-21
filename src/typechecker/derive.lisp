(defpackage #:coalton-impl/typechecker/derive
  (:use #:cl)
  (:local-nicknames
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker/stage-1)
   (#:env #:coalton-impl/typechecker/environment)
   (#:pred #:coalton-impl/typechecker/predicate))
  (:export
   #:toplevel-derivation                 ;; STRUCT
   #:make-toplevel-derivation            ;; CONSTRUCTOR
   #:toplevel-derivation-list            ;; TYPE
   #:derive-class-instance               ;; FUNCTION
   #:derive-class-instances              ;; FUNCTION
   #:derive-methods                      ;; GENERIC FUNCTION
   ))

(in-package #:coalton-impl/typechecker/derive)

(deftype parser-definition ()
  '(or parser:toplevel-define-type parser:toplevel-define-struct))

(defstruct toplevel-derivation
  (type-definition (util:required 'type-definition) :type parser-definition :read-only t)
  (class           (util:required 'class)           :type symbol            :read-only t))

(defun toplevel-derivation-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'toplevel-derivation-p x)))

(deftype toplevel-derivation-list ()
  '(satisfies toplevel-derivation-list-p))

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
EQL-specialize on symbol `class'."))

(defun derive-class-instances (derivations env)
  (declare (type toplevel-derivation-list derivations)
           (type tc:environment env)
           (values parser:toplevel-define-instance-list &optional))

  (mapcar (alexandria:rcurry #'derive-class-instance env) derivations))

(defun derive-class-instance (derivation env)
  "Entrypoint for deriver implementations.  Given symbol `class' and
parser type definition `def', produce a derived instance definition."
  (declare (type toplevel-derivation derivation)
           (type tc:environment env)
           (values parser:toplevel-define-instance &optional))

  (let ((class (toplevel-derivation-class derivation))
        (def   (toplevel-derivation-type-definition derivation))) 
    (let ((type-name (parser:identifier-src-name (parser:type-definition-name def)))) 
      (when (null (tc:lookup-class env class :no-error t))
        (tc:tc-error (format nil "Cannot derive class ~A for type ~A." class type-name)
                     (source:note (parser:type-definition-derive def)
                                  "Class ~A does not exist"
                                  class)
                     (source:note def
                                  "when deriving class ~A for type ~A."
                                  class
                                  type-name)))

      (when (endp (parser:type-definition-ctors def))
        (tc:tc-error (format nil "Cannot derive class ~A for type ~A." class type-name)
                     (source:note (parser:type-definition-derive def)
                                  "Type ~A has no constructors"
                                  type-name)
                     (source:note def
                                  "when deriving class ~A for type ~A."
                                  class
                                  type-name)))

      (when (endp (compute-applicable-methods #'derive-methods (list class def env)))
        (tc:tc-error (format nil "Cannot derive class ~A for type ~A." class type-name)
                     (source:note (parser:type-definition-derive def)
                                  "Deriver for class ~A is not implemented"
                                  class)
                     (source:note def
                                  "when deriving class ~A for type ~A."
                                  class
                                  type-name))))

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
     :methods (derive-methods class def env))))
