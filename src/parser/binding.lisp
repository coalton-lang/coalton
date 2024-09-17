;;;;
;;;; This file defines a generic protocol for writing code that
;;;; operates on both node-let-binding and toplevel-define structs.
;;;;

(defpackage #:coalton-impl/parser/binding
  (:use
   #:cl
   #:coalton-impl/parser/base
   #:coalton-impl/parser/pattern
   #:coalton-impl/parser/expression
   #:coalton-impl/parser/toplevel)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:export
   #:binding-name                       ; FUNCTION
   #:binding-value                      ; FUNCTION
   #:binding-parameters                 ; FUNCTION
   #:binding-toplevel-p                 ; FUNCTION
   #:binding-function-p                 ; FUNCTION
   #:binding-last-node                  ; FUNCTION
   ))

(in-package #:coalton-impl/parser/binding)

(defgeneric binding-name (binding)
  (:documentation "Returns the name that BINDING binds")

  (:method ((binding node-let-binding))
    (declare (values node-variable))
    (node-let-binding-name binding))

  (:method ((binding toplevel-define))
    (declare (values node-variable))
    (toplevel-define-name binding))

  (:method ((binding instance-method-definition))
    (declare (values node-variable))
    (instance-method-definition-name binding)))

(defgeneric binding-value (binding)
  (:documentation "Returns the value that BINDING binds")

  (:method ((binding node-let-binding))
    (declare (values node))
    (node-let-binding-value binding))

  (:method ((binding toplevel-define))
    (declare (values node-body))
    (toplevel-define-body binding))

  (:method ((binding instance-method-definition))
    (declare (values node-body))
    (instance-method-definition-body binding)))

(defgeneric binding-parameters (binding)
  (:documentation "Returns the parameters bound in BINDING")

  (:method ((binding node-let-binding))
    (declare (values pattern-list))
    nil)

  (:method ((binding toplevel-define))
    (declare (values pattern-list &optional))
    (toplevel-define-params binding))

  (:method ((binding instance-method-definition))
    (declare (values pattern-list &optional))
    (instance-method-definition-params binding)))

(defgeneric binding-toplevel-p (binding)
  (:documentation "Returns t if BINDING is a toplevel binding.")

  (:method ((binding node-let-binding))
    (declare (values boolean))
    nil)

  (:method ((binding toplevel-define))
    (declare (values boolean))
    t)

  (:method ((binding instance-method-definition))
    (declare (values boolean))
    t))

(defgeneric initform-abstraction-p (expr)
  (:method ((expr node-abstraction))
    (declare (ignorable expr))
    t)
  (:method ((expr node-the))
    (initform-abstraction-p (node-the-expr expr)))
  (:method (expr)
    (declare (ignorable expr))
    nil))

(defgeneric binding-function-p (binding)
  (:documentation "Returns t if BINDING is a lambda.")

  (:method ((binding node-let-binding))
    (declare (values boolean &optional))
    (initform-abstraction-p (node-let-binding-value binding)))

  (:method ((binding toplevel-define))
    (declare (values boolean))
    (and (or (toplevel-define-params binding)

             (and (null (node-body-nodes (toplevel-define-body binding)))
                  (initform-abstraction-p (node-body-last-node (toplevel-define-body binding)))))
         t))

  (:method ((binding instance-method-definition))
    (declare (values boolean))
    (and (or (instance-method-definition-params binding)

             (and (null (node-body-nodes (instance-method-definition-body binding)))
                  (initform-abstraction-p (node-body-last-node (instance-method-definition-body binding)))))
         t)))

(defgeneric binding-last-node (binding)
  (:documentation "Returns the last node in BINDING")

  (:method ((binding node-let-binding))
    (declare (values node))
    (node-let-binding-value binding))

  (:method ((binding toplevel-define))
    (declare (values node))
    (node-body-last-node (toplevel-define-body binding)))

  (:method ((binding instance-method-definition))
    (declare (values node))
    (node-body-last-node (instance-method-definition-body binding))))
