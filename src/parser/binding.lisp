;;;; src/parser/binding.lisp
;;;;
;;;; This file defines a generic protocol for writing code that
;;;; operatates on both node-let-binding and toplevel-define structs.
;;;;

(defpackage #:coalton-impl/parser/binding
  (:use
   #:cl
   #:coalton-impl/parser/expression
   #:coalton-impl/parser/parser)
  (:export
   #:name                               ; FUNCTION
   #:value                              ; FUNCTION
   #:source                             ; FUNCTION
   #:parameters                         ; FUNCTION
   #:toplevel                           ; FUNCTION
   ))

(in-package #:coalton-impl/parser/binding)

(defgeneric name (binding)
  (:documentation "Returns the name that BINDING binds")

  (:method ((binding node-let-binding))
    (declare (values node-variable))
    (node-let-binding-name binding))

  (:method ((binding toplevel-define))
    (declare (values node-variable))
    (toplevel-define-name binding)))

(defgeneric value (binding)
  (:documentation "Returns the value that BINDING binds")

  (:method ((binding node-let-binding))
    (declare (values node))
    (node-let-binding-value binding))

  (:method ((binding toplevel-define))
    (declare (values node-body))
    (toplevel-define-body binding)))

(defgeneric source (binding)
  (:documentation "Returns the source location of BINDING")

  (:method ((binding node-let-binding))
    (declare (values cons))
    (node-let-binding-source binding))

  (:method ((binding toplevel-define))
    (declare (values cons))
    (toplevel-define-source binding)))

(defgeneric parameters (binding)
  (:documentation "Returns the parameters bound in BINDING")

  (:method ((binding node-let-binding))
    (declare (values node-variable-list))
    nil)

  (:method ((binding toplevel-define))
    (declare (values node-variable-list))
    (toplevel-define-vars binding)))

(defgeneric toplevel (binding)
  (:documentation "Returns t if BINDING is a toplevel binding.")

  (:method ((binding node-let-binding))
    (declare (values boolean))
    nil)

  (:method ((binding toplevel-define))
    (declare (values boolean))
    t))
