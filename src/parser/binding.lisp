;;;;
;;;; This file defines a generic protocol for writing code that
;;;; operatates on both node-let-binding and toplevel-define structs.
;;;;

;;; TODO: rename name to binding-name and so on

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
   #:restricted                         ; FUNCTION
   #:last-node                          ; FUNCTION
   ))

(in-package #:coalton-impl/parser/binding)

(defgeneric name (binding)
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

(defgeneric value (binding)
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

(defgeneric source (binding)
  (:documentation "Returns the source location of BINDING")

  (:method ((binding node-let-binding))
    (declare (values cons))
    (node-let-binding-source binding))

  (:method ((binding toplevel-define))
    (declare (values cons))
    (toplevel-define-source binding))

  (:method ((binding instance-method-definition))
    (declare (values cons))
    (instance-method-definition-source binding)))

(defgeneric parameters (binding)
  (:documentation "Returns the parameters bound in BINDING")

  (:method ((binding node-let-binding))
    (declare (values node-variable-list))
    nil)

  (:method ((binding toplevel-define))
    (declare (values node-variable-list))
    (toplevel-define-vars binding))

  (:method ((binding instance-method-definition))
    (declare (values node-variable-list))
    (instance-method-definition-vars binding)))

(defgeneric toplevel (binding)
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

(defgeneric restricted (binding)
  (:documentation "Returns t if BINDING is a lambda.")

  (:method ((binding node-let-binding))
    (declare (values boolean))
     (node-abstraction-p (node-let-binding-value binding)))

  (:method ((binding toplevel-define))
    (declare (values boolean))
    (and (or (toplevel-define-vars binding)

             (and (null (node-body-nodes (toplevel-define-body binding)))
                  (node-abstraction-p (node-body-last-node (toplevel-define-body binding)))))
         t))

  (:method ((binding instance-method-definition))
    (declare (values boolean))
    (and (or (instance-method-definition-vars binding)

             (and (null (node-body-nodes (instance-method-definition-body binding)))
                  (node-abstraction-p (node-body-last-node (instance-method-definition-body binding)))))
         t)))

(defgeneric last-node (binding)
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
