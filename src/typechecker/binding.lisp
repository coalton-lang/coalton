(defpackage #:coalton-impl/typechecker/binding
  (:use
   #:cl
   #:coalton-impl/typechecker/expression
   #:coalton-impl/typechecker/toplevel)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:binding-type                       ; TYPE
   #:binding-type-list                  ; TYPE
   #:binding-type                       ; FUNCTION
   #:binding-value                      ; FUNCTION
   #:binding-parameters                 ; FUNCTION
   #:binding-restricted-p               ; FUNCTION
   #:binding-last-node                  ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/binding)

(deftype binding-type ()
  '(or node-let-binding toplevel-define instance-method-definition))

(deftype binding-type-list ()
  '(or node-let-binding-list toplevel-define-list instance-method-definition-list))

(defgeneric binding-type (binding)
  (:documentation "Returns the type of BINDING")

  (:method ((binding node-let-binding))
    (declare (values tc:qualified-ty))

    (node-type (node-let-binding-name binding)))

  (:method ((binding toplevel-define))
    (declare (values tc:qualified-ty))

    (node-type (toplevel-define-name binding)))

  (:method ((binding instance-method-definition))
    (declare (values tc:qualified-ty))

    (node-type (instance-method-definition-name binding))))

(defgeneric binding-value (binding)
  (:documentation "Returns the value bound by BINDING")

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
  (:documentation "Returns the parameters of BINDING")

  (:method ((binding node-let-binding))
    (declare (values node-variable-list))

    nil)

  (:method ((binding toplevel-define))
    (declare (values node-variable-list))

    (toplevel-define-vars binding))

  (:method ((binding instance-method-definition))
    (declare (values node-variable-list))

    (instance-method-definition-vars binding)))

(defgeneric binding-restricted-p (binding)
  (:documentation "Returns t if BINDING is restricted")

  (:method ((binding node-let-binding))
    (declare (values boolean))

    (node-abstraction-p (node-let-binding-value binding)))

  (:method ((binding toplevel-define))
    (declare (values boolean))

    (and
     (or (toplevel-define-vars binding)

         (and (null (node-body-nodes (toplevel-define-body binding)))
              (node-abstraction-p (node-body-last-node (toplevel-define-body binding)))))
     t))

  (:method ((binding instance-method-definition))
    (declare (values boolean))

    (and
     (or (instance-method-definition-vars binding)

         (and (null (node-body-nodes (instance-method-definition-body binding)))
              (node-abstraction-p
               (node-body-last-node (instance-method-definition-body binding)))))
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
