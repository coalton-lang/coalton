(in-package #:coalton-impl)

(defun coalton:print-value-db (&optional package)
  "Print the global value environment"
  (tc:print-value-db *global-environment* package))

(defun coalton:print-type-db (&optional package)
  "Print the global type environment"
  (tc:print-type-db *global-environment* package))

(defun coalton:print-class-db (&optional package)
  "Print the global class environment"
  (tc:print-class-db *global-environment* package))

(defun coalton:print-instance-db (&optional package)
  "Print the global instance environment"
  (tc:print-instance-db *global-environment* package))

(defun coalton:print-specializations (&optional package)
  "Print all specializations"
  (tc:print-specializations *global-environment* package))

(defun coalton:type-of (symbol)
  "Lookup the type of value SYMBOL in the global environment"
  (tc:lookup-value-type *global-environment* symbol))

(defun coalton:kind-of (symbol)
  "Lookup the kind of type SYMBOL in the global environment"
  (tc:kind-of (coalton-impl/typechecker::type-entry-type (coalton-impl::lookup-type *global-environment* symbol))))

(defun coalton:lookup-code (name)
  "Lookup the compiled code of a given definition"
  (declare (type symbol name))
  (tc:lookup-code *global-environment* name))
