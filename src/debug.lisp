(in-package #:coalton-impl)

(defun print-value-db (&optional package)
  "Print the global value environment"
  (coalton-impl/typechecker::print-value-db *global-environment* package))

(defun print-type-db (&optional package)
  "Print the global type environment"
  (coalton-impl/typechecker::print-type-db *global-environment* package))

(defun print-class-db (&optional package)
  "Print the global class environment"
  (coalton-impl/typechecker::print-class-db *global-environment* package))

(defun print-instance-db (&optional package)
  "Print the global instance environment"
  (coalton-impl/typechecker::print-instance-db *global-environment* package))

(defun coalton:type-of (symbol)
  "Lookup the type of value SYMBOL in the global environment"
  (coalton-impl::lookup-value-type *global-environment* symbol))

(defun coalton:kind-of (symbol)
  "Lookup the kind of type SYMBOL in the global environment"
  (coalton-impl/typechecker::kind-of (coalton-impl/typechecker::type-entry-type (coalton-impl::lookup-type *global-environment* symbol))))

(defun coalton:lookup-code (name)
  "Lookup the compiled code of a given definition"
  (declare (type symbol name))
  (coalton-impl/typechecker::lookup-code *global-environment* name))
