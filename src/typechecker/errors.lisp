(defpackage #:coalton-impl/typechecker/errors
  (:use #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:coalton-type-error                   ; CONDITION
   #:with-type-context                    ; MACRO
   ))

(in-package #:coalton-impl/typechecker/errors)

(defvar *include-type-error-context* t
  "Whether to rethrow type errors with their enclosing context. This can be disabled for easier debugging of the compiler.")

(defvar *type-error-context-stack* '()
  "The stack of context frames for the current type operation")

(define-condition coalton-type-error (util:coalton-error)
  ()
  (:documentation "A type error from Coalton code."))

(defmethod print-object :after ((er coalton-type-error) stream)
  (dolist (ctx *type-error-context-stack*)
    (format stream "~%In ~A" ctx)))

(defmacro with-type-context ((context &rest args) &body body)
  `(let ((*type-error-context-stack* (if *include-type-error-context*
                                         (cons (format nil ,context ,@args) *type-error-context-stack*)
                                         *type-error-context-stack*)))
     (progn ,@body)))
