(defpackage #:coalton-impl/error
  (:use #:cl)
  (:export
   #:coalton-error                      ; CONDITION
   #:coalton-warning                    ; CONDITION
   #:coalton-style-warning              ; CONDITION
   #:with-context                       ; MACRO
   #:coalton-type-error                 ; CONDITION
   ))

(in-package #:coalton-impl/error)

(define-condition coalton-error (error)
  ()
  (:documentation "Supertype for Coalton errors"))

(define-condition coalton-type-error (coalton-error)
  ()
  (:documentation "Supertype for Coalton type errors"))

(define-condition coalton-warning (warning)
  ()
  (:documentation "Supertype for Coalton warnings"))

(define-condition coalton-style-warning (style-warning)
  ()
  (:documentation "Supertype for Coalton style warnings"))

(defvar *error-context-stack* '()
  "The stack of context frames for the current operation")

(defmethod print-object :after ((er coalton-error) stream)
  (dolist (ctx *error-context-stack*)
    (format stream "~%In ~A" ctx)))

(defmethod print-object :after ((er coalton-warning) stream)
  (dolist (ctx *error-context-stack*)
    (format stream "~%In ~A" ctx)))

(defmethod print-object :after ((er coalton-style-warning) stream)
  (dolist (ctx *error-context-stack*)
    (format stream "~%In ~A" ctx)))

(defmacro with-context ((context &rest args) &body body)
  `(let ((*error-context-stack* (cons (format nil ,context ,@args) *error-context-stack*)))
     (progn ,@body)))
