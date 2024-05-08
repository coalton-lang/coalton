(defpackage #:coalton-impl/codegen/base
  (:use
   #:cl)
  (:export
   #:emit
   #:emit-ast
   #:emit-comment
   #:emit-env))

(in-package #:coalton-impl/codegen/base)

;;; protocol: compiler backend

;; These generic functions are the interface to compiler backends.
;; Implementations are defined in src/compiler.lisp.

(defgeneric emit (stream form)
  (:documentation "Emit a Lisp form")
  (:method (stream form)
    (values)))

(defgeneric emit-ast (stream name type value)
  (:documentation "Emit an AST entry")
  (:method (stream name type value)
    (values)))

(defgeneric emit-comment (stream comment)
  (:documentation "Emit a comment")
  (:method (stream comment)
    (values)))

;; Multiple definitions of the same functions are emitted to the
;; global environment (via 'set-code') before and during optimization,
;; so it's currently up to the backend to, e.g., provide logic to emit
;; only final, optimized versions.

(defgeneric emit-env (stream name args)
  (:documentation "Emit a global environment update")
  (:method (stream name args)
    (values)))
