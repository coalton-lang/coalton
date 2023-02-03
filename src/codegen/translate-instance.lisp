(defpackage #:coalton-impl/codegen/translate-instance
  (:use
   #:cl
   #:coalton-impl/util
   #:coalton-impl/codegen/ast
   #:coalton-impl/codegen/resolve-instance)
  (:import-from
   #:coalton-impl/codegen/typecheck-node
   #:typecheck-node)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)
   (#:error #:coalton-impl/error))
  (:export
   #:translate-instance                   ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/translate-instance)

(defun translate-instance (instance add-inline env)
  (error "not impl"))
