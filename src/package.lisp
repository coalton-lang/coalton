(in-package #:cl-user)

(defpackage #:coalton
  (:documentation "Public interface to COALTON.")
  (:use)                                ; Keep the package clean!

  (:import-from
   #:common-lisp
   #:in-package)
  (:export
   #:in-package)

  (:export #:call-coalton-function)

  (:export
   #:coalton-toplevel
   #:coalton-codegen
   #:coalton-codegen-ast
   #:coalton
   #:declare
   #:define
   #:define-type
   #:define-class
   #:define-instance
   #:repr
   #:monomorphize
   #:specialize
   #:unable-to-codegen)
  
  ;; Early Types
  (:export
   #:-> #:→
   #:=> #:⇒
   #:∀
   #:Unit
   #:Void
   #:Boolean #:True #:False
   #:Char
   #:U8
   #:U16
   #:U32
   #:U64
   #:I8
   #:I16
   #:I32
   #:I64
   #:Integer
   #:IFix
   #:UFix
   #:Single-Float
   #:Double-Float
   #:String
   #:Fraction
   #:Arrow
   #:List #:Cons #:Nil)

  ;; Primitive Syntax
  (:export
   #:fn #:λ
   #:match
   #:let
   #:=                                  ; Syntax
   #:lisp
   #:<-                                 ; Syntax
   #:_
   #:return
   #:the)

  ;; Macros
  (:export
   #:if
   #:when
   #:unless
   #:and
   #:or
   #:cond
   #:nest
   #:pipe
   #:.<
   #:.>
   #:make-list
   #:to-boolean
   #:do
   #:progn
   #:assert)

  (:export
   #:print-value-db
   #:print-type-db
   #:print-class-db
   #:print-instance-db
   #:print-specializations
   #:lookup-code
   #:lookup-class
   #:lookup-fundeps
   #:type-of
   #:kind-of)

  (:intern
   #:seq
   #:bind
   #:Boolean/True
   #:Boolean/False
   #:Unit/Unit))
