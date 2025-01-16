(in-package #:cl-user)

(defpackage #:coalton
  (:documentation "Public interface to COALTON.")
  (:use)                                ; Keep the package clean!

  (:import-from
   #:common-lisp
   #:in-package
   #:defpackage)
  (:export
   #:in-package
   #:defpackage)

  (:export #:call-coalton-function)

  (:export
   #:coalton-toplevel
   #:coalton-codegen
   #:coalton-codegen-ast
   #:coalton-codegen-types
   #:coalton
   #:declare
   #:define
   #:define-type
   #:define-type-alias
   #:define-struct
   #:define-class
   #:define-instance
   #:repr
   #:lisp-toplevel
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
   #:the
   #:while
   #:while-let
   #:loop
   #:break
   #:continue
   #:for
   #:in                                 ; Syntax
   )

  ;; Macros
  (:export
   #:if
   #:when
   #:unless
   #:and
   #:or
   #:cond
   #:as
   #:try-as
   #:unwrap-as
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
   #:describe-type-of
   #:describe-type-alias
   #:set-type-printing-mode
   #:kind-of)

  (:intern
   #:seq
   #:bind
   #:Boolean/True
   #:Boolean/False
   #:Unit/Unit))
