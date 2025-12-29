(coalton-cffi/utils:define-cffi-package #:coalton-cffi/types
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-cffi/aliases
   #:coalton-cffi/complex)
  (:import-from
   #:coalton-library/types
   #:Proxy
   #:proxy-of)
  (:export
   #:ForeignType
   #:foreign-type-size
   #:foreign-struct-type?
   #:ForeignRepr
   #:foreign-repr
   #:foreign-size
   #:foreign-repr-of
   #:foreign-size-of
   #:coalton-type-to-foreign-type
   #:define-foreign-repr-instance))

(in-package #:coalton-cffi/types)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :native (cl:or cl:symbol cl:list))
  (define-type ForeignType
    "Foreign types which are known to the CFFI system, including built-in types, like `:int`, or user-defined types, like `(:struct my-struct)`.")

  (inline)
  (declare foreign-type-size (ForeignType -> Size))
  (define (foreign-type-size foreign-type)
    "The number of bytes used to represent an element of type `foreign-type`."
    (lisp Size (foreign-type)
      (cffi:foreign-type-size foreign-type)))

  (inline)
  (declare foreign-struct-type? (ForeignType -> Boolean))
  (define (foreign-struct-type? foreign-type)
    "Is `foreign-type` a foreign structure type?"
    (lisp Boolean (foreign-type)
      (cl:typep foreign-type '(cl:cons (cl:member :struct))))))

(coalton-toplevel

  (define-class (ForeignRepr :T)
    "A class of types which have a foreign representation."
    (foreign-repr
     "Get the foreign type associated with the type provided by proxy."
     (Proxy :T -> ForeignType))
    (foreign-size
     "Get the size in bytes of the type provided by proxy."
     (Proxy :T -> Size)))

  (inline)
  (declare foreign-repr-of (ForeignRepr :T => :T -> ForeignType))
  (define (foreign-repr-of x)
    "Get the foreign type associated with `x`."
    (foreign-repr (proxy-of x)))

  (inline)
  (declare foreign-size-of (ForeignRepr :T => :T -> Size))
  (define (foreign-size-of x)
    "Get the size in bytes of `x`."
    (foreign-size (proxy-of x))))

(cl:defun coalton-type-to-foreign-type (coalton-type)
  "Get the foreign type associated with `coalton-type`, provided as a symbol or list."
  (cl:eval `(coalton (foreign-repr (the (Proxy ,coalton-type) Proxy)))))

(cl:defmacro define-foreign-repr-instance (type ctype)
  "Define an instance of `ForeignRepr` for `type`, taking `ctype` as its foreign representation."
  `(coalton-toplevel
     (define-instance (ForeignRepr ,type)
       (inline)
       (define (foreign-repr _)
         (lisp ForeignType () ',ctype))
       (inline)
       (define (foreign-size _)
         (lisp Size () ,(cffi:foreign-type-size ctype))))))

(define-foreign-repr-instance U8  :uint8)
(define-foreign-repr-instance U16 :uint16)
(define-foreign-repr-instance U32 :uint32)
(define-foreign-repr-instance U64 :uint64)
(define-foreign-repr-instance I8  :int8)
(define-foreign-repr-instance I16 :int16)
(define-foreign-repr-instance I32 :int32)
(define-foreign-repr-instance I64 :int64)

(coalton-toplevel
  (define-instance (ForeignRepr Void)
    (inline)
    (define (foreign-repr _)
      (lisp ForeignType () ':void))
    (inline)
    (define (foreign-size _)
      0)))

(define-foreign-repr-instance String  :string)
(define-foreign-repr-instance Boolean :boolean)

(define-foreign-repr-instance F32 :float)
(define-foreign-repr-instance F64 :double)

(define-foreign-repr-instance (Complex F32) cl-complex-float)
(define-foreign-repr-instance (Complex F64) cl-complex-double)
