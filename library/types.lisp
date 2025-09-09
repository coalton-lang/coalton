(coalton-library/utils:defstdlib-package #:coalton-library/types
  (:use
   #:coalton)
  (:local-nicknames
   (#:utils #:coalton-library/utils))
  (:export
   #:Proxy
   #:proxy-of
   #:as-proxy-of
   #:proxy-inner
   #:proxy-function-from
   #:proxy-function-to
   #:LispType
   #:RuntimeRepr #:runtime-repr #:coalton-type-string
   #:coalton-type-string-of
   #:runtime-repr-of))

(in-package #:coalton-library/types)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (repr :enum)
  (define-type (Proxy :a)
    "Proxy holds no data, but has a phantom type parameter."
    Proxy)

  (inline)
  (declare proxy-of (:a -> Proxy :a))
  (define (proxy-of _)
    "Returns a Proxy containing the type of the parameter."
    Proxy)

  (inline)
  (declare as-proxy-of (:a -> Proxy :a -> :a))
  (define (as-proxy-of x _)
    "Returns the parameter, forcing the proxy to have the same type as the parameter."
    x)

  (inline)
  (declare proxy-inner (Proxy (:a :b) -> Proxy :b))
  (define (proxy-inner _)
    Proxy)

  (inline)
  (declare proxy-function-from (Proxy (:a -> :b) -> Proxy :a))
  (define (proxy-function-from _)
    "Get a `Proxy` representing the \"domain\" of a function type."
    Proxy)

  (inline)
  (declare proxy-function-to (Proxy (:a -> :b) -> Proxy :b))
  (define (proxy-function-to _)
    "Get a `Proxy` representing the \"codomain\" of a function type."
    Proxy)

  (repr :native (cl:or cl:symbol cl:list))
  (define-type LispType
    "The runtime representation of a Coalton type as a Lisp type.")

  (define-class (RuntimeRepr :a)
    "Types which have a runtime LispType representation.

The compiler will auto-generate instances of `RuntimeRepr` for all defined types."
    (runtime-repr
     "The type emitted by the Coalton compiler for the type parameter to the given Proxy."
     (Proxy :a -> LispType))
    (coalton-type-string
     "Write the type `:a` as a string for debugging purposes."
     (Proxy :a -> String)))

  (inline)
  (declare runtime-repr-of (RuntimeRepr :a => :a -> LispType))
  (define (runtime-repr-of x)
    "Returns the runtime representation of the type of the given value."
    (runtime-repr (proxy-of x)))

  (inline)
  (declare coalton-type-string-of (RuntimeRepr :a => :a -> String))
  (define (coalton-type-string-of x)
    "Get the static type of the expression `x`."
    (coalton-type-string (proxy-of x)))

  ;; Additional RuntimeRepr instances for early-defined types

  (define-instance (RuntimeRepr Boolean)
    (inline)
    (define (runtime-repr _)
      (lisp LispType () 'cl:boolean))
    (define (coalton-type-string _)
      "Boolean"))

  (define-instance (RuntimeRepr Char)
    (inline)
    (define (runtime-repr _)
      (lisp LispType () 'cl:character))
    (define (coalton-type-string _)
      "Char"))

  (define-instance (RuntimeRepr Integer)
    (inline)
    (define (runtime-repr _)
      (lisp LispType () 'cl:integer))
    (define (coalton-type-string _)
      "Integer"))

  (define-instance (RuntimeRepr F32)
    (inline)
    (define (runtime-repr _)
      (lisp LispType () 'cl:single-float))
    (define (coalton-type-string _)
      "F32"))

  (define-instance (RuntimeRepr F64)
    (inline)
    (define (runtime-repr _)
      (lisp LispType () 'cl:double-float))
    (define (coalton-type-string _)
      "F64"))

  (define-instance (RuntimeRepr String)
    (inline)
    (define (runtime-repr _)
      (lisp LispType () 'cl:string))
    (define (coalton-type-string _)
      "String"))

  (define-instance (RuntimeRepr Fraction)
    (inline)
    (define (runtime-repr _)
      (lisp LispType () 'cl:rational))
    (define (coalton-type-string _)
      "Fraction"))

  (define-instance ((RuntimeRepr :a) (RuntimeRepr :b) => (RuntimeRepr (:a -> :b)))
    (inline)
    (define (runtime-repr _)
      (lisp LispType () 'coalton-impl/runtime/function-entry:function-entry))
    (define (coalton-type-string p)
      (utils:concat-string
       "(Arrow "
       (utils:concat-string
        (coalton-type-string (proxy-function-from p))
        (utils:concat-string
         " "
         (utils:concat-string
          (coalton-type-string (proxy-function-to p))
          ")"))))))

  (define-instance (RuntimeRepr :a => (RuntimeRepr (List :a)))
    (inline)
    (define (runtime-repr _)
      (lisp LispType () 'cl:list))
    (define (coalton-type-string p)
      (utils:concat-string
       "(List "
       (utils:concat-string
        (coalton-type-string (proxy-inner p))
        ")"))))

  (define-instance (RuntimeRepr :a => (RuntimeRepr (Optional :a)))
    (inline)
    (define (runtime-repr _)
      ;; If using `cl:t` proves to be inefficient we could try to
      ;; improve this, perhaps using proxy-inner.
      (lisp LispType () 'cl:t))
    (define (coalton-type-string p)
      (utils:concat-string
       "(Optional "
       (utils:concat-string
        (coalton-type-string (proxy-inner p))
        ")"))))

  ;; The compiler will not auto-generate RuntimeRepr instances for
  ;; types defined in this file to avoid circular dependencies.

  (define-instance (RuntimeRepr :a => (RuntimeRepr (Proxy :a)))
    (inline)
    (define (runtime-repr _)
      (lisp LispType () '(cl:member 'proxy/proxy)))
    (define (coalton-type-string p)
      "Proxy"
      (utils:concat-string
       "(Proxy "
       (utils:concat-string
        (coalton-type-string (proxy-inner p))
        ")"))))
  
  (define-instance (RuntimeRepr LispType)
    (inline)
    (define (runtime-repr _)
      (lisp LispType () '(cl:or cl:symbol cl:list)))
    (define (coalton-type-string _)
      "LispType")))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/TYPES")
