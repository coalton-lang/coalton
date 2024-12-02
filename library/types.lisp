(coalton-library/utils:defstdlib-package #:coalton-library/types
  (:use
   #:coalton)
  (:export
   #:Proxy
   #:proxy-of
   #:as-proxy-of
   #:proxy-inner
   #:LispType
   #:RuntimeRepr #:runtime-repr
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

  (declare proxy-of (:a -> Proxy :a))
  (define (proxy-of _)
    "Returns a Proxy containing the type of the parameter."
    Proxy)

  (declare as-proxy-of (:a -> Proxy :a -> :a))
  (define (as-proxy-of x _)
    "Returns the parameter, forcing the proxy to have the same type as the parameter."
    x)

  (declare proxy-inner (Proxy (:a :b) -> Proxy :b))
  (define (proxy-inner _)
    Proxy)

  (declare proxy-fun (Proxy (:a -> :b) -> Proxy :a))
  (define (proxy-fun _)
    Proxy)

  (declare proxy-arg (Proxy (:a -> :b) -> Proxy :b))
  (define (proxy-arg _)
    Proxy)

  (repr :native (cl:or cl:symbol cl:list))
  (define-type LispType
    "The runtime representation of a Coalton type as a lisp type.")

  (define-class (RuntimeRepr :a)
    "Types which have a runtime LispType representation.

`runtime-repr` corresponds to the type emitted by the Coalton compiler for the type parameter to the given Proxy.

The compiler will auto-generate instances of `RuntimeRepr` for all defined types."
    (runtime-repr (Proxy :a -> LispType)))

  (declare runtime-repr-of (RuntimeRepr :a => :a -> LispType))
  (define (runtime-repr-of x)
    "Returns the runtime representation of the type of the given value."
    (runtime-repr (proxy-of x)))

  ;; Additional RuntimeRepr instances for early-defined types

  (define-instance (RuntimeRepr Boolean)
    (define (runtime-repr _)
      (lisp LispType () 'cl:boolean)))

  (define-instance (RuntimeRepr Char)
    (define (runtime-repr _)
      (lisp LispType () 'cl:character)))

  (define-instance (RuntimeRepr Integer)
    (define (runtime-repr _)
      (lisp LispType () 'cl:integer)))

  (define-instance (RuntimeRepr Single-Float)
    (define (runtime-repr _)
      (lisp LispType () 'cl:single-float)))

  (define-instance (RuntimeRepr Double-Float)
    (define (runtime-repr _)
      (lisp LispType () 'cl:double-float)))

  (define-instance (RuntimeRepr String)
    (define (runtime-repr _)
      (lisp LispType () 'cl:string)))

  (define-instance (RuntimeRepr Fraction)
    (define (runtime-repr _)
      (lisp LispType () 'cl:rational)))

  (define-instance ((RuntimeRepr :a) (RuntimeRepr :b) => RuntimeRepr (:a -> :b))
    (define (runtime-repr x)
      (let ((repr-a (runtime-repr (proxy-fun x)))
            (repr-b (runtime-repr (proxy-arg x))))
        (lisp LispType (repr-a repr-b)
          `(-> ,repr-a ,repr-b)))))

  (define-instance (RuntimeRepr :a => RuntimeRepr (List :a))
    (define (runtime-repr x)
      (let ((repr-a (runtime-repr (proxy-inner x))))
        (lisp LispType (repr-a)
          `(cl:list ,repr-a)))))

  ;; The compiler will not auto-generate RuntimeRepr instances for
  ;; types defined in this file to avoid circular dependencies.
  
  (define-instance (RuntimeRepr LispType)
    (define (runtime-repr _)
      (lisp LispType () '(cl:or cl:symbol cl:list))))

  (define-instance (RuntimeRepr (Proxy :a))
    (define (runtime-repr _)
      (lisp LispType () '(cl:member 'proxy/proxy)))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/TYPES")
