(coalton-library/utils:defstdlib-package #:coalton-library/typeable
  (:use #:coalton)
  (:export
   #:Typeable #:TypeRep #:typeOf
   #:Proxy #:toProxy))

(cl:in-package #:coalton-library/typeable)

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(coalton-toplevel

  (declare-type Proxy (:a -> *))
  (define-type (Proxy :a)
    "Proxy is a singleton with a phantom type. It is polykinded:

   - `(the (Proxy Integer) Proxy)'
   - `(the (Proxy List) Proxy)'
   - `(the (Proxy Arrow) Proxy)'
"
    Proxy)

  (repr :native coalton-impl/typechecker::ty)
  (define-type TypeRep
    "Value level type representation.")

  (define-class (Typeable :a)
    "Typeable is a default instance for all defined types.
It allows for introspection of the current type within a polymorphic function."
    (typeRep ((Proxy :a) -> TypeRep)))

  (declare toProxy (:a -> Proxy :a))
  (define (toProxy _)
    "Converts a value to its Proxy."
    Proxy)

  (declare typeOf (Typeable :a => :a -> TypeRep))
  (define (typeOf a)
    "Returns the TypeRep representing the type of A."
    (typeRep (toProxy a)))

  (define-instance ((Typeable :z) (Typeable :e) => Typeable (:z :e))
    (define (typeRep x)
      (let a = (typeRep ((the (Proxy (:a :b) -> (Proxy :a)) (fn (_) Proxy)) x)))
      (let b = (typeRep ((the (Proxy (:a :b) -> (Proxy :b)) (fn (_) Proxy)) x)))

      (lisp TypeRep (a b)
        (coalton-impl/typechecker::%make-tapp a b)))))

(cl:defmacro %define-early-typeable (type cl:&optional name)
  `(coalton-toplevel
     (define-instance
         (Typeable
          ,(cl:if name name
               (coalton-impl/typechecker:tycon-name
                (coalton-impl/typechecker:tcon-tycon (cl:eval type)))))
       (define (typeRep _)
         (lisp TypeRep ()
           ,type)))))

(%define-early-typeable coalton-impl/typechecker:*boolean-type*)
(%define-early-typeable coalton-impl/typechecker:*char-type*)
(%define-early-typeable coalton-impl/typechecker:*integer-type*)
(%define-early-typeable coalton-impl/typechecker:*single-float-type*)
(%define-early-typeable coalton-impl/typechecker:*double-float-type*)
(%define-early-typeable coalton-impl/typechecker:*string-type*)
(%define-early-typeable coalton-impl/typechecker:*fraction-type*)
(%define-early-typeable coalton-impl/typechecker:*arrow-type* Arrow)
(%define-early-typeable coalton-impl/typechecker:*list-type*)

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/TYPEABLE")
