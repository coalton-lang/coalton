(coalton-library/utils::defstdlib-package #:coalton-library/builtin
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-compatibility-layer)
  (:local-nicknames
   (#:compat #:coalton-compatibility-layer))
  (:export
   #:unreachable
   #:undefined
   #:error ; re-export from classes
   #:not
   #:xor
   #:boolean-not
   #:boolean-or
   #:boolean-and
   #:boolean-xor))

(in-package #:coalton-library/builtin)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(defmacro unreachable (cl:&optional (datum "Unreachable") cl:&rest arguments)
  "Signal an error with CL format string DATUM and optional format arguments ARGUMENTS."
  `(lisp :a ()
     (cl:error ,datum ,@arguments)))

(coalton-toplevel
  (define (undefined _)
    "A function which can be used in place of any value, throwing an error at runtime."
    (error "Undefined"))

  (inline)
  (declare not (Boolean -> Boolean))
  (define (not x)
    "Synonym for `boolean-not`."
    (boolean-not x))

  (inline)
  (declare xor (Boolean -> Boolean -> Boolean))
  (define (xor x y)
    "Synonym for `boolean-xor`."
    (boolean-xor x y))

  (inline)
  (declare boolean-not (Boolean -> Boolean))
  (define (boolean-not x)
    "The logical negation of `x`. Is `x` false?"
    (lisp Boolean (x)
      (cl:not x)))

  (inline)
  (declare boolean-or (Boolean -> Boolean -> Boolean))
  (define (boolean-or x y)
    "Is either `x` or `y` true? Note that this is a *function* which means both `x` and `y` will be evaluated. Use the `or` macro for short-circuiting behavior."
    (lisp Boolean (x y)
      (cl:or x y)))

  (inline)
  (declare boolean-and (Boolean -> Boolean -> Boolean))
  (define (boolean-and x y)
    "Are both `x` and `y` true? Note that this is a *function* which means both `x` and `y` will be evaluated. Use the `and` macro for short-circuiting behavior."
    (lisp Boolean (x y)
      (cl:and x y)))

  (inline)
  (declare boolean-xor (Boolean -> Boolean -> Boolean))
  (define (boolean-xor x y)
    "Are `x` or `y` true, but not both?"
    (match x
      ((True) (boolean-not y))
      ((False) y))))


(compat:try-lock-package "COALTON-LIBRARY/BUILTIN")
