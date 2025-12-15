(coalton-library/utils:defstdlib-package #:coalton-library/symbol
  (:documentation "An interface to Common Lisp symbols.")
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/functions
   #:coalton-library/classes)
  (:local-nicknames
   (#:compat #:coalton-compatibility))
  (:export
   #:Symbol

   #:symbol-name
   #:uninterned?
   #:keyword?
   #:make-keyword
   #:make-symbol
   #:gensym
   ))

(in-package #:coalton-library/symbol)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel

  (repr :native cl:symbol)
  (define-type Symbol
    "A Common Lisp symbol.")

  (inline)
  (declare symbol-name (Symbol -> String))
  (define (symbol-name s)
    "Return the name of the symbol `s`."
    (lisp String (s)
      (cl:symbol-name s)))

  (inline)
  (declare uninterned? (Symbol -> Boolean))
  (define (uninterned? s)
    "Is the symbol `s` uninterned?"
    (lisp Boolean (s)
      (cl:null (cl:symbol-package s))))

  (inline)
  (declare keyword? (Symbol -> Boolean))
  (define (keyword? s)
    "Is the symbol `s` a Common Lisp keyword?"
    (lisp Boolean (s)
      (to-boolean (cl:keywordp s))))

  (inline)
  (declare make-keyword (String -> Symbol))
  (define (make-keyword s)
    "Find or make a keyword named `s`.

**WARNING**: This function interns a new symbol. It will not get garbage collected. Use with caution."
    (lisp Symbol (s)
      (cl:values (cl:intern s "KEYWORD"))))
  
  (inline)
  (declare make-symbol (String -> Symbol))
  (define (make-symbol s)
    "Make an uninterned symbol with the name `s`."
    (lisp Symbol (s)
      (cl:make-symbol s)))

  (inline)
  (declare gensym (Unit -> Symbol))
  (define (gensym)
    "Make an uninterned symbol as by `cl:gensym`."
    (lisp Symbol ()
      (cl:gensym)))

  (define-instance (Eq Symbol)
    (inline)
    (define (== s1 s2)
      (lisp Boolean (s1 s2)
        (to-boolean (cl:eq s1 s2)))))

  (define-instance (Default Symbol)
    (inline)
    (define (default)
      (lisp Symbol ()
        'cl:nil))))

(compat:try-lock-package "COALTON-LIBRARY/SYMBOL")
