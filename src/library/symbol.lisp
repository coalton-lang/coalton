;;;; symbol.lisp
;;;;
;;;; Symbols implemented as Common Lisp symbols.

(cl:in-package #:coalton-library)

(cl:defpackage #:coalton-interned-symbols
  (:use)
  (:documentation "A package for the Symbol data type to intern symbols into."))

(coalton-toplevel
 (define-type Symbol
   "A symbol, not unlike those in Lisp."
   (%Symbol Lisp-Object))
 
 (declare intern (String -> Symbol))
 (define (intern str)
   "Create a symbol with a name. The identity

    (== (intern x) (intern x))

is always True.
"
   (lisp Symbol (str)
     (%Symbol (cl:intern str (cl:find-package "COALTON-INTERNED-SYMBOLS")))))

 (declare unique-symbol (Unit -> Symbol))
 (define (unique-symbol _)
   "Produce a unique symbol. The identity

    (== (unique-symbol) (unique symbol))

is always False. More strongly, the identity

    (== x (unique-symbol))

for any run-time value x is always False."
   (lisp Symbol ()
     (%Symbol (cl:gensym))))

 (declare symbol-name (Symbol -> String))
 (define (symbol-name s)
   "Return the name of a symbol as a string."
   (match s
     ((%Symbol s)
      (lisp String (s)
        (cl:symbol-name s)))))

 (declare symbol-package (Symbol -> (Optional String)))
 (define (symbol-package s)
   "Return the package of a symbol as a string, or None if it's not interned."
   (match s
     ((%Symbol s)
      (lisp (Optional String) (s)
        (cl:let ((p (cl:symbol-package s)))
          (cl:if (cl:null p)
                 None
                 (Some 
                  (cl:package-name p))))))))

  (define-instance (Eq Symbol)
    (define (== s1 s2)
      (match (Tuple s1 s2)
        ((Tuple (%Symbol s1) (%Symbol s2))
         (lisp Boolean (s1 s2)
           (cl:eq s1 s2))))))

  ;; We don't define `into` because of the existence of packages.
  )
