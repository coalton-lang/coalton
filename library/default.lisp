(coalton-library/utils:defstdlib-package #:coalton-library/default
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes)
  (:local-nicknames
   (#:cell #:coalton-library/cell)
   (#:iter #:coalton-library/iterator))
  (:export
   #:default))

(in-package #:coalton-library/default)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(cl:defmacro define-default-numbers (cl:&rest types)
  `(progn ,@(cl:loop :for type :in types
               :collect `(define-instance (Default ,type)
                           (define (default) 0)))))

(coalton-toplevel
  ;;
  ;; Default
  ;;

  (define-class (Default :t)
    (default (Unit -> :t)))

  ;;
  ;; Instances
  ;;

  (define-instance (Default String)
    (define (default) ""))

  (define-instance (Default (List :a))
    (define (default) Nil))

  (define-default-numbers
    I8 U8 I16 I32 I64 U16 U32 U64
    IFix UFix
    Integer Double-Float Single-Float Fraction)

  (define-instance (Default (Optional :a))
    (define (default) None)))
