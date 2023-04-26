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

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:defmacro define-number-default (type)
    `(define-instance (Default ,type)
       (define (default) 0))))

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
    
  (define-number-default I8)
  (define-number-default U8)
  (define-number-default I16)
  (define-number-default I32)
  (define-number-default I64)
  (define-number-default U16)
  (define-number-default U32)
  (define-number-default U64)
  (define-number-default IFix)
  (define-number-default UFix)
  (define-number-default Integer)
  (define-number-default Double-Float)
  (define-number-default Single-Float)
  (define-number-default Fraction)

  (define-instance (Default (Optional :a))
    (define (default) None)))
