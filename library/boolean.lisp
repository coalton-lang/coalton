(coalton/utils:defstdlib-package #:coalton/boolean
  (:use
   #:coalton
   #:coalton/classes)
  (:import-from
   #:coalton/hash
   #:define-sxhash-hasher))

(in-package #:coalton/boolean)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;;
  ;; Boolean instances
  ;;

  (define-instance (Eq Boolean)
    (inline)
    (define (== x y)
      (lisp Boolean (x y)
        (cl:eq x y))))

  (define-instance (Ord Boolean)
    (define (<=> x y)
      (match x
        ((True)
         (match y
           ((True) EQ)
           ((False) GT)))
        ((False)
         (match y
           ((True) LT)
           ((False) EQ))))))

  (define-instance (Default Boolean)
    (inline)
    (define (default) False))

  (define-sxhash-hasher Boolean))

#+sb-package-locks
(sb-ext:lock-package "COALTON/BOOLEAN")
