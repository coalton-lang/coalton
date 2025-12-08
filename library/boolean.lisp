(coalton-library/utils:defstdlib-package #:coalton-library/boolean
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-compatibility-layer)
  (:local-nicknames
   (#:compat #:coalton-compatibility-layer))
  (:import-from
   #:coalton-library/hash
   #:define-sxhash-hasher))

(in-package #:coalton-library/boolean)

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

(compat:try-lock-package "COALTON-LIBRARY/BOOLEAN")
