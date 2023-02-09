(coalton-library/utils:defstdlib-package #:coalton-library/boolean
  (:use
   #:coalton
   #:coalton-library/classes)
  (:import-from
   #:coalton-library/hash
   #:define-sxhash-hasher))

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(in-package #:coalton-library/boolean)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;;
  ;; Boolean instances
  ;;

  (define-instance (Eq Boolean)
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

  (define-sxhash-hasher Boolean))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/BOOLEAN")
