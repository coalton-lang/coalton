(coalton-library/utils:defstdlib-package #:coalton-library/boolean
  (:use
   #:coalton
   #:coalton-library/classes))

(cl:in-package #:coalton-library/boolean)

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
           ((False) EQ)))))))

(define-sxhash-hasher Boolean)

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/BOOLEAN")
