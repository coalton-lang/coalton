(in-package #:coalton-library)

;;;
;;; Boolean
;;;

;;; Boolean is defined in types.lisp

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
