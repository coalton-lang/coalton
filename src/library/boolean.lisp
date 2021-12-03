(in-package #:coalton-library)

;;;
;;; Boolean
;;;

;;; Boolean is defined in types.lisp

(coalton-toplevel
  ;;
  ;; Boolean instances
  ;;

  (define-instance (Show Boolean)
    (define (show x)
      (match x
        ((True) "True")
        ((False) "False"))))

  (define-instance (Eq Boolean)
    (define (== x y)
      (not (/= x y))))

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
