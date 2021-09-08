(in-package #:coalton-library)

(coalton-toplevel
  ;;
  ;; Boolean
  ;;
  
  ;; Boolean is defined in types.lisp

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
      (not (/= x y)))
    (define (/= x y)
      (xor x y)))

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
