(in-package #:coalton-library)

;;;
;;; Boolean
;;;

;;; Boolean is defined in types.lisp

(coalton-toplevel
  (declare not (Boolean -> Boolean))
  (define (not x)
    (match x
      ((True) False)
      ((False) True)))

  (declare or (Boolean -> Boolean -> Boolean))
  (define (or x y)
    (match x
      ((True) True)
      ((False) y)))

  (declare and (Boolean -> Boolean -> Boolean))
  (define (and x y)
    (match x
      ((True) y)
      ((False) False)))

  (declare xor (Boolean -> Boolean -> Boolean))
  (define (xor x y)
    (match x
      ((True) (not y))
      ((False) y))))

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
