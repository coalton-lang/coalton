(in-package #:coalton-library)

;;;
;;; Boolean
;;;

;;; Boolean is defined in types.lisp

(coalton-toplevel
  (declare boolean-not (Boolean -> Boolean))
  (define (boolean-not x)
    "Is X False?"
    (match x
      ((True) False)
      ((False) True)))

  (declare boolean-or (Boolean -> Boolean -> Boolean))
  (define (boolean-or x y)
    "Is X or Y True? Note that this is a *function* which means both X and Y will be evaluated. Use the OR macro for short-circuiting behavior."
    (match x
      ((True) True)
      ((False) y)))

  (declare boolean-and (Boolean -> Boolean -> Boolean))
  (define (boolean-and x y)
    "Are X and Y True? Note that this is a *function* which means both X and Y will be evaluated. Use the AND macro for short-circuiting behavior."
    (match x
      ((True) y)
      ((False) False)))

  (declare boolean-xor (Boolean -> Boolean -> Boolean))
  (define (boolean-xor x y)
    "Are X or Y True, but not both?"
    (match x
      ((True) (boolean-not y))
      ((False) y)))

  (define not
    "Synonym for BOOLEAN-NOT."
    boolean-not)

  (define xor
    "Synonym for BOOLEAN-XOR."
    boolean-xor))

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
