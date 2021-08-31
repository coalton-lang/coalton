(in-package #:coalton-user)

(coalton-toplevel

  ;;
  ;; Unit
  ;;

  (define Unit (lisp Unit () 'Unit))
  
  (define-type Boolean
    True
    False)

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
      ((False) y)))

  (define-type (List :a)
    (Cons :a (List :a))
    Nil)

  (define-type (Tuple :a :b)
    (Tuple :a :b))
  
  (define-type (Optional :a)
    (Some :a)
    None)

  (define (undefined x)
    (lisp :a ()  (cl:error "Undefined"))))
