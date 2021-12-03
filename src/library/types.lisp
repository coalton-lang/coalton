(in-package #:coalton-library)

(coalton-toplevel

  (define-type Unit Unit)

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
    boolean-xor)

  ;; Boolean is an early type
  (define True (lisp Boolean ()  cl:t))
  (define False (lisp Boolean ()  cl:nil))

  (define-type (List :a)
    "A list in singly-linked representation."
    (Cons :a (List :a))
    Nil)

  (define-type (Tuple :a :b)
    "A heterogeneous collection of items."
    (Tuple :a :b))

  (define-type (Tuple3 :a :b :c)
    (Tuple3 :a :b :c))

  (define-type (Tuple4 :a :b :c :d)
    (Tuple4 :a :b :c :d))

  (define-type (Tuple5 :a :b :c :d :e)
    (Tuple5 :a :b :c :d :e))

  (define-type (Optional :a)
    "Represents something that may not have a value."
    (Some :a)
    None)

  (define-type (Result :bad :good)
    "Represents something that may have failed."
    ;; We write (Result :bad :good) instead of (Result :good :bad)
    ;; because of the limitations of how we deal with higher-kinded
    ;; types; we want to implement Functor on this.
    (Ok :good)
    (Err :bad))

  (define-type Fraction
    "A ratio of integers always in reduced form."
    ;; We avoid "Rational" or "Ratio" since those might be a more
    ;; generic concept than a humble fraction of integers. This
    ;; fraction is always assumed to be in reduced terms.
    ;;
    ;; This shouldn't be pattern matched against with user code.
    ;;
    ;; See fraction.lisp for more info.
    (%Fraction Integer Integer)))
