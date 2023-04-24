;;;; fundep-tests.lisp

(in-package #:coalton-tests)

;; Fundep parsing
(deftest define-fundep-classes ()
  (check-coalton-types
   "(define-class (C :a :b (:a -> :b)))")

  (check-coalton-types
   "(define-class (C :a :b :c (:a :b -> :c)))")

  (check-coalton-types
   "(define-class (C :a :b :c (:a -> :b :c)))")

  (check-coalton-types
   "(define-class (C :a :b (:a -> :b) (:b -> :a)))")

  (signals tc:tc-error
    (check-coalton-types
     "(define-class (C :a :b (:a -> :c)))")))

;; Instance conflicts
(deftest define-fundep-instances ()
  (check-coalton-types
   "(define-class (C :a :b (:a -> :b)))

    (define-instance (C Integer String))

    (define-instance (C String Integer))")

  ;; Trivially conflicting instances
  (signals tc:tc-error
    (check-coalton-types
     "(define-class (C :a :b (:a -> :b)))

      (define-instance (C Integer String))

      (define-instance (C Integer Unit))"))

  ;;
  ;; Instances with unequal generality should conflict in either definition order
  ;;
  (signals tc:tc-error
    (check-coalton-types
     "(define-class (C :a :b (:a -> :b)))

      (define-instance (C (List :a) String))

      (define-instance (C (List Integer) Unit))"))

  (signals tc:tc-error
    (check-coalton-types
     "(define-class (C :a :b (:a -> :b)))

      (define-instance (C (List Integer) Unit))

      (define-instance (C (List :a) String))")))

;; Check that fundep declerations are used to improve type checking 
(deftest fundep-improve-types ()
  (check-coalton-types
   "(define-class (C :a :b (:a -> :b))
      (m (:a -> :b)))

    (define-instance (C String Integer)
      (define (m x) 5))

    (define (ambig _x) Unit)

    (define x (ambig (m \"hello\")))")

  ;; verify that the above example fails without the above fundep dec
  (signals tc:tc-error
    (check-coalton-types
     "(define-class (C :a :b)
        (m (:a -> :b)))

      (define-instance (C String Integer)
        (define (m x) 5))

      (define (ambig _x) Unit)

      (define x (ambig (m \"hello\")))"))

  ;; verify that the correct type is inferred
  (check-coalton-types
   "(define-class (C :a :b (:a -> :b))
      (m (:a -> :b)))

    (define-instance (C String Integer)
      (define (m x ) 5))

    (define x (m \"hello\"))"

   '("x" . "Integer")))

(deftest fundep-improvement-specifity ()
  ;; Check that (List String) -> :a matches the fundep (List :a) -> Integer
  (check-coalton-types
   "(define-class (C :a :b (:a -> :b))
      (m (:a -> :b)))

    (define-instance (C (List :a) Integer)
      (define (m x) 5))

    (define x (m (make-list \"hello\" \"hi\")))"

   '("x" . "Integer"))

  ;; Check that (List :a) -> :b does not match (List String) -> Integer
  (check-coalton-types
   "(define-class (C :a :b (:a -> :b))
     (m (:a -> :b)))

    (define-instance (C (List String) Integer)
      (define (m x) 5))

    (define (f x)
      (m (make-list x)))"

   '("f" . "(C (List :a) :b => :a -> :b)")))

(deftest fundep-ambiguous-methods ()
  ;; Unambiguous because of fundep
  (check-coalton-types
   "(define-class (C :a :b (:a -> :b))
      (m :a))")

  ;; Amgbigious despite fundep
  (signals tc:tc-error
    (check-coalton-types
     "(define-class (C :a :b (:a -> :b))
      (m :b))"))

  ;; Ambiguous without fundep
  (signals tc:tc-error
    (check-coalton-types
     "(define-class (C :a :b)
        (m :a))")))

(deftest fundep-ambiguous-declerations ()
  (check-coalton-types
   "(define-class (C :a :b (:a -> :b)))

    (declare f (C :a :b => :a -> Unit))
    (define (f _) Unit)")

  (check-coalton-types
   "(define-class (C :a :b (:a -> :b)))

    (declare f ((C :b :c) (C :a :b) => :a -> Unit))
    (define (f _) Unit)"))

(deftest fundep-implicit-explicit ()
  (check-coalton-types
   "(declare gh-792 (List Integer -> List Integer))
    (define (gh-792 items)
      (coalton-library/iterator:collect! (coalton-library/iterator:into-iter items)))")

  (check-coalton-types
   "(define (gh-792-impl items)
      (the (List coalton:Integer)
           (coalton-library/iterator:collect!
               (coalton-library/iterator:into-iter
               (the (List Integer) items)))))"))

(deftest fundep-explicit-binding ()
  (check-coalton-types
   "(declare fast-evens (coalton-library/iterator:IntoIterator :a coalton:Integer => :a -> coalton:List coalton:Integer))
    (define (fast-evens items)
      (pipe items
            coalton-library/iterator:into-iter
            (map (* 3))
            (coalton-library/iterator:filter! even?)
            coalton-library/iterator:collect!))"))

(deftest fundep-unambigous-method ()
  (check-coalton-types
   "(define-class (C :a :b :c (:a -> :b :c))
      (m (:a -> :b)))

     (define (f x)
       (m x))"))
