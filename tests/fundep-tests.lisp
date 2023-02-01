;;;; fundep-tests.lisp

(in-package #:coalton-tests)

;; Fundep parsing
#+broken
(deftest define-fundep-classes ()
  (run-coalton-typechecker
   '((coalton:define-class (C :a :b (:a -> :b)))))

  (run-coalton-typechecker
   '((coalton:define-class (C :a :b :c (:a :b -> :c)))))

  (run-coalton-typechecker
   '((coalton:define-class (C :a :b :c (:a -> :b :c)))))

  (run-coalton-typechecker
   '((coalton:define-class (C :a :b (:a -> :b) (:b -> :a)))))

  (signals ast:coalton-parse-error
    (run-coalton-typechecker
     '((coalton:define-class (C :a :b (:a -> :c))))))

  (signals ast:coalton-parse-error
    (run-coalton-typechecker
     '((coalton:define-class (C :a :b (:a ->)))))))

;; Instance conflicts
#+broken
(deftest define-fundep-instances ()
  (run-coalton-typechecker
   '((coalton:define-class (C :a :b (:a -> :b)))

     (coalton:define-instance (C coalton:Integer coalton:String))

     (coalton:define-instance (C coalton:String coalton:Integer))))

  ;; Trivially conflicting instances
  (signals tc:coalton-type-error
    (run-coalton-typechecker
     '((coalton:define-class (C :a :b (:a -> :b)))

       (coalton:define-instance (C coalton:Integer coalton:String))

       (coalton:define-instance (C coalton:Integer coalton:Unit)))))

  ;;
  ;; Instances with unequal generality should conflict in either definition order
  ;;
  (signals tc:coalton-type-error
    (run-coalton-typechecker
     '((coalton:define-class (C :a :b (:a -> :b)))

       (coalton:define-instance (C (coalton:List :a) coalton:String))

       (coalton:define-instance (C (coalton:List coalton:Integer) coalton:Unit)))))

  (signals tc:coalton-type-error
    (run-coalton-typechecker
     '((coalton:define-class (C :a :b (:a -> :b)))

       (coalton:define-instance (C (coalton:List coalton:Integer) coalton:Unit))

       (coalton:define-instance (C (coalton:List :a) coalton:String))))))

;; Check that fundep declerations are used to improve type checking 
#+broken
(deftest fundep-improve-types ()
  (run-coalton-typechecker
   '((coalton:define-class (C :a :b (:a -> :b))
       (m (:a -> :b)))

     (coalton:define-instance (C String Integer)
       (coalton:define (m x) 5))

     (coalton:define (ambig x) Unit)

     (coalton:define x (ambig (m "hello")))))

  ;; verify that the above example fails without the above fundep dec
  (signals tc:coalton-type-error
    (run-coalton-typechecker
     '((coalton:define-class (C :a :b)
         (m (:a -> :b)))

       (coalton:define-instance (C String Integer)
         (coalton:define (m x) 5))

       (coalton:define (ambig x) Unit)

       (coalton:define x (ambig (m "hello"))))))

  ;; verify that the correct type is inferred
  (check-coalton-types
   '((coalton:define-class (C :a :b (:a -> :b))
       (m (:a -> :b)))

     (coalton:define-instance (C String Integer)
       (coalton:define (m x ) 5))

     (coalton:define x (m "hello")))

   '((x . Integer))))

#+broken
(deftest fundep-improvement-specifity ()
  ;; Check that (List String) -> :a matches the fundep (List :a) -> Integer
  (check-coalton-types
   '((coalton:define-class (C :a :b (:a -> :b))
       (m (:a -> :b)))

     (coalton:define-instance (C (coalton:List :a) Integer)
       (coalton:define (m x) 5))

     (coalton:define x (m (coalton:make-list "hello" "hi"))))

   '((x . coalton:Integer)))

  ;; Check that (List :a) -> :b does not match (List String) -> Integer
  (check-coalton-types
   '((coalton:define-class (C :a :b (:a -> :b))
      (m (:a -> :b)))

     (coalton:define-instance (C (coalton:List String) Integer)
       (coalton:define (m x) 5))

     (coalton:define (f x)
       (m (coalton:make-list x))))

   '((f . (C (coalton:List :a) :b => :a -> :b)))))

#+broken
(deftest fundep-ambigious-methods ()
  ;; Unambigious because of fundep
  (run-coalton-typechecker
   '((coalton:define-class (C :a :b (:a -> :b))
       (m :a))))

  ;; Amgbigious despite fundep
  (signals tc:coalton-type-error
    (run-coalton-typechecker
     '((coalton:define-class (C :a :b (:a -> :b))
         (m :b)))))

  ;; Ambigious without fundep
  (signals tc:coalton-type-error
    (run-coalton-typechecker
     '((coalton:define-class (C :a :b)
         (m :a))))))


#+broken
(deftest fundep-ambigious-declerations ()
  (run-coalton-typechecker
   '((coalton:define-class (C :a :b (:a -> :b)))

     (coalton:declare f (C :a :b => :a -> coalton:Unit))
     (coalton:define (f _) coalton:Unit)))

  (run-coalton-typechecker
   '((coalton:define-class (C :a :b (:a -> :b)))

     (coalton:declare f ((C :b :c) (C :a :b) => :a -> coalton:Unit))
     (coalton:define (f _) coalton:Unit))))

#+broken
(deftest fundep-implicit-explicit ()
  (run-coalton-typechecker
   '((coalton:declare gh-792 (coalton:List coalton:Integer -> coalton:List coalton:Integer))
     (coalton:define (gh-792 items)
       (coalton-library/iterator:collect! (coalton-library/iterator:into-iter items)))))

  (run-coalton-typechecker
   '((coalton:define (gh-792-impl items)
       (coalton:the (coalton:List coalton:Integer)
                    (coalton-library/iterator:collect!
                     (coalton-library/iterator:into-iter
                      (coalton:the (coalton:List coalton:Integer) items))))))))

#+broken
(deftest fundep-explicit-binding ()
  (run-coalton-typechecker
   '((coalton:declare fast-evens (coalton-library/iterator:IntoIterator :a coalton:Integer => :a -> coalton:List coalton:Integer))
     (coalton:define (fast-evens items)
       (coalton:pipe items
             coalton-library/iterator:into-iter
             (coalton-prelude:map (coalton-prelude:* 3))
             (coalton-library/iterator:filter! coalton-prelude:even?)
             coalton-library/iterator:collect!)))))
