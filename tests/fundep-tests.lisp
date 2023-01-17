;;;; fundep-tests.lisp

(in-package #:coalton-tests)

;; Fundep parsing
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

;; Ambigious variables
(deftest fundep-ambigious-variables ()
  (signals tc:coalton-type-error
    (run-coalton-typechecker
     '((coalton:define-class (C :a :b (:a -> :b)))

       (coalton:define-instance (C Integer :a))))

    ;; Transitive ambigious variable
    (signals tc:coalton-type-error
      (run-coalton-typechecker
       '((coalton:define-class (C :a :b :c (:a -> :b) (:b -> :c)))

         (coalton:define-instance (C coalton:Integer :a (coalton:List :a))))))

    ;; Ambigious variable in the transitive closure of multiple variables
    (signals tc:coalton-type-error
      (run-coalton-typechecker
       '((coalton:define-class (C :a :b :c (:a :b -> :c)))

         (coalton:define-instance (C Integer String :a)))))))

;; Check that fundep declerations are used to improve type checking 
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


(deftest fundep-ambigious-declerations ()
  (run-coalton-typechecker
   '((coalton:define-class (C :a :b (:a -> :b)))

     (coalton:declare f (C :a :b => :a -> coalton:Unit))
     (coalton:define (f _) coalton:Unit)))

  (run-coalton-typechecker
   '((coalton:define-class (C :a :b (:a -> :b)))

     (coalton:declare f ((C :b :c) (C :a :b) => :a -> coalton:Unit))
     (coalton:define (f _) coalton:Unit))))

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
