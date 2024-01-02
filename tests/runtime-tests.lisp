(cl:in-package #:coalton-native-tests)

(coalton-toplevel
  (define (recursive-predicate x inc goal)
    (if (== x goal)
        goal
        (recursive-predicate (+ x inc) inc goal))))

;; Test that functions with typeclass constraints call themselves recursively
;; This was bugged in the past #216 #147 #125
(define-test test-recursive-predicate ()
  (is (== (recursive-predicate 0 1 5) 5)))

(coalton-toplevel
  (define (gh-295-f a)
    (let ((g (fn (x)
               (== x (make-list a)))))
      g)))

;; See gh #295
(define-test test-deferred-predicate-removal ()
  (progn
    (is (== (gh-295-f (the Integer 1) (make-list 2 3 4)) False))
    (is (== (gh-295-f (the Integer 1) (make-list 1)) True))))


;; Test that functions can be given explicit predicates in any order
;; See gh #377
(coalton-toplevel
  (declare gh-377-a ((Num :a) (Ord :a) => :a -> :a -> :a -> (List :a)))
  (define (gh-377-a step start end)
    (let ((inner
            (fn (current acc)
              (if (> current end)
                  acc
                  (inner (+ current step)
                         (Cons current acc))))))
      (inner start Nil)))

 (declare gh-377-b ((Ord :a) (Num :a) => :a -> :a -> :a -> (List :a)))
  (define (gh-377-b step start end)
    (let ((inner
            (fn (current acc)
              (if (> current end)
                  acc
                  (inner (+ current step)
                         (Cons current acc))))))
      (inner start Nil))))

(define-test test-explicit-predicate-order ()
  (progn
    (is (== (gh-377-a (the Integer 1) 0 5)
            (make-list 5 4 3 2 1 0)))
    (is (== (gh-377-b (the Integer 1) 0 5)
            (make-list 5 4 3 2 1 0)))))


;; Test that classes can have both methods and constants in either order
;; See gh #400
(coalton-toplevel
  (define-class (Gh-400-a :a)
    (gh-400-a-constant :a)
    (gh-400-a-method (:a -> :a)))

  (define-class (Gh-400-b :a)
    (gh-400-b-method (:a -> :a))
    (gh-400-b-constant :a))

  (define-instance (Gh-400-a Integer)
    (define gh-400-a-constant 5)
    (define (gh-400-a-method x) x))

  (define-instance (Gh-400-b Integer)
    (define (gh-400-b-method x) x)
    (define gh-400-b-constant 5)))

;; Test that classes can have method constraints
(coalton-toplevel
  (define-class (Gh-430 :a)
    (gh-430-m (Num :b => :a -> :b -> :b -> :b)))

  (define-instance (Gh-430 String)
    (define (gh-430-m a b c)
      (+ b c))))

(define-test test-method-constraints ()
  (is (== 5 (gh-430-m "str" 2 3)))
  (is (== 2 (gh-430-m "hello" 1 1))))

;; Test that unused instance predicates are compiled correctly
;; See gh #463

(coalton-toplevel
  (define-class (Gh-463 :a)
    (gh-463-m (:a -> :a -> :a)))

  (define-instance (Num :a => Gh-463 :a)
    (define (gh-463-m x y)
      x))

  (define (gh-463-f x)
    (gh-463-m x 2)))

(define-test test-unused-instance-constraint ()
  (is (== 3 (gh-463-f 3))))

;; Test defaulting and context reduction
(define-test test-defaulting ()
  (is (== (+ (Some (Some (the Integer 1))) (Some (Some 2))) (Some (Some 3)))))


;; Test that explicit type declarations in let bindings work
;; See gh #478
(define-test test-explicit-type-decs-in-let ()
  (let ((declare z (Num :a => :a -> :a))
        (z id))
    (z 5)))

;; Test multi param typeclass context reduction
(coalton-toplevel
  (declare context-reduction (Into :a Integer => :a -> Integer))
  (define (context-reduction x)
    (into x)))

;; Check that defaulting propagates through let bindings
;; See gh #526
(coalton-toplevel
  (define gh-526
    (let ((y (even? 2)))
      y)))

(define-test test-defaulting-propigation ()
    (is (== True gh-526)))

;; Check that defaulted monomorphized bindings work
(coalton-toplevel
  (define gh-654 +))

(define-test test-monomorphized-defaulting ()
  (is (== 2 (gh-654 1 1))))


;; Check that codegen for fundep classes works
(coalton-toplevel
  (define-class (Mult :a :b :c (:a :b -> :c))
    (_* (:a -> :b -> :c))))

(coalton-toplevel
  (define-class (Gh-968 :a)
    (gh-968-m (:a -> :a)))

  (define-class (Gh-968 :a => Gh-968-b :a))

  (declare gh-968-f (Gh-968-b :a => :a -> :a))
  (define (gh-968-f x)
    (gh-968-m x))

  (define-instance (gh-968 Unit)
    (define gh-968-m (fn (x) x)))

  (define-instance (gh-968-b Unit))

  (define gh-968-x (gh-968-f Unit)))

(coalton-toplevel
  (define-type (WrapperT :a)
    (Single :a)
    (Many (Vector (WrapperT :a))))

  (define-instance (Eq :a => Eq (WrapperT :a))
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Single x) (Single y))
         (== x y))

        ((Tuple (Many xs) (Many ys))
         (== xs ys))

        (_ False)))))

(define-test test-gh-973 ()
    (is (== (Many (vector:make (Single "hello")))
                     (Many (vector:make (Single "hello"))))))

;; Test accessors on transparent structs
(coalton-toplevel
  (repr :transparent)
  (define-struct (TransparentWrapper :a)
    (inner :a)))

(define-test test-transparent-wrapper ()
  (is (== (make-list "x")
          (map .inner (make-list (TransparentWrapper "x"))))))
