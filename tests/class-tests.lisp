(cl:in-package #:coalton-native-tests)

(coalton-toplevel
  (define-type Mock-Monoid
    "A monoid that tracks its structure"
    (XValue Integer)
    (XEmpty)
    (X<> Mock-Monoid Mock-Monoid))

  (define-instance (Semigroup Mock-Monoid)
    (define <> X<>))

  (define-instance (Monoid Mock-Monoid)
    (define mempty XEmpty))

  (define (structural-== a b)
    "Compare the structure of the monoid - does not follow the monoid laws"
    (match (Tuple a b)
      ((Tuple (XValue a)  (XValue b))  (== a b))
      ((Tuple (XEmpty)    (XEmpty))    True)
      ((Tuple (X<> a1 a2) (X<> b1 b2)) (and (structural-== a1 b1)
                                            (structural-== a2 b2)))
      (_                                   False)))

  (define (canonicalize v)
    "Canonicalize the mock monoid into a form which follows the monoid laws"
    (match v
      ((X<> a b)
       (let a = (canonicalize a))
       (let b = (canonicalize b))
       (match (X<> a b)
         ;; Monoid laws
         ((X<> (XEmpty) b) b)
         ((X<> b (XEmpty)) b)

         ;; Associativity
         ((X<> (X<> a b) c) (X<> a (X<> b c)))
         (v v)))
      (v v)))

  (define-instance (Eq Mock-Monoid)
    (define (== a b)
      (structural-== (canonicalize a) (canonicalize b)))))

(define-test test-folds ()
  (let fold  = coalton-library/classes:fold)
  (let foldr = coalton-library/classes:foldr)
  (let mconcat = coalton-library/classes:mconcat)

  (is (structural-== (fold X<> XEmpty Nil) XEmpty))
  (is (structural-==
       (fold X<> XEmpty (map XValue (make-list 1 2 3)))
       (X<> (X<> (X<> XEmpty (XValue 1)) (XValue 2)) (XValue 3))))
  (is (structural-== (foldr X<> XEmpty Nil) XEmpty))
  (is (structural-==
       (foldr X<> XEmpty (map XValue (make-list 1 2 3)))
       (X<> (XValue 1) (X<> (XValue 2) (X<> (XValue 3) XEmpty)))))
  (is (== (mconcat Nil) XEmpty))
  (is (==
       (mconcat (map XValue (make-list 1 2 3)))
       (X<> (XValue 1) (X<> (XValue 2) (XValue 3))))))
    
  
