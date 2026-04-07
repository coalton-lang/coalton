(in-package #:coalton-native-tests)

(coalton-toplevel
  (declare explicit-rec-eq
    (forall (:item)
      (coalton/classes:Eq :item => :item -> Boolean)))
  (define (explicit-rec-eq x)
    (if (== x x)
        True
        (explicit-rec-eq x)))

  (declare explicit-rec-eq-integer (Integer -> Boolean))
  (define (explicit-rec-eq-integer x)
    (explicit-rec-eq x))

  (repr :transparent)
  (define-type (ScopedDictBox :a)
    (ScopedDictBox :a))

  (define-class (ScopedDictClass :wrapper)
    (scoped-dict-method
      (forall (:item)
        (coalton/classes:Eq :item => (:wrapper :item) * (coalton/types:Proxy :item) -> Boolean))))

  (define-instance (ScopedDictClass ScopedDictBox)
    (define (scoped-dict-method wrapped proxy)
      (match wrapped
        ((ScopedDictBox inner)
         (let ((declare rebuild
                       (forall (:ignored)
                         (coalton/classes:Eq :item => (coalton/types:Proxy :ignored) * :item -> Boolean)))
               (rebuild (fn (_other value)
                          (== value inner))))
           (rebuild proxy inner))))))

  (declare scoped-dict-method-integer (Integer -> Boolean))
  (define (scoped-dict-method-integer x)
    (scoped-dict-method (ScopedDictBox x) (coalton/types:proxy-of x)))

  (define-type (SuperclassMapBox :a)
    (SuperclassMapBox :a))

  (define-type (SuperclassMapWrap :a)
    (SuperclassMapWrap :a))

  (define-instance (Functor SuperclassMapBox)
    (define (map f x)
      (match x
        ((SuperclassMapBox y) (SuperclassMapBox (f y))))))

  (define-instance (Functor SuperclassMapWrap)
    (define (map f x)
      (match x
        ((SuperclassMapWrap y) (SuperclassMapWrap (f y))))))

  (define-instance (Applicative SuperclassMapWrap)
    (define (pure x)
      (SuperclassMapWrap x))
    (define (lifta2 f x y)
      (match x
        ((SuperclassMapWrap xv)
         (match y
           ((SuperclassMapWrap yv)
            (SuperclassMapWrap (f xv yv))))))))

  (define-instance (Monad SuperclassMapWrap)
    (define (>>= x f)
      (match x
        ((SuperclassMapWrap y) (f y)))))

  (declare superclass-map-regression
    ((Functor :f) (Monad :m) => :f Unit * :m Integer -> :m Integer))
  (define (superclass-map-regression _dummy m)
    (map 1+ m))

  (declare superclass-map-regression-run
    (SuperclassMapBox Unit * SuperclassMapWrap Integer -> SuperclassMapWrap Integer))
  (define (superclass-map-regression-run dummy m)
    (superclass-map-regression dummy m)))

(define-test superclass-map-regression-runtime ()
  (matches (SuperclassMapWrap 2)
           (superclass-map-regression-run
            (SuperclassMapBox Unit)
            (SuperclassMapWrap 1))))

(in-package #:coalton-tests)

(deftest test-scoped-forall-dictionary-resolution ()
  (is (coalton:lookup-code 'coalton-native-tests::explicit-rec-eq-integer))
  (is (coalton:lookup-code 'coalton-native-tests::scoped-dict-method-integer)))
