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
   "(define-class (C :a :b (:a -> :b) (:b -> :a)))"))

;; Instance conflicts
(deftest define-fundep-instances ()
  (check-coalton-types
   "(define-class (C :a :b (:a -> :b)))

    (define-instance (C Integer String))

    (define-instance (C String Integer))"))

;; Check that fundep declarations are used to improve type checking 
(deftest fundep-improve-types ()
  (check-coalton-types
   "(define-class (C :a :b (:a -> :b))
      (m (:a -> :b)))

    (define-instance (C String Integer)
      (define (m x) 5))

    (define (ambig _x) Unit)

    (define x (ambig (m \"hello\")))")

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
      (m :a))"))

(deftest fundep-ambiguous-declarations ()
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
      (coalton/iterator:collect! (coalton/iterator:into-iter items)))")

  (check-coalton-types
   "(define (gh-792-impl items)
      (the (List coalton:Integer)
           (coalton/iterator:collect!
               (coalton/iterator:into-iter
               (the (List Integer) items)))))"))

(deftest fundep-explicit-binding ()
  (check-coalton-types
   "(declare fast-evens (coalton/iterator:IntoIterator :a coalton:Integer => :a -> coalton:List coalton:Integer))
    (define (fast-evens items)
      (pipe items
            coalton/iterator:into-iter
            (map (* 3))
            (coalton/iterator:filter! even?)
            coalton/iterator:collect!))"))

(deftest fundep-unambigous-method ()
  (check-coalton-types
   "(define-class (C :a :b :c (:a -> :b :c))
      (m (:a -> :b)))

    (define (f x)
      (m x))

    (declare g (C :a :b :c => :a -> :b))
    (define (g x)
      (m x))

    (declare h (C :a :b :c => :a -> :b))
    (define (h x)
      (m x)
      (m x))

    (define-class (C :a :b :c => D :a :b :c)
      (n :a))"))

(deftest fundep-unambigous-local-bindings ()
  ;; See https://github.com/coalton-lang/coalton/issues/913
  (check-coalton-types
   "(define-class (Moo :a :b :c (:a -> :b :c))
      (moo-size (:a -> :b))
      (moo-find (:a -> :b -> (Optional :c))))

    (declare filled-moos ((Num :b) (Ord :b) (Moo :a :b :c) => :a -> Iterator :b))
    (define (filled-moos moo)
      (let ((filled?
              (fn (i) (coalton/optional:some? (moo-find moo i)))))
        (coalton/iterator:filter! filled? (coalton/iterator:up-to (moo-size moo)))))"))

(deftest fundep-inherited ()
  ;; see https://github.com/coalton-lang/coalton/issues/1050
  (check-coalton-types
   "(define-class (subcolable :a :b (:a -> :b))
      (subcol (:a -> UFix -> UFix -> :a)))
    (define-class (subcolable :a :b => sizable :a :b)
      (size (:a -> UFix)))

    (define (f1 xs)
      (subcol xs 0 (coalton/math:1- (size xs))))

    (declare f2 (sizable :a :b => :a -> :a))
    (define (f2 xs)
      (subcol xs 0 (coalton/math:1- (size xs))))"
   '("f1" . "(sizable :a :b => :a -> :a)")))

(deftest fundep-entail ()
  (check-coalton-types
   "(define-class (C :a :b (:a -> :b)))
    (define-class (C :a :b => D :a :b)
      (m :a))

    (declare f (D :a :b => Unit -> :a))
    (define (f) m)

    (declare g (D (List :a) (List :b) => Unit -> List :a))
    (define (g) m)"
   '("f" . "(D :a :b => Unit -> :a)")
   '("g" . "(D (List :a) (List :b) => Unit -> List :a)"))

  ;; see https://github.com/coalton-lang/coalton/issues/1643
  (check-coalton-types
   "(declare myzip1 ((coalton/iterator:FromIterator :a (Tuple :b :c))
                     (coalton/iterator:IntoIterator :d :b)
                     (coalton/iterator:IntoIterator :e :c)
                     => :d -> :e -> :a))
    (define (myzip1 a b)
      (coalton/iterator:collect!
       (coalton/iterator:zip!
        (coalton/iterator:into-iter a)
        (coalton/iterator:into-iter b))))

    (define (myzip2 a b)
      (coalton/iterator:collect!
       (coalton/iterator:zip!
        (coalton/iterator:into-iter a)
        (coalton/iterator:into-iter b))))"
   #+SBCL
   '("myzip2" . "((coalton/iterator:FromIterator :a (Tuple :b :c))
                  (coalton/iterator:IntoIterator :d :b)
                  (coalton/iterator:IntoIterator :e :c)
                  => :d -> :e -> :a)")
   #+CCL
   '("myzip2" . "((coalton/iterator:IntoIterator :d :a)
                  (coalton/iterator:IntoIterator :c :b)
                  (coalton/iterator:FromIterator :e (Tuple :b :a))
                  => :c -> :d -> :e)")))

(deftest fundep-nested-entail ()
  ;; see https://github.com/coalton-lang/coalton/issues/1717
  (check-coalton-types
   "(define-class (HasA :t :a (:t -> :a))
      (get-a (coalton/types:Proxy :t -> :a)))

    (define-class (Monad :m => MonadHasA :m :t (:m -> :t)))

    (declare get-has-a-prx (MonadHasA :m :t => coalton/types:Proxy (:m :a) -> coalton/types:Proxy :t))
    (define (get-has-a-prx _)
      coalton/types:Proxy)

    (declare print-a-str-pure ((HasA :t :a) (MonadHasA :m :t) => Unit -> :m Unit))
    (define (print-a-str-pure)
      (let m-prx = coalton/types:Proxy)
      (let msg = (get-a (get-has-a-prx m-prx)))
      (coalton/types:as-proxy-of (pure Unit)
                                         m-prx))")

  (check-coalton-types
   "(define-class (HasA :t :a (:t -> :a))
      (get-a (coalton/types:Proxy :t -> :a)))

    (define-class ((HasA :t :a) (Monad :m) => MonadHasA :m :t :a (:m -> :t)))

    (declare get-has-a-prx (MonadHasA :m :t :a => coalton/types:Proxy (:m :b) -> coalton/types:Proxy :t))
    (define (get-has-a-prx _)
      coalton/types:Proxy)

    (declare print-a-str-pure (MonadHasA :m :t :a => Unit -> :m Unit))
    (define (print-a-str-pure)
      (let m-prx = coalton/types:Proxy)
      (let msg = (get-a (get-has-a-prx m-prx)))
      (coalton/types:as-proxy-of (pure Unit)
                                         m-prx))")
  )

(deftest fundep-catch-inferred-dict-regression ()
  ;; See https://github.com/coalton-lang/coalton/issues/1719
  (is (null
       (collect-compiler-error
        "(package coalton-unit-test/fundep-catch)
         (define-class (MyClass :a :b (:a -> :b))
           (to-zero (:a -> :b))
           (to-zero-prx (coalton/types:Proxy :a -> :b)))

         (define-instance (MyClass String Integer)
           (define (to-zero _) 0)
           (define (to-zero-prx _) 0))

         (declare breaks (MyClass :a :t => coalton/types:Proxy :a -> String))
         (define (breaks x)
           (catch (progn
                    (to-zero-prx x)
                    \"Runs\")
             (_ \"Fails\")))"))))

(deftest fundep-fundep-identity ()
  ;; See https://github.com/coalton-lang/coalton/issues/1736
  (check-coalton-types
   "(define-class ((Monad :r) (Monad :m) => MonadChain :r :m (:r -> :m)))

    (declare bar (MonadChain :r :m => :r String))
    (define bar
      (pure \"bar\"))

    (declare foo (MonadChain :h :h => :h String))
    (define foo
      bar)"))

(deftest fundep-nested-fundep-instances ()
  ;; See https://github.com/coalton-lang/coalton/pull/1743
  (check-coalton-types
   "
  (define-class (FundepClass :a :b (:a -> :b))
    (use-a (:a -> Unit)))

  (define-type (BaseType :a :b)
    (BaseType :a))

  (declare unwrap% (BaseType :a :b -> :a))
  (define (unwrap% (BaseType val))
    val)

  (define-instance (FundepClass :a :b => FundepClass (BaseType :a :b) (List :b))
    (inline)
    (define (use-a grouped)
      (use-a (unwrap% grouped))))

  (define-struct (WrapperType :a)
    (inner-base-type (BaseType :a Unit)))

  (define-instance (FundepClass (BaseType :a Unit) (List Unit)
                    => FundepClass (WrapperType :a) Unit)
    (inline)
    (define (use-a wrapper)
      (use-a (.inner-base-type wrapper))))"))
