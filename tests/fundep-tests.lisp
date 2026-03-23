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

(deftest ty-scheme-alpha-equivalence ()
  (flet ((parse-scheme (string)
           (let ((source (source:make-source-string string)))
             (with-open-stream (stream (source:source-stream source))
               (tc:parse-ty-scheme
                (parser:parse-qualified-type
                 (parser:with-reader-context stream
                   (eclector.concrete-syntax-tree:read stream))
                 source)
                entry:*global-environment*)))))
    (is (tc:ty-scheme=
         (parse-scheme
          "((coalton/iterator:FromIterator :a (coalton/classes:Tuple :b :c))
            (coalton/iterator:IntoIterator :d :b)
            (coalton/iterator:IntoIterator :e :c)
            => :d * :e -> :a)")
         (parse-scheme
          "((coalton/iterator:IntoIterator :right-container :right-element)
            (coalton/iterator:IntoIterator :left-container :left-element)
            (coalton/iterator:FromIterator :result
             (coalton/classes:Tuple :left-element :right-element))
            => :left-container * :right-container -> :result)")))))

;;; Targeted tests for ty-scheme= predicate bag matching.
;;; The optimization commits to the first predicate match (no backtracking)
;;; when the tgen map is complete after comparing the main type.

(deftest ty-scheme=-complete-map-predicate-order ()
  "When all tgen ids appear in the main type, predicate order shouldn't matter.
This exercises the O(n²) fast path."
  (flet ((parse-scheme (string)
           (let ((source (source:make-source-string string)))
             (with-open-stream (stream (source:source-stream source))
               (tc:parse-ty-scheme
                (parser:parse-qualified-type
                 (parser:with-reader-context stream
                   (eclector.concrete-syntax-tree:read stream))
                 source)
                entry:*global-environment*)))))
    ;; Same predicates, different order, all vars used in main type
    (is (tc:ty-scheme=
         (parse-scheme "(coalton/classes:Eq :a => :a -> :a -> coalton:Boolean)")
         (parse-scheme "(coalton/classes:Eq :b => :b -> :b -> coalton:Boolean)")))
    ;; Two predicates, reversed order, vars in main type
    (is (tc:ty-scheme=
         (parse-scheme
          "((coalton/classes:Eq :a) (coalton/classes:Ord :b)
            => :a -> :b -> coalton:Boolean)")
         (parse-scheme
          "((coalton/classes:Ord :y) (coalton/classes:Eq :x)
            => :x -> :y -> coalton:Boolean)")))
    ;; Multiple predicates on different vars, all present in main type
    (is (tc:ty-scheme=
         (parse-scheme
          "((coalton/classes:Num :a) (coalton/classes:Num :b) (coalton/classes:Eq :a)
            => :a -> :b -> coalton:Boolean)")
         (parse-scheme
          "((coalton/classes:Eq :x) (coalton/classes:Num :y) (coalton/classes:Num :x)
            => :x -> :y -> coalton:Boolean)")))))

(deftest ty-scheme=-incomplete-map-backtracking ()
  "When tgen ids appear only in predicates (not in the main type), the map is
incomplete and backtracking is required.  The greedy first-match can pick the
wrong pairing, so the O(n!) backtracking path must find the correct one."
  ;; Construct schemes directly because the parser rejects ambiguous type
  ;; variables.  Both schemes represent:
  ;;   forall a b c. (C a b, C a c) => a -> Unit
  ;; where b and c appear only in predicates, leaving the map incomplete
  ;; after comparing the main type.
  (let* ((class-name (gensym "TEST-CLASS-"))
         (tgen0 (tc:make-tgen :id 0))    ; a — used in main type
         (tgen1 (tc:make-tgen :id 1))    ; b — predicate-only
         (tgen2 (tc:make-tgen :id 2))    ; c — predicate-only
         (pred-ab (tc:make-ty-predicate :class class-name :types (list tgen0 tgen1)))
         (pred-ac (tc:make-ty-predicate :class class-name :types (list tgen0 tgen2)))
         (main-ty (tc:make-function-ty :positional-input-types (list tgen0)
                                       :output-types (list tc:*unit-type*)))
         ;; Scheme 1: preds in order (ab, ac)
         (scheme1 (tc:make-ty-scheme
                   :kinds (list tc:+kstar+ tc:+kstar+ tc:+kstar+)
                   :type (tc:make-qualified-ty :predicates (list pred-ab pred-ac)
                                               :type main-ty)))
         ;; Scheme 2: preds reversed (ac, ab)
         (scheme2 (tc:make-ty-scheme
                   :kinds (list tc:+kstar+ tc:+kstar+ tc:+kstar+)
                   :type (tc:make-qualified-ty :predicates (list pred-ac pred-ab)
                                               :type main-ty))))
    (is (tc:ty-scheme= scheme1 scheme2))
    ;; Also verify: same scheme equals itself
    (is (tc:ty-scheme= scheme1 scheme1))))

(deftest ty-scheme=-incomplete-map-distinguishes-constraints ()
  "With an incomplete map, schemes that differ in predicate argument structure
must be distinguished even though the predicate-only variables are ambiguous."
  (let* ((class-name (gensym "TEST-CLASS-"))
         (tgen0 (tc:make-tgen :id 0))
         (tgen1 (tc:make-tgen :id 1))
         (tgen2 (tc:make-tgen :id 2))
         (main-ty (tc:make-function-ty :positional-input-types (list tgen0)
                                       :output-types (list tc:*unit-type*)))
         ;; Scheme 1: (C a b, C a c) => a -> Unit
         (scheme1 (tc:make-ty-scheme
                   :kinds (list tc:+kstar+ tc:+kstar+ tc:+kstar+)
                   :type (tc:make-qualified-ty
                          :predicates (list (tc:make-ty-predicate :class class-name
                                                                  :types (list tgen0 tgen1))
                                           (tc:make-ty-predicate :class class-name
                                                                  :types (list tgen0 tgen2)))
                          :type main-ty)))
         ;; Scheme 2: (C a b, C c a) => a -> Unit — second pred has args flipped
         (scheme2 (tc:make-ty-scheme
                   :kinds (list tc:+kstar+ tc:+kstar+ tc:+kstar+)
                   :type (tc:make-qualified-ty
                          :predicates (list (tc:make-ty-predicate :class class-name
                                                                  :types (list tgen0 tgen1))
                                           (tc:make-ty-predicate :class class-name
                                                                  :types (list tgen2 tgen0)))
                          :type main-ty))))
    ;; Different predicate argument structure — should NOT be equal
    (is (not (tc:ty-scheme= scheme1 scheme2)))))

(deftest ty-scheme=-nonequivalent ()
  "Schemes that should NOT be alpha-equivalent."
  (flet ((parse-scheme (string)
           (let ((source (source:make-source-string string)))
             (with-open-stream (stream (source:source-stream source))
               (tc:parse-ty-scheme
                (parser:parse-qualified-type
                 (parser:with-reader-context stream
                   (eclector.concrete-syntax-tree:read stream))
                 source)
                entry:*global-environment*)))))
    ;; Different predicates
    (is (not (tc:ty-scheme=
              (parse-scheme "(coalton/classes:Eq :a => :a -> coalton:Boolean)")
              (parse-scheme "(coalton/classes:Ord :a => :a -> coalton:Boolean)"))))
    ;; Same predicates but vars swapped in main type
    (is (not (tc:ty-scheme=
              (parse-scheme
               "((coalton/classes:Eq :a) (coalton/classes:Ord :b) => :a -> :b -> coalton:Boolean)")
              (parse-scheme
               "((coalton/classes:Eq :a) (coalton/classes:Ord :b) => :b -> :a -> coalton:Boolean)"))))
    ;; Different number of predicates
    (is (not (tc:ty-scheme=
              (parse-scheme
               "((coalton/classes:Eq :a) (coalton/classes:Ord :a) => :a -> coalton:Boolean)")
              (parse-scheme
               "(coalton/classes:Eq :a => :a -> coalton:Boolean)"))))))

(deftest fundep-explicit-binding ()
  (check-coalton-types
   "(declare fast-evens (coalton/iterator:IntoIterator :a coalton:Integer => :a -> coalton:List coalton:Integer))
    (define (fast-evens items)
      (pipe items
            coalton/iterator:into-iter
            (fn (iter) (map (fn (x) (* x 3)) iter))
            (fn (iter) (coalton/iterator:filter! even? iter))
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
      (moo-find (:a * :b -> (Optional :c))))

    (declare filled-moos ((Num :b) (Ord :b) (Moo :a :b :c) => :a -> Iterator :b))
    (define (filled-moos moo)
      (let ((filled?
              (fn (i) (coalton/optional:some? (moo-find moo i)))))
        (coalton/iterator:filter! filled? (coalton/iterator:up-to (moo-size moo)))))"))

(deftest fundep-inherited ()
  ;; see https://github.com/coalton-lang/coalton/issues/1050
  (check-coalton-types
   "(define-class (subcolable :a :b (:a -> :b))
      (subcol (:a * UFix * UFix -> :a)))
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

    (declare f (D :a :b => (Void -> :a)))
    (define (f) m)

    (declare g (D (List :a) (List :b) => (Void -> List :a)))
    (define (g) m)"
   '("f" . "(D :a :b => (Void -> :a))")
   '("g" . "(D (List :a) (List :b) => (Void -> List :a))"))

  ;; see https://github.com/coalton-lang/coalton/issues/1643
  (check-coalton-types
   "(declare myzip1 ((coalton/iterator:FromIterator :a (Tuple :b :c))
                     (coalton/iterator:IntoIterator :d :b)
                     (coalton/iterator:IntoIterator :e :c)
                     => :d * :e -> :a))
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
                  => :d * :e -> :a)")
   #+CCL
   '("myzip2" . "((coalton/iterator:IntoIterator :d :a)
                  (coalton/iterator:IntoIterator :c :b)
                  (coalton/iterator:FromIterator :e (Tuple :b :a))
                  => :c * :d -> :e)")))

(deftest fundep-nested-entail ()
  ;; see https://github.com/coalton-lang/coalton/issues/1717
  (check-coalton-types
   "(define-class (HasA :t :a (:t -> :a))
      (get-a (coalton/types:Proxy :t -> :a)))

    (define-class (Monad :m => MonadHasA :m :t (:m -> :t)))

    (declare get-has-a-prx (MonadHasA :m :t => coalton/types:Proxy (:m :a) -> coalton/types:Proxy :t))
    (define (get-has-a-prx _)
      coalton/types:Proxy)

    (declare print-a-str-pure ((HasA :t :a) (MonadHasA :m :t) => (Void -> :m Unit)))
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

    (declare print-a-str-pure (MonadHasA :m :t :a => (Void -> :m Unit)))
    (define (print-a-str-pure)
      (let m-prx = coalton/types:Proxy)
      (let msg = (get-a (get-has-a-prx m-prx)))
      (coalton/types:as-proxy-of (pure Unit)
                                         m-prx))")
  )

(deftest fundep-superclass-determined-var-regression ()
  ;; See https://github.com/coalton-lang/coalton/issues/1716
  (check-coalton-types
   "(define-class (HasA :t :a (:t -> :a))
      (get-a (coalton/types:Proxy :t -> :a)))

    (define-class (HasA :t :a => WrapsHasA :w :t (:w -> :t))
      (get-two-as (:w -> Tuple :a :a)))"))

(deftest fundep-superclass-determined-var-workaround-regression ()
  ;; See https://github.com/coalton-lang/coalton/issues/1716
  (check-coalton-types
   "(define-class (HasA :t :a (:t -> :a))
      (get-a (coalton/types:Proxy :t -> :a)))

    (define-class (HasA :t :a => WrapsHasA :w :t :a (:w -> :t))
      (get-two-as (:w -> Tuple :a :a)))"))

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
