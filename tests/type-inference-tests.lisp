;;;; type-inference-tests.lisp

(in-package #:coalton-tests)

(deftest test-type-inference ()
  (check-coalton-types
   "(define f 5)"

   '("f" . "Integer"))

  (check-coalton-types
   "(define f (fn (x) x))
    (define g (f 5))"

   '("f" . "(:a -> :a)")
   '("g" . "Integer"))

  (check-coalton-types
   "(define f (fn (x _y) x))
    (define g (f 5 \"str\"))
    (define h (f \"str\" 5))"

   '("f" . "(:a * :b -> :a)")
   '("g" . "Integer")
   '("h" . "String"))

  ;; Check that identity qualifies
  (check-coalton-types
   "(define (id_ a) a)
    (define x (id_ 3))
    (define y (id_ \"three\"))"

   '("id_" . "(:a -> :a)")
   '("x" . "Integer")
   '("y" . "String"))

  ;; Check that let bindings are polymorphic over kinds
  (check-coalton-types
   "(define x
     (let ((id (fn (a) a)))
       ((id id) 5)))"

   '("x" . "Integer"))

  ;; Check that let bindings can have explicit types
  (check-coalton-types
   "(define (f x)
      (let ((declare g (Integer -> Integer))
            (g id))
       (g x)))"

   '("f" . "(Integer -> Integer)"))

  (check-coalton-types
   "(define (example f)
      (f))"

   '("example" . "((Void -> :a) -> :a)"))

  (check-coalton-types
   "(define (example2 f)
      (let (values a b) = (f))
      (+ a b))"

   '("example2" . "(Num :a => (Void -> :a * :a) -> :a)"))

  (check-coalton-types
   "(declare unit-identity (Unit -> Integer))
    (define (unit-identity _) 1)
    (define y (unit-identity Unit))"

   '("unit-identity" . "(Unit -> Integer)")
   '("y" . "Integer"))

  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(declare unit-identity (Unit -> Integer))
      (define (unit-identity _) 1)
      (define x (unit-identity))"))

  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define (add2 x y) (+ x y))
      (define partial (add2 1))"))

  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(declare impossible (Void -> Integer))
      (define (impossible _x) 1)"))

  ;; Void (zero values) cannot be bound to a variable
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define (f)
        (let b = (values))
        b)"))

  ;; Result of a Void function cannot be bound to a variable
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define (g) (values))
      (define (f)
        (let b = (g))
        b)"))

  ;; Result of when (which is Void) cannot be bound to a variable
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define (f b)
        (let r = (when b 1))
        r)"))

  ;; Multiple values cannot be bound to a single variable
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define (f)
        (let b = (values 1 2))
        b)"))

  ;; Result of a multi-value function cannot be bound to a single variable
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(declare two-vals (Void -> Integer * Integer))
      (define (two-vals)
        (values 1 2))
      (define (f)
        (let b = (two-vals))
        b)"))

  (check-coalton-types
   "(define (when-zero-values b)
      (when b
        1))

    (define (unless-zero-values b)
      (unless b
        1))

    (define (bare-return)
      (return))"

   '("when-zero-values" . "(Boolean -> Void)")
   '("unless-zero-values" . "(Boolean -> Void)")
   '("bare-return" . "(Void -> Void)"))

  (check-coalton-types
   "(define-exception CatchResultError
      (CatchPayload Integer))

    (define (catch-pair x)
      (catch (values x (1+ x))
        ((CatchPayload y) (values y (1+ y)))
        (_ (values 0 0))))

    (define (catch-pair-bind x)
      (let (values a b) = (catch (throw (CatchPayload x))
                            ((CatchPayload y) (values y (1+ y)))
                            (_ (values 0 0))))
      (+ a b))

    (define (catch-void b)
      (catch (when b 1)
        ((CatchPayload _) (values))
        (_ (values))))"

   '("catch-pair" . "(Integer -> Integer * Integer)")
   '("catch-pair-bind" . "(Integer -> Integer)")
   '("catch-void" . "(Boolean -> Void)"))

  (check-coalton-types
   "(define-exception TestException
      (TestException Integer))

    (declare foo (Void -> Void))
    (define (foo)
      (values))

    (declare bar (Void -> Void))
    (define (bar)
      (catch (foo)
        ((TestException _) (values)))
      (values))"

   '("foo" . "(Void -> Void)")
   '("bar" . "(Void -> Void)"))

  (check-coalton-types
   "(define (count-down n)
      (rec loop ((i n))
        (unless (== i 0)
          (loop (- i 1)))))"

   '("count-down" . "(Num :a => :a -> Void)"))

  (check-coalton-types
   "(define (count-to n)
      (for ((declare i UFix)
            (i 0 (1+ i)))
        :returns i
        :while (< i n)
        Unit))

    (define (repeat-from-binding)
      (for ((x 10 x))
        :repeat x
        (show \"hi\")))

    (define (for-init-binding-scope)
      (for ((declare a UFix)
            (declare b UFix)
            (a b a)
            (b 1 (+ b 1)))
        :returns b
        :while (< b 10)
        Unit))

    (define (sum-to n)
      (for ((declare i UFix)
            (declare acc UFix)
            (i 0 (1+ i))
            (acc 0 (+ acc i)))
        :returns acc
        :repeat n
        Unit))"

   '("count-to" . "(UFix -> UFix)")
   '("repeat-from-binding" . "(Void -> Void)")
   '("for-init-binding-scope" . "(Void -> UFix)")
   '("sum-to" . "(UFix -> UFix)")))

(deftest test-keyword-function-types ()
  (check-coalton-types
   "(declare declared-keyword-only (&key (:x Integer) -> Integer))
    (define (declared-keyword-only &key (x 1))
      x)

    (declare declared-empty-keyword (&key -> Integer))
    (define (declared-empty-keyword &key)
      5)

    (define (keyword-add x &key (y 1))
      (+ x y))

    (define (keyword-only &key (x 1))
      x)

    (define (empty-keyword x &key)
      x)

   (define (optional-default &key (tag None))
      tag)"

   '("declared-keyword-only" . "(&key (:x Integer) -> Integer)")
   '("declared-empty-keyword" . "(Void -> Integer)")
   '("keyword-add" . "(Num :a => :a &key (:y :a) -> :a)")
   '("keyword-only" . "(Num :a => &key (:x :a) -> :a)")
   '("empty-keyword" . "(:a -> :a)")
   '("optional-default" . "(&key (:tag (Optional :a)) -> Optional :a)")))

(deftest test-keyword-parser-rejections ()
  (signals coalton-impl/parser:parse-error
    (check-coalton-types
     "(define broken
        (fn (x &key timeout)
          x))"))

  (signals coalton-impl/parser:parse-error
    (check-coalton-types
     "(define broken
        (fn (x)
          (f x :y)))"))

  (signals coalton-impl/parser:parse-error
    (check-coalton-types
     "(define broken
        (fn (x)
          (f x :y 1 2)))"))

  (signals coalton-impl/parser:parse-error
    (check-coalton-types
     "(define broken
        (fn (x)
          (f x :y 1 :y 2)))")))

(deftest test-keyword-higher-order-inference ()
  (check-coalton-types
   "(define (run f x)
      (f x))

    (define (run0 f)
      (f))

    (define (run-timeout f x)
      (f x :timeout 5))

    (define (plain x)
      x)

    (define (keywordy x &key (timeout 0) (extra 10))
      (+ (+ x timeout) extra))

    (define (keywordy0 &key (timeout 0) (extra 10))
      (+ timeout extra))

    (define a (run plain 7))
    (define b (run keywordy 7))
    (define c (run-timeout keywordy 7))
    (define d (run0 keywordy0))"

   '("plain" . "(:a -> :a)")
   '("keywordy" . "(Num :a => :a &key (:extra :a) (:timeout :a) -> :a)")
   '("a" . "Integer")
   '("b" . "Integer")
   '("c" . "Integer")
   '("d" . "Integer")))

(deftest test-keyword-only-higher-order-inference ()
  (let* ((toplevel-string
           "(define (a f)
              (f :x \"hi\"))

            (define (g &key (x \"x\") (_y \"y\"))
              x)

            (define test (a g))")
         (*package* (make-package "COALTON-KEYWORD-ONLY-HOF-PACKAGE"
                                  :use '("COALTON" "COALTON-PRELUDE"))))
    (unwind-protect
         (let ((source (source:make-source-string toplevel-string)))
           (with-open-stream (stream (source:source-stream source))
             (let ((program (parser:with-reader-context stream
                              (parser:read-program stream source))))
               (multiple-value-bind (program env)
                   (entry:entry-point program)
                 (declare (ignore program))
                 (flet ((lookup (name)
                          (tc:lookup-value-type env (intern (string-upcase name) *package*)))
                        (parse-scheme (string)
                          (let ((source (source:make-source-string string)))
                            (with-open-stream (stream (source:source-stream source))
                              (tc:parse-ty-scheme
                               (parser:parse-qualified-type
                                (parser:with-reader-context stream
                                  (eclector.concrete-syntax-tree:read stream))
                                source)
                               env)))))
                   (is (tc:ty-scheme=
                        (lookup "g")
                        (parse-scheme "(&key (:x String) (:_y String) -> String)")))
                   (is (tc:ty-scheme=
                        (lookup "test")
                        (parse-scheme "String")))
                   (let* ((a-qual-type (tc:fresh-inst (lookup "a")))
                          (a-type (tc:qualified-ty-type a-qual-type))
                          (f-type (first (tc:function-ty-positional-input-types a-type)))
                          (f-keyword (first (tc:function-ty-keyword-input-types f-type)))
                          (a-output (first (tc:function-ty-output-types a-type)))
                          (f-output (first (tc:function-ty-output-types f-type))))
                     (is (typep a-type 'tc:function-ty))
                     (is (= 1 (length (tc:function-ty-positional-input-types a-type))))
                     (is (null (tc:function-ty-keyword-input-types a-type)))
                     (is (not (tc:function-ty-keyword-open-p a-type)))
                     (is (typep f-type 'tc:function-ty))
                     (is (null (tc:function-ty-positional-input-types f-type)))
                     (is (= 1 (length (tc:function-ty-keyword-input-types f-type))))
                     (is (tc:function-ty-keyword-open-p f-type))
                     (is (eq :x (tc:keyword-ty-entry-keyword f-keyword)))
                     (is (tc:ty= tc:*string-type* (tc:keyword-ty-entry-type f-keyword)))
                     (is (tc:ty= a-output f-output))))))))
      (delete-package *package*))))

(deftest test-keyword-typechecker-rejections ()
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define (f x &key (y 1))
        (+ x y))

      (define bad
        (f 1 :unknown 2))"))

  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(declare expect-timeout
        ((Integer &key (:timeout Integer) -> Integer) -> Integer))

      (define (expect-timeout f)
        (f 1 :timeout 2))

      (define (plain x)
        x)

      (define bad
        (expect-timeout plain))")))

(deftest test-recursive-type-inference ()
  ;; Check mutually recursive definitions
  (check-coalton-types
   "(define (f a) (g a))
    (define (g b) (f b))"

   '("f" . "(:a -> :b)")
   '("g" . "(:a -> :b)"))

  ;; Check unusual recursive definitions
  (check-coalton-types
   "(define f
       (fn (a) (f a)))"

   '("f" . "(:a -> :b)"))

  (check-coalton-types
   "(define f
       (fn (_a) (f 5)))"

   '("f" . "(Num :a => :a -> :b)")))

(deftest test-explicit-type-declarations ()
  ;; Check that explicit declarations can reduce the type of a definition
  (check-coalton-types
   "(declare f Integer)
    (define f (undefined \"hello\"))"

   '("f" . "Integer"))

  ;; Implicitly typed functions should only infer types from the declared type signature of an explicitly typed functions
  ;; http://jeremymikkola.com/posts/2019_01_12_type_inference_for_haskell_part_12.html
  (check-coalton-types
   "(declare lst (List Integer))
    (define lst (undefined \"a list\"))

    (define (a x) (singleton (b x)))

    (declare b (:a -> :a))
    (define (b y)
      (let ((_foo (c 5)))
        y))

    (define (c z) (append lst (a z)))"

   '("a" . "(:a -> (List :a))")))

(deftest test-explicit-forall-declarations ()
  (check-coalton-types
   "(declare choose (forall (:result :input) (:input * :result -> :input)))
    (define (choose x _y) x)"

   '("choose" . "(forall (:result :input) (:input * :result -> :input))"))

  (check-coalton-types
   "(declare keep-first (forall (:outer) (forall (:inner) (:outer * :inner -> :outer))))
    (define (keep-first x _y) x)"

   '("keep-first" . "(forall (:outer :inner) (:outer * :inner -> :outer))"))

  (check-coalton-types
   "(declare choose-short (forall (:result :input) :input * :result -> :input))
    (define (choose-short x _y) x)"

   '("choose-short" . "(forall (:result :input) (:input * :result -> :input))"))

  (check-coalton-types
   "(declare keep-first-short (forall (:outer) (forall (:inner) :outer * :inner -> :outer)))
    (define (keep-first-short x _y) x)"

   '("keep-first-short" . "(forall (:outer :inner) (:outer * :inner -> :outer))"))

  (check-coalton-types
   "(define (f x)
      (let ((declare local-id (forall (:item) (:item -> :item)))
            (local-id (fn (y) y)))
        (local-id x)))"

   '("f" . "(:a -> :a)"))

  (check-coalton-types
   "(declare scoped-local-implicit (forall (:item) :item -> :item))
    (define (scoped-local-implicit x)
      (let ((declare keep-item (Unit -> :item))
            (keep-item (fn (_unit) (the :item x))))
        (keep-item Unit)))"

   '("scoped-local-implicit" . "(forall (:item) (:item -> :item))"))

  (check-coalton-types
   "(declare scoped-local-explicit (forall (:outer) :outer -> :outer))
    (define (scoped-local-explicit x)
      (let ((declare keep-outer (forall (:inner) :outer * :inner -> :outer))
            (keep-outer (fn (y _z) (lisp (-> :outer) (y) y))))
        (keep-outer x Unit)))"

   '("scoped-local-explicit" . "(forall (:outer) (:outer -> :outer))"))

  (check-coalton-types
   "(define the-scoped-forall
      (the (forall (:item) (:item -> :item))
        (fn (x)
          (the :item x))))"

   '("the-scoped-forall" . "(forall (:item) (:item -> :item))"))

  (check-coalton-types
   "(declare scoped-proxy-roundtrip (forall (:item) :item -> :item))
    (define (scoped-proxy-roundtrip x)
      (let ((declare reify (coalton/types:Proxy :item -> :item))
            (reify (fn (p)
                     (coalton/types:as-proxy-of
                      (the :item x)
                      p))))
        (reify (coalton/types:proxy-of x))))"

   '("scoped-proxy-roundtrip" . "(forall (:item) (:item -> :item))"))

  (check-coalton-types
   "(declare scoped-proxy-inner
      (forall (:item)
        coalton/types:Proxy (Optional :item) -> coalton/types:Proxy :item))
    (define (scoped-proxy-inner maybe-proxy)
      (let ((declare drop-outer
                    (forall (:wrapper)
                      coalton/types:Proxy (:wrapper :item)
                      -> coalton/types:Proxy :item))
            (drop-outer (fn (p)
                          (coalton/types:proxy-inner p))))
        (drop-outer maybe-proxy)))"

   '("scoped-proxy-inner" . "(forall (:item) (coalton/types:Proxy (Optional :item) -> coalton/types:Proxy :item))"))

  (check-coalton-types
   "(define-type (ScopedMethodWrap :f :a)
      (ScopedMethodWrap (:f :a)))

    (define-class (ScopedMethodClass :wrapper)
      (scoped-method
        (forall (:item)
          ((:wrapper :item) * (coalton/types:Proxy :item) -> (:wrapper :item)))))

    (define-instance (ScopedMethodClass (ScopedMethodWrap :f))
      (define (scoped-method wrapped proxy)
        (match wrapped
          ((ScopedMethodWrap inner)
           (let ((declare rebuild
                         (forall (:ignored)
                           (coalton/types:Proxy :ignored) * (:f :item)
                           -> (ScopedMethodWrap :f :item)))
                 (rebuild (fn (_other value)
                            (ScopedMethodWrap (the (:f :item) value)))))
             (rebuild proxy inner))))))

    (declare use-scoped-method
      (forall (:f :item)
        ((ScopedMethodWrap :f :item) * (coalton/types:Proxy :item) -> (ScopedMethodWrap :f :item))))
    (define (use-scoped-method wrapped proxy)
      (scoped-method wrapped proxy))"

   '("use-scoped-method" . "(forall (:f :item) ((ScopedMethodWrap :f :item) * (coalton/types:Proxy :item) -> (ScopedMethodWrap :f :item)))"))

  (check-coalton-types
   "(define-type (ScopedMethodWrap :f :a)
      (ScopedMethodWrap (:f :a)))

    (define-class (ScopedLispMethodClass :wrapper)
      (scoped-lisp-method
        (forall (:item)
          ((:wrapper :item) * (coalton/types:Proxy :item) -> (:wrapper :item)))))

    (define-instance (ScopedLispMethodClass (ScopedMethodWrap :f))
      (define (scoped-lisp-method wrapped _proxy)
        (lisp (-> (ScopedMethodWrap :f :item)) (wrapped)
          wrapped)))

    (declare use-scoped-lisp-method
      (forall (:f :item)
        ((ScopedMethodWrap :f :item) * (coalton/types:Proxy :item) -> (ScopedMethodWrap :f :item))))
    (define (use-scoped-lisp-method wrapped proxy)
      (scoped-lisp-method wrapped proxy))"

   '("use-scoped-lisp-method" . "(forall (:f :item) ((ScopedMethodWrap :f :item) * (coalton/types:Proxy :item) -> (ScopedMethodWrap :f :item)))"))

  (check-coalton-types
   "(define-type (ScopedConstraintBox :a)
      (ScopedConstraintBox :a))

    (define-class (ScopedConstraintMethodClass :wrapper)
      (scoped-constrained-method
        (forall (:item)
          (Eq :item => (:wrapper :item) * (coalton/types:Proxy :item) -> (:wrapper :item)))))

    (define-instance (ScopedConstraintMethodClass ScopedConstraintBox)
      (define (scoped-constrained-method wrapped proxy)
        (match wrapped
          ((ScopedConstraintBox inner)
           (let ((declare rebuild
                         (forall (:ignored)
                           (Eq :item => (coalton/types:Proxy :ignored)
                                       * :item
                                       -> Boolean)))
                 (rebuild (fn (_other value)
                            (== value inner))))
             (if (rebuild proxy inner)
                 wrapped
                 (ScopedConstraintBox inner)))))))

    (declare use-scoped-constrained-method
      (forall (:item)
        (Eq :item => (ScopedConstraintBox :item)
                     * (coalton/types:Proxy :item)
                     -> (ScopedConstraintBox :item))))
    (define (use-scoped-constrained-method wrapped proxy)
      (scoped-constrained-method wrapped proxy))"

   '("use-scoped-constrained-method" . "(forall (:item) (Eq :item => (ScopedConstraintBox :item) * (coalton/types:Proxy :item) -> (ScopedConstraintBox :item)))"))

  (check-coalton-types
   "(define-type (ScopedMethodWrap :f :a)
      (ScopedMethodWrap (:f :a)))

    (define-class (ShadowedMethodClass :wrapper)
      (shadowed-method
        (forall (:item)
          ((:wrapper :item) -> (coalton/types:Proxy Unit)))))

    (define-instance (ShadowedMethodClass (ScopedMethodWrap :f))
      (define (shadowed-method wrapped)
        (match wrapped
          ((ScopedMethodWrap _inner)
           (let ((declare make-shadowed
                         (forall (:item)
                           :item -> (coalton/types:Proxy :item)))
                 (make-shadowed (fn (value)
                                  (coalton/types:proxy-of value))))
             (make-shadowed Unit))))))

    (declare use-shadowed-method
      (forall (:f :item)
        ((ScopedMethodWrap :f :item) -> (coalton/types:Proxy Unit))))
    (define (use-shadowed-method wrapped)
      (shadowed-method wrapped))"

   '("use-shadowed-method" . "(forall (:f :item) ((ScopedMethodWrap :f :item) -> (coalton/types:Proxy Unit)))")))

(deftest test-type-definitions ()
  ;; Test recursive type definitions
  (check-coalton-types
   "(define-type (Tree_ :a)
      (Leaf :a)
      (Branch (Tree_ :a) (Tree_ :a)))

    (define (f a)
      (Branch a (f a)))"

   '("Leaf" . "(:a -> Tree_ :a)")
   '("Branch" . "(Tree_ :a * Tree_ :a -> Tree_ :a)")
   '("f" . "(Tree_ :a -> Tree_ :a)"))

  ;; Check mutually recursive type definitions
  (check-coalton-types
   "(define-type (A :a)
      (A (B :a)))

    (define-type (B :a)
      (B (A :a)))")

  ;; Check higher kinded type variables
  (check-coalton-types
   "(define-type (TFix :f)
      (InType (:f (TFix :f))))"

   '("InType" . "(:f (TFix :f) -> TFix :f)")))

(deftest test-monomorphism-restriction ()
  ;; Check that functions defined as a lambda are not subject to the
  ;; monomorphism restriction
  (check-coalton-types
   "(define-class (Disp :a))

    (declare disp (Disp :a => :a -> String))
    (define (disp _x) \"not impl\")

    (define (f a)
      (disp a))

    (define g
      (fn (b)
        (disp b)))"

   '("f" . "(Disp :a => :a -> String)")
   '("g" . "(Disp :a => :a -> String)")))

(deftest test-type-classes ()
  ;; Check that type constraints are propagated
  (check-coalton-types
   "(define (f a b) (== a b))"

   '("f" . "(Eq :a => :a * :a -> Boolean)"))


  (check-coalton-types
   "(define-class (Eq_ :a)
      (==_ (:a * :a -> Boolean)))

    (define-type Color Red Green Blue)

    (define-instance (Eq_ Color)
      (define (==_ a b) False))

    (define a (==_ Red Green))

    (define (g x y)
      (==_ x (singleton y)))

    (define (h x y)
      (==_ (singleton x) (singleton y)))"

   '("a" . "Boolean")
   '("g" . "(Eq_ (List :a) => (List :a) * :a -> Boolean)")))

(deftest test-typeclass-polymorphic-recursion ()
  ;; Check that polymorphic recursion is possible
  (check-coalton-types
   "(declare f (Eq :a => :a * :a -> Boolean))
    (define (f a b)
      (if (== a b)
        True
        (f (singleton a) (singleton b))))"

   '("f" . "(Eq :a => :a * :a -> Boolean)")))

(deftest test-typeclass-definition-constraints ()

  ;; Check that typeclass methods can constrain other variables
  (check-coalton-types
   "(define-class (Test :a)
      (test (Eq :b => :a -> :b)))"))

(deftest test-typeclass-additional-constraints ()

  ;; Check that typeclass methods can provide additional constraints
  (check-coalton-types
   "(define-class (Test :a)
      (test-bare (:a -> :a))
      (test-addl (Eq :a => :a -> :a)))

    ;; Can define on types without Eq
    (define-type TestType A B)

    (define-instance (Test TestType)
      (define test-bare id)
      (define test-addl id))

    ;; Can define generic function that uses Eq if constrained
    (declare test-generic ((Eq :a) (Test :a) => :a -> :a))
    (define test-generic test-addl)

    ;; Can use test-addl on TestType if Eq defined
    (define-instance (Eq TestType)
      (define (== x y)
        (match (Tuple x y)
          ((Tuple (A) (A)) True)
          ((Tuple (B) (B)) True)
          (_ False))))

    (declare test-addl-testtype (TestType -> TestType))
    (define test-addl-testtype test-addl)")

  ;; Check that methods not requiring additional constraints can be used
  (check-coalton-types
   "(define-class (Test :a)
      (test-bare (:a -> :a))
      (test-addl (Eq :a => :a -> :a)))

    ;; Can define on types without Eq
    (define-type TestType A B)

    (define-instance (Test TestType)
      (define test-bare id)
      (define test-addl id))

    (declare test-bare-testtype (TestType -> TestType))
    (define test-bare-testtype test-bare)")

  (check-coalton-types
   "(declare lifted (Integer -> Integer))
    (define lifted
      (map (fn (x) (+ x 1))
           (fn (x) x)))"

   '("lifted" . "(Integer -> Integer)"))

  ;; Check that it works with functional dependencies
  (check-coalton-types
   "(define-type (Box :a)
      (Box :a))

    (define-class (ClassA :m :a (:m -> :a))
      (get-a (:m -> :a)))

    (define-class (ClassB :m :a (:m -> :a))
      (convert (ClassA :n :a => :n -> :m)))

    (define-instance (ClassA (Box :a) :a)
      (define (get-a (Box a)) a))

    (define-instance (ClassB (Box :a) :a)
      (define (convert bx)
        (Box (get-a bx))))"))

(deftest test-typeclass-flexible-instances ()
  (check-coalton-types
   "(define-class (Eq_ :a)
      (==? (:a * :a -> Boolean)))

     (define-instance (Eq_ :a => Eq_ (Tuple :a Integer))
       (define (==? a b) False))

     (define-instance (Eq_ Integer)
       (define (==? a b) False))

     (declare f
      (Tuple Integer Integer *
       Tuple Integer Integer ->
       Boolean))
     (define (f a b)
       (==? a b))")

  ;; https://github.com/coalton-lang/coalton/issues/1918
  (check-coalton-types
   "(define-type (Issue1918Box :x :y :a)
      (Issue1918Box% :a (:y -> :x)))

    (define-instance (Functor (Issue1918Box :x :y))
      (define (map f (Issue1918Box% a y->x))
        (Issue1918Box% (f a) y->x)))

    (define-instance (Applicative (Issue1918Box :r :r))
      (define (pure a)
        (Issue1918Box% a id))

      (define (lifta2 f (Issue1918Box% a _) (Issue1918Box% b _))
        (Issue1918Box% (f a b) id)))

    (declare issue-1918-project ((:a -> :y) * Issue1918Box :x :y :a -> :x))
    (define (issue-1918-project a->y (Issue1918Box% a y->x))
      (y->x (a->y a)))

    (define issue-1918-value
      (issue-1918-project id (pure \"hi\")))"

   '("issue-1918-value" . "String"))

  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(repr :transparent)
      (define-type (IssuePtState :s1 :s2 :a)
        (IssuePtSt% (:s1 -> :s2 * :a)))

      (declare issue-run-state
        (:s1 * IssuePtState :s1 :s2 :a -> :s2 * :a))
      (define (issue-run-state st (IssuePtSt% f-state))
        (f-state st))

      (define-instance (Functor (IssuePtState :s1 :s2))
        (define (map f st)
          (IssuePtSt%
            (fn (s)
              (let (values s2 a) = (issue-run-state s st))
              (values s2 (f a))))))

      (define-instance (Applicative (IssuePtState :s1 :s2))
        (define (pure a)
          (IssuePtSt%
            (fn (s)
              (values s a))))

        (define (lifta2 f st-a st-b)
          (IssuePtSt%
            (fn (s1)
              (let (values s2 a) = (issue-run-state s1 st-a))
              (let (values s3 b) = (issue-run-state s2 st-b))
              (values s3 (f a b))))))")))

(deftest test-typeclass-cyclic-superclass-checks ()
  (check-coalton-types
   "(define-class (TestClassA :a)
     (example-method (TestClassA :b => :a -> :b)))"))

(deftest test-the ()
  (check-coalton-types
   "(define (f a b)
      ((the (Integer * Integer -> Boolean) ==) a b))"

   '("f" . "(Integer * Integer -> Boolean)"))

  (check-coalton-types
   "(define x (the U32 (+ 1 2)))"

   '("x" . "U32")))

(deftest test-regression ()
  ;; Fixed in #283
  (check-coalton-types
   "(define (f a)
      (let ((g (fn (x) (Tuple x a))))
        (g 5)))"

   '("f" . "(Num :b => :a -> Tuple :b :a)")))

(deftest test-weak-type-variables ()
  ;; See gh #84
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define f
        (let ((x (coalton/vector:new)))
          (fn (n)
            (coalton/vector:push! n x)
            x)))"))

  ;; Allocating in a function body remains polymorphic because each call
  ;; gets a fresh allocation site.
  (check-coalton-types
   "(define (mk _x)
      (let ((x (coalton/vector:new)))
        (fn (n)
          (coalton/vector:push! n x)
          x)))"

   '("mk" . "(:a -> :b -> Vector :b)")))

(deftest test-weak-type-variables-advanced ()
  ;; Relaxed value restriction: expansive expressions can still generalize
  ;; weak variables that occur only covariantly.
  (check-coalton-types
   "(define wrapped-id
      ((fn (x) x) None))"

   '("wrapped-id" . "(Optional :a)"))

  (check-coalton-types
   "(define-type (Cov :a)
      (Cov :a))
    (define cov-app
      ((fn (x) x) (Cov None)))"

   '("cov-app" . "(Cov (Optional :a))"))

  ;; Contravariant weak occurrences are still blocked.
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define-type (Contra :a)
        (Contra (:a -> Unit)))
      (define contra-app
        ((fn (x) x) (Contra (fn (_x) Unit))))"))

  ;; Opaque mutable containers are treated as invariant.
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define boxed-vec
        ((fn (x) x) (coalton/vector:new)))"))

  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define boxed-cell
        ((fn (x) x) (coalton/cell:new None)))"))

  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define boxed-queue
        ((fn (x) x) (coalton/queue:new)))"))

  ;; User-defined opaque native types default to invariant.
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(repr :native cl:t)
      (define-type (Opaque :a))
      (declare opaque-wrap (:a -> Opaque :a))
      (define (opaque-wrap x)
        (lisp (-> (Opaque :a)) (x) x))
      (define opaque-top
        ((fn (x) x) (opaque-wrap None)))"))

  ;; Constructor wrappers are only non-expansive when their arguments are.
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define wrapped-new
        (Some (coalton/vector:new)))"))

  (check-coalton-types
   "(define wrapped-none
      (Some None))"

   '("wrapped-none" . "(Optional (Optional :a))"))

  ;; Capturing a top-level mutable cell in an implicit binding is expansive.
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define stash
        (let ((c (coalton/cell:new None)))
          (fn (x)
            (coalton/cell:write! c (Some x))
            (coalton/cell:read c))))"))

  ;; Explicit type declarations still allow expansive bindings to be monomorphic.
  (check-coalton-types
   "(declare int-vec (Vector Integer))
    (define int-vec (coalton/vector:new))

    (define (push-int n)
      (coalton/vector:push! n int-vec))

    (define peek-int
      (fn ()
        (coalton/vector:index 0 int-vec)))"

   '("push-int" . "(Integer -> UFix)")
   '("peek-int" . "(Void -> Optional Integer)"))

  (check-coalton-types
   "(declare int-queue (coalton/queue:Queue Integer))
    (define int-queue (coalton/queue:new))

    (define (push-int-queue n)
      (coalton/queue:push! n int-queue))

    (define pop-int-queue
      (fn ()
        (coalton/queue:pop! int-queue)))"

   '("push-int-queue" . "(Integer -> Void)")
   '("pop-int-queue" . "(Void -> Optional Integer)"))

  ;; A single inferred queue binding must not be usable at multiple element
  ;; types; Queue remains invariant for relaxed value restriction.
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define shared-queue (coalton/queue:new))

      (define (push-shared-int)
        (coalton/queue:push! 1 shared-queue))

      (define (push-shared-string)
        (coalton/queue:push! \"oops\" shared-queue))"))

  ;; Mutable allocations inside function bodies remain polymorphic per call.
  (check-coalton-types
   "(define (mk-stash _seed)
      (let ((c (coalton/cell:new None)))
        (fn (x)
          (coalton/cell:write! c (Some x))
          (coalton/cell:read c))))"

   '("mk-stash" . "(:a -> :b -> Optional :b)")))

(deftest test-dynamic-variable-types ()
  (check-coalton-types
   "(declare *id* (:a -> :a))
    (define *id* (fn (x) x))

    (define use-id
      (dynamic-bind ((*id* (fn (x) x)))
        (Tuple (*id* 1) (*id* True))))"

   '("use-id" . "(Tuple Integer Boolean)"))

  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(declare *id* (:a -> :a))
      (define *id* (fn (x) x))

      (define bad-id
        (dynamic-bind ((*id* (the (String -> String) (fn (x) x))))
          (*id* \"hello\")))"))

  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(declare *id* (:a -> :a))
      (define *id* (fn (x) x))

      (define bad-id
        (dynamic-bind ((*id* ((fn (f) f) (fn (x) x))))
          (Tuple (*id* 1) (*id* True))))")))

(deftest test-function-definition-shorthand ()
  (check-coalton-types
   "(define f (fn () 5))"
   '("f" . "(Num :a => (Void -> :a))")))

(deftest test-collection-builder-defaults ()
  (check-coalton-types
   "(define seq-default [1 2 3])
    (define assoc-default [1 => 2 3 => 4])"
   '("seq-default" . "(coalton/seq:Seq Integer)")
   '("assoc-default" . "(coalton/seq:Seq (Tuple Integer Integer))")))

(deftest test-empty-association-builder ()
  (check-coalton-types
   "(define empty-assoc
      (the (coalton/seq:Seq (Tuple Integer Integer))
           [=>]))"
   '("empty-assoc" . "(coalton/seq:Seq (Tuple Integer Integer))")))

(deftest test-explicit-builder-instances ()
  (check-coalton-types
   "(define vector-builder
      (the (coalton/vector:Vector Integer) [1 2 3]))
    (define lisparray-builder
      (the (coalton/lisparray:LispArray Integer) [1 2 3]))
    (define lisparray-comprehension-builder
      (the (coalton/lisparray:LispArray Integer)
           [x :for x :in (coalton/iterator:up-to 3)]))
    (define seq-below-builder
      (the (coalton/seq:Seq F32)
           [x :for x :below 3.0]))
    (define queue-builder
      (the (coalton/queue:Queue Integer) [1 2 3]))
    (define ordmap-builder
      (the (coalton/ordmap:OrdMap Integer Integer)
           [1 => 2 3 => 4]))
    (define hashtable-builder
      (the (coalton/hashtable:Hashtable Integer Integer)
           [1 => 2 3 => 4]))"
   '("vector-builder" . "(coalton/vector:Vector Integer)")
   '("lisparray-builder" . "(coalton/lisparray:LispArray Integer)")
   '("lisparray-comprehension-builder" . "(coalton/lisparray:LispArray Integer)")
   '("seq-below-builder" . "(coalton/seq:Seq F32)")
   '("queue-builder" . "(coalton/queue:Queue Integer)")
   '("ordmap-builder" . "(coalton/ordmap:OrdMap Integer Integer)")
   '("hashtable-builder" . "(coalton/hashtable:Hashtable Integer Integer)")))

(deftest test-collection-builder-function-defaults ()
  (check-coalton-types
   "(define (mk-seq-default)
      [True False])
    (define (mk-assoc-default)
      [True => False False => True])
   (define (mk-seq-comprehension-default)
      [x :for x :in (coalton/iterator:once True)])
    (define (mk-seq-comprehension-underscore)
      [False :for _ :in (coalton/iterator:once True)])
    (define (mk-seq-below-default)
      [x :for x :below 3])
    (define (mk-assoc-comprehension-default)
      [x => x :for x :in (coalton/iterator:once True)])
    (define (mk-assoc-comprehension-underscore)
      [False => True :for _ :in (coalton/iterator:once True)])
    (define (mk-assoc-below-default)
      [x => x :for x :below 3])"
   '("mk-seq-default" . "(Void -> coalton/seq:Seq Boolean)")
   '("mk-assoc-default" . "(Void -> coalton/seq:Seq (Tuple Boolean Boolean))")
   '("mk-seq-comprehension-default" . "(Void -> coalton/seq:Seq Boolean)")
   '("mk-seq-comprehension-underscore" . "(Void -> coalton/seq:Seq Boolean)")
   '("mk-seq-below-default" . "((coalton/types:RuntimeRepr :num) (Num :num) (Ord :num) => (Void -> coalton/seq:Seq :num))")
   '("mk-assoc-comprehension-default" . "(Void -> coalton/seq:Seq (Tuple Boolean Boolean))")
   '("mk-assoc-comprehension-underscore" . "(Void -> coalton/seq:Seq (Tuple Boolean Boolean))")
   '("mk-assoc-below-default" . "((Num :num) (Ord :num) => (Void -> coalton/seq:Seq (Tuple :num :num)))")))

(deftest test-function-implicit-progn ()
  (check-coalton-types
   "(define (f a)
      (let _a = (+ a 1))
      a)"

   '("f" . "(Num :a => :a -> :a)")))

(deftest test-returns ()
  (check-coalton-types
   "(define (f a)
      (return \"hello\")
      a)"

   '("f" . "(String -> String)")))

(deftest test-defaulting ()
  ;; See gh #505
  (check-coalton-types
   "(declare a (Num :a => :a -> :a))
    (define (a x)
      (let ((y 2))
        (+ x y)))"

   '("a" . "(Num :a => :a -> :a)"))

  ;; See gh #505
  (check-coalton-types
   "(declare f (:a -> :a))
    (define (f x)
      2
      x)"

   '("f" . "(:a -> :a)"))

  ;; Check that bindings aren't defaulted too early
  (check-coalton-types
   "(define (f _x)
      (let ((y 1))
        (+ 0.5 y)))"

   '("f" . "(:a -> F32)"))

  ;; Check that superclasses of Num are defaulted
  (check-coalton-types
   "(define x (even? 2))"

   '("x" . "Boolean")))

(deftest test-nameless-overapplication ()
  ;; See gh #1208
  (check-coalton-types
   "(define f (fn (x) (fn (y) (+ x y))))"

   '("f" . "(Num :a => :a -> :a -> :a)")))
