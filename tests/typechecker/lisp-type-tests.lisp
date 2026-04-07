(in-package #:coalton-native-tests)

(coalton-toplevel
  (repr :transparent)
  (define-type (TransparentTypeTest :t)
    (TransparentTypeTest (coalton/lisparray:LispArray :t)))

  (declare complex-type-fn ((Complex :t) -> (Complex :t)))
  (define (complex-type-fn x) x)

  (declare complex-type-fn-1 ((Complex Double-Float) -> (Complex Double-Float)))
  (define (complex-type-fn-1 x) x)

  (declare lisp-array-fn ((coalton/lisparray:LispArray :t) -> :t))
  (define (lisp-array-fn x)
    (coalton/lisparray:aref x 0))

  (declare lisp-array-fn-1 ((coalton/lisparray:LispArray IFix) -> IFix))
  (define (lisp-array-fn-1 x)
    (coalton/lisparray:aref x 0))

  (declare transparent-type-fn ((TransparentTypeTest :t) -> :t))
  (define (transparent-type-fn (TransparentTypeTest x))
    (coalton/lisparray:aref x 0))

  (declare transparent-type-fn-1 ((TransparentTypeTest IFix) -> IFix))
  (define (transparent-type-fn-1 (TransparentTypeTest x))
    (coalton/lisparray:aref x 0))

  (define-type (LispArrayMultiTyvar :a :b))
  (declare lisp-array-multi-tyvar-fn ((coalton/lisparray:LispArray (LispArrayMultiTyvar :a :b)) -> (LispArrayMultiTyvar :a :b)))
  (define (lisp-array-multi-tyvar-fn x)
    (coalton/lisparray:aref x 0))

  (declare named-vars-fn ((Tuple :left :right) -> (Tuple :left :right)))
  (define (named-vars-fn pair)
    pair)

  (declare ordered-forall-fn (forall (:result :input) :input * :result -> :input))
  (define (ordered-forall-fn x _y)
    x)

  (declare nested-forall-fn (forall (:outer) (forall (:inner) :outer * :inner -> :outer)))
  (define (nested-forall-fn x _y)
    x)

  (define-class (NameTrackedClass :monad :state)
    (name-tracked-get (:monad :state)))

  (define-class (ExplicitMethodClass :wrapper)
    (explicit-method
      (forall (:item :result)
        ((:wrapper :item) -> (coalton/types:Proxy :item) -> (:item -> :result) -> :result))))

  (declare pretty-keyword-fn (Integer &key (:timeout Integer) (:extra Integer) -> Integer))
  (define (pretty-keyword-fn x &key (timeout 0) (extra 10))
    (+ x (+ timeout extra)))
  )

(in-package #:coalton-tests)

(defun parse-lisp-type-test-scheme (string)
  (let ((*package* (make-package "COALTON-LISP-TYPE-TEST-PACKAGE"
                                 :use '("COALTON" "COALTON-PRELUDE"))))
    (unwind-protect
         (let ((source (source:make-source-string string)))
           (with-open-stream (stream (source:source-stream source))
             (coalton-impl/typechecker:parse-ty-scheme
              (coalton-impl/parser:parse-qualified-type
               (coalton-impl/parser:with-reader-context stream
                 (eclector.concrete-syntax-tree:read stream))
               source)
              coalton-impl/entry:*global-environment*)))
      (delete-package *package*))))

(deftest test-lisp-types ()
  (let ((env coalton-impl/entry:*global-environment*))
    (labels ((coalton-type (name)
               (coalton-impl/typechecker/environment:type-entry-type
                (coalton-impl/typechecker/environment:lookup-type env name)))
             (coalton-type-of (value-name)
               (coalton-impl/typechecker/environment:lookup-value-type env value-name))
             (parsed-type (string)
               (coalton-impl/typechecker:qualified-ty-type
                (coalton-impl/typechecker:ty-scheme-type
                 (parse-lisp-type-test-scheme string))))
             (render-type (coalton-type)
               (let ((coalton-impl/settings:*coalton-print-unicode* nil))
                 (coalton-impl/typechecker:type-to-string coalton-type env)))
             (coalton-type-of-arg1 (value-name)
               (car (coalton-impl/typechecker/types:function-type-arguments
                     (coalton-type-of value-name))))
             (lisp-type (coalton-type)
               (coalton-impl/typechecker/lisp-type:lisp-type coalton-type env)))

      ;; Concrete type mappings
      (is (equal 'single-float (lisp-type (coalton-type 'coalton:F32))))
      (is (equal 'double-float (lisp-type (coalton-type 'coalton:F64))))

      ;; A few special cases
      (is (equal '(or number coalton/math/complex:complex)
                 (lisp-type (coalton-type-of-arg1 'coalton-native-tests::complex-type-fn))))
      (is (equal '(complex double-float)
                 (lisp-type (coalton-type-of-arg1 'coalton-native-tests::complex-type-fn-1))))
      (is (equal '(simple-array * (*))
                 (lisp-type (coalton-type-of-arg1 'coalton-native-tests::lisp-array-fn))))
      (is (equal '(simple-array fixnum (*))
                 (lisp-type (coalton-type-of-arg1 'coalton-native-tests::lisp-array-fn-1))))

      ;; Transparent type
      (is (equal 'coalton-impl/runtime/function-entry:function-entry
                 (lisp-type (coalton-type-of 'coalton-native-tests::transparent-type-fn))))
      (is (equal '(simple-array * (*))
                 (lisp-type (coalton-type-of-arg1 'coalton-native-tests::transparent-type-fn))))
      (is (equal '(simple-array fixnum (*))
                 (lisp-type (coalton-type-of-arg1 'coalton-native-tests::transparent-type-fn-1))))

      ;; LispArray with multi-tyvar element type
      (is (equal '(simple-array coalton-native-tests::LispArrayMultiTyvar (*))
                 (lisp-type (coalton-type-of-arg1 'coalton-native-tests::lisp-array-multi-tyvar-fn))))

      ;; Preserved type-variable names in printed schemes
      (check-string= "named function type variables"
                     "forall :LEFT :RIGHT. Tuple :LEFT :RIGHT -> Tuple :LEFT :RIGHT"
                     (render-type (coalton-type-of 'coalton-native-tests::named-vars-fn)))
      (check-string= "explicit forall binder order"
                     "forall :RESULT :INPUT. :INPUT * :RESULT -> :INPUT"
                     (render-type (coalton-type-of 'coalton-native-tests::ordered-forall-fn)))
      (check-string= "nested forall binder order"
                     "forall :OUTER :INNER. :OUTER * :INNER -> :OUTER"
                     (render-type (coalton-type-of 'coalton-native-tests::nested-forall-fn)))
      (check-string= "nested applications avoid outer parentheses"
                     "List (Seq Integer) -> Tuple Boolean String"
                     (render-type
                      (parsed-type
                       "(List (coalton/seq:Seq Integer) -> Tuple Boolean String)")))
      (check-string= "documentation generator preserves explicit forall order"
                     "&forall; :RESULT :INPUT. :INPUT * :RESULT &rarr; :INPUT"
                     (coalton/doc/markdown::to-markdown
                      (coalton-type-of 'coalton-native-tests::ordered-forall-fn)))
      (check-string= "documentation generator preserves nested forall order"
                     "&forall; :OUTER :INNER. :OUTER * :INNER &rarr; :OUTER"
                     (coalton/doc/markdown::to-markdown
                      (coalton-type-of 'coalton-native-tests::nested-forall-fn)))
      (check-string= "class method type variables"
                     "forall :MONAD :STATE. NAMETRACKEDCLASS :MONAD :STATE => :MONAD :STATE"
                     (render-type (coalton-type-of 'coalton-native-tests::name-tracked-get)))
      (check-string= "explicit class method forall order"
                     "forall :WRAPPER :ITEM :RESULT. EXPLICITMETHODCLASS :WRAPPER => :WRAPPER :ITEM -> Proxy :ITEM -> (:ITEM -> :RESULT) -> :RESULT"
                     (render-type (coalton-type-of 'coalton-native-tests::explicit-method)))
      (check-string= "keyword arguments retain source syntax"
                     "Integer &key (:extra Integer) (:timeout Integer) -> Integer"
                     (render-type (coalton-type-of 'coalton-native-tests::pretty-keyword-fn)))
      )))

(deftest test-documentation-anchors-include-package-name ()
  (let* ((env coalton-impl/entry:*global-environment*)
         (hashmap-count
           (coalton/doc/model::make-coalton-value
            (coalton-impl/typechecker/environment:lookup-name env 'coalton/hashmap:count)))
         (hashtable-count
           (coalton/doc/model::make-coalton-value
            (coalton-impl/typechecker/environment:lookup-name env 'coalton/hashtable:count)))
         (explicit-method-markdown
           (coalton/doc/markdown::to-markdown
            (coalton-impl/typechecker/environment:lookup-value-type
             env
             'coalton-native-tests::explicit-method))))
    (check-string= "hashmap count anchor"
                   "coalton-hashmap-count-value"
                   (coalton/doc/model:object-aname hashmap-count))
    (check-string= "hashtable count anchor"
                   "coalton-hashtable-count-value"
                   (coalton/doc/model:object-aname hashtable-count))
    (is (not (string= (coalton/doc/model:object-aname hashmap-count)
                      (coalton/doc/model:object-aname hashtable-count))))
    (is (search "href=\"#coalton-types-proxy-type\"" explicit-method-markdown))
    (is (search "href=\"#coalton-native-tests-explicitmethodclass-class\""
                explicit-method-markdown))))

(deftest test-tyvar-source-names-do-not-affect-quantification ()
  (let* ((left (coalton-impl/typechecker:make-tyvar
                :id 0
                :kind coalton-impl/typechecker:+kstar+
                :source-name :left))
         (right (coalton-impl/typechecker:make-tyvar
                 :id 0
                 :kind coalton-impl/typechecker:+kstar+
                 :source-name :right))
         (scheme (coalton-impl/typechecker:quantify
                  (list left)
                  (coalton-impl/typechecker:qualify nil right))))
    (is (coalton-impl/typechecker:ty= left right))
    (is (= 1 (length (coalton-impl/typechecker:ty-scheme-kinds scheme))))))

(deftest test-unary-function-types-match-arrow-applications ()
  (let* ((functor-var (coalton-impl/typechecker:make-tyvar
                       :id 1000
                       :kind (coalton-impl/typechecker:make-kfun
                              :from coalton-impl/typechecker:+kstar+
                              :to coalton-impl/typechecker:+kstar+)
                       :source-name :f))
         (result-var (coalton-impl/typechecker:make-tyvar
                      :id 1001
                      :kind coalton-impl/typechecker:+kstar+
                      :source-name :result))
         (expected
           (nth-value 0
             (coalton-impl/typechecker:apply-type-argument functor-var result-var)))
         (actual
           (coalton-impl/typechecker:make-function-type
            coalton-impl/typechecker:*integer-type*
            coalton-impl/typechecker:*boolean-type*))
         (subs (coalton-impl/typechecker:match expected actual))
         (arrow-partial
           (nth-value 0
             (coalton-impl/typechecker:apply-type-argument
              coalton-impl/typechecker:*arrow-type*
              coalton-impl/typechecker:*integer-type*))))
    (is (coalton-impl/typechecker:ty=
         (coalton-impl/typechecker:apply-substitution subs functor-var)
         arrow-partial))
    (is (coalton-impl/typechecker:ty=
         (coalton-impl/typechecker:apply-substitution subs result-var)
         coalton-impl/typechecker:*boolean-type*))))

(deftest test-arrow-matching-rejects-zero-output-functions ()
  (let* ((functor-var (coalton-impl/typechecker:make-tyvar
                       :id 1002
                       :kind (coalton-impl/typechecker:make-kfun
                              :from coalton-impl/typechecker:+kstar+
                              :to coalton-impl/typechecker:+kstar+)
                       :source-name :f))
         (result-var (coalton-impl/typechecker:make-tyvar
                      :id 1003
                      :kind coalton-impl/typechecker:+kstar+
                      :source-name :result))
         (expected
           (nth-value 0
             (coalton-impl/typechecker:apply-type-argument functor-var result-var)))
         (actual
           (coalton-impl/typechecker:make-function-ty
            :positional-input-types (list coalton-impl/typechecker:*integer-type*)
            :keyword-input-types nil
            :keyword-open-p nil
            :output-types nil)))
    (handler-case
        (progn
          (coalton-impl/typechecker:match expected actual)
          (is nil))
      (coalton-impl/typechecker/type-errors:unification-error ()
        (is t)))))

(deftest test-arrow-matching-rejects-multi-output-functions ()
  (let* ((functor-var (coalton-impl/typechecker:make-tyvar
                       :id 1004
                       :kind (coalton-impl/typechecker:make-kfun
                              :from coalton-impl/typechecker:+kstar+
                              :to coalton-impl/typechecker:+kstar+)
                       :source-name :f))
         (result-var (coalton-impl/typechecker:make-tyvar
                      :id 1005
                      :kind coalton-impl/typechecker:+kstar+
                      :source-name :result))
         (expected
           (nth-value 0
             (coalton-impl/typechecker:apply-type-argument functor-var result-var)))
         (actual
           (coalton-impl/typechecker:make-function-ty
            :positional-input-types (list coalton-impl/typechecker:*integer-type*)
            :keyword-input-types nil
            :keyword-open-p nil
            :output-types (list coalton-impl/typechecker:*boolean-type*
                                coalton-impl/typechecker:*string-type*))))
    (handler-case
        (progn
          (coalton-impl/typechecker:match expected actual)
          (is nil))
      (coalton-impl/typechecker/type-errors:unification-error ()
        (is t)))))
