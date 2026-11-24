;;;; fundep-tests.lisp

(in-package #:coalton-tests)

(deftest derived-eq-parametric-recursive-wrapper ()
  (check-coalton-types
   "(derive Eq)
    (define-type (RecList :a) NilR (ConsR :a (RecList :a)))
    (derive Eq)
    (define-type (Boxed :a) (BoxR (RecList :a)))
    (declare boxed-equal (Eq :a => Boxed :a * Boxed :a -> Boolean))
    (define (boxed-equal x y) (== x y))
    (define same-box (== (BoxR (ConsR (the Integer 1) NilR))
                        (BoxR (ConsR (the Integer 1) NilR))))"
   '("boxed-equal" . "(Eq :a => Boxed :a * Boxed :a -> Boolean)")
   '("same-box" . "Boolean")))

(deftest specialization-overlap-occurs-check ()
  (let* ((a (tc:make-variable))
         (b (tc:make-variable))
         (tuple (tc:type-entry-type (tc:lookup-type entry:*global-environment* 'coalton/classes:Tuple)))
         (list-b (tc:apply-type-argument tc:*list-type* b))
         (first-type (tc:make-function-type*
                      (list a) (tc:apply-type-argument-list tuple (list a a))))
         (second-type (tc:make-function-type*
                       (list list-b) (tc:apply-type-argument-list tuple (list b list-b))))
         (first-entry (tc:make-specialization-entry :from 'original :to 'first :to-ty first-type))
         (second-entry (tc:make-specialization-entry :from 'original :to 'second :to-ty second-type)))
    (signals tc:infinite-type-unification-error (tc:unify nil first-type second-type))
    (let ((env (tc:add-specialization
                (tc:add-specialization (tc:make-default-environment) first-entry) second-entry)))
      (is (eq first-entry (tc:lookup-specialization env 'original 'first)))
      (is (eq second-entry (tc:lookup-specialization env 'original 'second))))))

(deftest specialization-overlap-freshens-variables ()
  (let* ((a (tc:make-variable))
         (first-entry (tc:make-specialization-entry
                       :from 'original :to 'first
                       :to-ty (tc:make-function-type* (list a) tc:*integer-type*)))
         (second-entry (tc:make-specialization-entry
                        :from 'original :to 'second
                        :to-ty (tc:make-function-type*
                                (list (tc:apply-type-argument tc:*list-type* a))
                                tc:*integer-type*)))
         (env (tc:add-specialization (tc:make-default-environment) first-entry)))
    (signals tc:overlapping-specialization-error (tc:add-specialization env second-entry))))

(deftest synchronize-type-variable-counter-visits-shared-structures-once ()
  (let* ((var (tc:make-variable))
         (env (tc:add-specialization
               (tc:make-default-environment)
               (tc:make-specialization-entry :from 'original :to 'specialized :to-ty var))))
    (setf (tc:ty-alias var) (list var var))
    (let ((coalton-impl/typechecker/types::*next-variable-id* 0))
      (tc:synchronize-type-variable-counter env)
      (is (> (tc:tyvar-id (tc:make-variable)) (tc:tyvar-id var))))))

(deftest fundep-entail-composes-and-applies-substitutions ()
  (let ((*package* (make-package (gensym "FUNDEP-ENTAIL-") :use '("COALTON" "COALTON-PRELUDE"))))
    (unwind-protect
         (let ((source (source:make-source-string "(define-class (C :a :b (:a -> :b)))")))
           (with-open-stream (stream (source:source-stream source))
             (let* ((env (nth-value 1 (entry:entry-point
                                      (parser:with-reader-context stream
                                        (parser:read-program stream source)))))
                    (k1 (tc:make-variable))
                    (k2 (tc:make-variable))
                    (k3 (tc:make-variable))
                    (a (tc:make-variable))
                    (b (tc:make-variable))
                    (c (tc:make-variable))
                    (d (tc:make-variable)))
               (flet ((pred (from to)
                        (tc:make-ty-predicate :class (intern "C") :types (list from to))))
                 (let ((subs (tc:fundep-entail
                              env
                              (list (pred k1 b) (pred k2 c) (pred k3 d))
                              (list (pred k1 a) (pred k2 b) (pred k3 a))
                              (list k1 k2 k3))))
                   (dolist (var (list a b c))
                     (is (tc:ty= d (tc:apply-substitution subs var)))))
                 (let ((subs (tc:fundep-entail
                              env
                              (list (pred k1 b) (pred k2 a))
                              (list (pred k1 a) (pred k2 c))
                              (list k1 k2))))
                   (is (tc:ty= b (tc:apply-substitution subs a)))
                   (is (tc:ty= b (tc:apply-substitution subs c))))))))
      (delete-package *package*))))

(deftest context-reduction-applies-substitutions-once ()
  (let* ((a (tc:make-variable))
         (b (tc:make-variable))
         (pred (tc:make-ty-predicate :class 'coalton/classes:Num :types (list a)))
         (subs (list (tc:make-substitution :from a :to b)
                     (tc:make-substitution :from b :to tc:*integer-type*))))
    (is (null (tc:reduce-context entry:*global-environment* nil subs)))
    (let ((reduced (tc:reduce-context entry:*global-environment* (list pred) subs)))
      (is (= 1 (length reduced)))
      (is (tc:type-predicate= (first reduced) (tc:apply-substitution subs pred))))
    (is (null (tc:reduce-context entry:*global-environment*
                                (list (tc:make-ty-predicate :class 'coalton/classes:Num
                                                            :types (list tc:*integer-type*))) nil)))
    (is (tc:type-predicate= pred (first (tc:reduce-context entry:*global-environment* (list pred) nil))))))

(deftest class-method-result-binders-preserve-capabilities ()
  (check-coalton-types
   "(define-class (OutputOf :a) (output-of (:a -> :b)))"
   '("output-of" . "(OutputOf :a => :a -> :b)")))

(deftest fundep-improvement-is-independent-of-predicate-order ()
  (let ((*package* (make-package (gensym "FUNDEP-ORDER-") :use '("COALTON" "COALTON-PRELUDE"))))
    (unwind-protect
         (let ((source (source:make-source-string "(define-class (C :a :b (:a -> :b)))")))
           (with-open-stream (stream (source:source-stream source))
             (let ((env (nth-value 1 (entry:entry-point
                                     (parser:with-reader-context stream
                                       (parser:read-program stream source))))))
               (flet ((pred (from to)
                        (tc:make-ty-predicate :class (intern "C") :types (list from to))))
                 (let* ((a (tc:make-variable)) (b (tc:make-variable)) (c (tc:make-variable))
                        (preds (list (pred tc:*integer-type* a)
                                     (pred tc:*string-type* b)
                                     (pred tc:*integer-type* c))))
                   (alexandria:map-permutations
                    (lambda (permutation)
                      (let ((subs (nth-value 1 (tc:solve-fundeps env permutation nil))))
                        (is (tc:ty= (tc:apply-substitution subs a) (tc:apply-substitution subs c)))))
                    preds))
                 (signals tc:coalton-internal-type-error
                   (tc:solve-fundeps env (list (pred tc:*integer-type* tc:*integer-type*)
                                               (pred tc:*string-type* tc:*string-type*)
                                               (pred tc:*integer-type* tc:*boolean-type*)) nil))
                 ;; The last pair makes the first two determinants equal.
                 (let* ((a (tc:make-variable)) (b (tc:make-variable))
                        (x (tc:make-variable)) (y (tc:make-variable))
                        (subs (nth-value 1
                                         (tc:solve-fundeps
                                          env (list (pred a x) (pred b y)
                                                    (pred tc:*integer-type* a)
                                                    (pred tc:*integer-type* b)) nil))))
                   (is (tc:ty= (tc:apply-substitution subs x) (tc:apply-substitution subs y))))))))
      (delete-package *package*))))

(deftest scheme-equality-preserves-function-boundaries ()
  (let* ((left (tc:to-scheme
                (tc:make-function-ty
                 :positional-input-types (list tc:*integer-type* tc:*string-type*)
                 :output-types (list tc:*boolean-type*))))
         (right (tc:to-scheme
                 (tc:make-function-ty
                  :positional-input-types (list tc:*integer-type*)
                  :output-types (list tc:*string-type* tc:*boolean-type*)))))
    (is (not (tc:ty-scheme= left right)))
    (is (not (tc:ty-scheme= right left)))
    (is (tc:ty-scheme= left left)))
  (let* ((value-var (tc:make-variable))
         (result-var (tc:make-variable :allow-result-p t))
         (value-scheme (tc:quantify (list value-var) (tc:qualify nil value-var)))
         (result-scheme (tc:quantify (list result-var) (tc:qualify nil result-var))))
    (is (not (tc:ty-scheme= value-scheme result-scheme)))
    (is (not (tc:ty-scheme= result-scheme value-scheme)))))

(deftest instance-head-improvement-preserves-unknown-shapes ()
  (check-coalton-types
   "(define-class (ConvertTo :a :b) (convert-to (:a -> :b)))
    (define-instance (ConvertTo :a :a) (define (convert-to x) x))
    (declare convert-any (ConvertTo :a String => :a -> String))
    (define (convert-any x) (convert-to x))"
   '("convert-any" . "(ConvertTo :a String => :a -> String)"))
  (check-coalton-types
   "(define-type (Box :a) (Box :a))
    (define-class (ClassA :m :a (:m -> :a)) (get-a (:m -> :a)))
    (define-instance (ClassA (Box :a) :a) (define (get-a (Box x)) x))
    (define (get-any x) (get-a x))"
   '("get-any" . "(ClassA :m :a => :m -> :a)")))

(deftest fundep-partially-overlapping-determinants ()
  (dolist (instances
            '("(define-instance (C (Tuple :a Integer) :a))
               (define-instance (C (Tuple String :b) :b))"
              "(define-instance (C (Tuple String :b) :b))
               (define-instance (C (Tuple :a Integer) :a))"))
    (signals tc:tc-error
      (check-coalton-types
       (concatenate 'string "(define-class (C :a :b (:a -> :b)))" instances))))
  ;; Agreement at just one specialization of a determinant is insufficient.
  (signals tc:tc-error
    (check-coalton-types
     "(define-class (C :a :b :c (:a -> :b)))
      (define-instance (C :a :a Boolean))
      (define-instance (C :b Integer String))"))
  ;; Both partial relations must remain available when their outputs agree.
  (check-coalton-types
   "(define-class (C :a :b :c (:a -> :b)) (m (:a * :c -> :b)))
    (define-instance (C (Tuple :a Integer) Integer Boolean)
      (define (m _ _) 1))
    (define-instance (C (Tuple String :b) Integer String)
      (define (m _ _) 2))
    (define first-result (m (Tuple True (the Integer 1)) True))
    (define second-result (m (Tuple \"x\" True) \"x\"))"
   '("first-result" . "Integer")
   '("second-result" . "Integer"))
  (check-coalton-types
   "(define-class (C :a :b :c (:a -> :b)) (m (:a * :c -> :b)))
    (define-instance (C Integer String Boolean) (define (m _ _) \"specific\"))
    (define-instance (C :a String String) (define (m _ _) \"general\"))
    (define general-result (m True \"x\"))"
   '("general-result" . "String")))

(deftest predicate-mgu-refines-shared-variables ()
  (let* ((a (tc:make-variable))
         (b (tc:make-variable))
         (left (tc:make-ty-predicate :class 'c :types (list a a)))
         (right (tc:make-ty-predicate :class 'c :types (list b tc:*integer-type*))))
    (dolist (pair (list (cons left right) (cons right left)))
      (let ((subs (tc:predicate-mgu (car pair) (cdr pair))))
        (is (tc:type-predicate= (tc:apply-substitution subs left)
                                (tc:apply-substitution subs right)))
        (is (tc:ty= tc:*integer-type* (tc:apply-substitution subs a)))
        (is (tc:ty= tc:*integer-type* (tc:apply-substitution subs b)))))))

(deftest overlapping-nonlinear-instance-heads ()
  (dolist (instances '("(define-instance (C :a :a)) (define-instance (C :b Integer))"
                       "(define-instance (C :b Integer)) (define-instance (C :a :a))"))
    (signals tc:tc-error
      (check-coalton-types (concatenate 'string "(define-class (C :a :b))" instances))))
  (check-coalton-types
   "(define-class (C :a :b))
    (define-instance (C :a :a))
    (define-instance (C Integer String))"))

(deftest library-conversion-constraints ()
  ;; Open conversions retain dictionary parameters in polymorphic wrappers.
  (check-coalton-types
   "(declare build-seq (Into (:f :a) (coalton/seq:Seq :a)
                        => :f :a -> coalton/seq:Seq :a))
    (define (build-seq xs) (into xs))
    (declare convert-complex (Into (coalton/math:Complex :a)
                                   (coalton/math:Complex coalton/computable-reals:CReal)
                              => coalton/math:Complex :a
                              -> coalton/math:Complex coalton/computable-reals:CReal))
    (define (convert-complex z) (into z))
    (declare same (Into :a :a => :a -> :a))
    (define (same x) (into x))
    (declare same-via-iso (Iso :a :a => :a -> :a))
    (define (same-via-iso x) (into x))
    (define same-pair (the (Tuple Integer Integer)
                          (into (the (Tuple Integer Integer) (Tuple 1 2)))))
    (define text (the String (into (coalton/cell:read (coalton/cell:new (the Integer 42))))))")
  ;; A fallback's context alone cannot select a polymorphic conversion.
  (dolist (program
            '("(declare same (:a -> :a)) (define (same x) (into x))"
              "(declare build-seq (Foldable :f => :f :a -> coalton/seq:Seq :a))
               (define (build-seq xs) (into xs))"))
    (signals tc:tc-error (check-coalton-types program)))
  ;; Incomparable heads require overlap even when their contexts differ.
  (dolist (instances
            '("(define-instance (C (coalton/cell:Cell :a) :a))
               (define-instance (Into :a String => C (coalton/cell:Cell :a) String))"
              "(define-instance (Into :a String => C (coalton/cell:Cell :a) String))
               (define-instance (C (coalton/cell:Cell :a) :a))"))
    (signals tc:tc-error
      (check-coalton-types (concatenate 'string "(define-class (C :a :b))" instances)))))

(deftest cell-conversions-require-explicit-access ()
  (dolist (program
            '("(define wrapped (the (coalton/cell:Cell Integer) (into (the Integer 42))))"
              "(define unwrapped (the Integer (into (coalton/cell:new (the Integer 42)))))"
              "(define text (the String (into (coalton/cell:new \"42\"))))"
              "(define text (the String (into (coalton/cell:new (the Integer 42)))))"
              "(define text (the String (into (coalton/cell:new (coalton/cell:new (the Integer 42))))))"))
    (signals tc:tc-error (check-coalton-types program)))
  (check-coalton-types
   "(define (read-cell c) (coalton/cell:read c))
    (define (make-cell x) (coalton/cell:new x))
    (declare cell-text (Into :a String => coalton/cell:Cell :a -> String))
    (define (cell-text c) (into (coalton/cell:read c)))"
   '("read-cell" . "(coalton/cell:Cell :a -> :a)")
   '("make-cell" . "(:a -> coalton/cell:Cell :a)")))

(deftest occurs-check-compares-variable-identities ()
  (let* ((a (tc:make-variable))
         (copy (copy-structure a))
         (recursive (tc:make-tapp :from tc:*list-type* :to copy)))
    (is (tc:ty= a copy))
    (signals tc:coalton-internal-type-error (tc:unify nil a recursive))
    (signals tc:coalton-internal-type-error (tc:unify nil recursive a))
    (let ((b (tc:make-variable)))
      (signals tc:coalton-internal-type-error
        (tc:predicate-mgu
         (tc:make-ty-predicate :class 'c :types (list a recursive))
         (tc:make-ty-predicate :class 'c :types (list b b)))))))

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

(deftest fundep-tagged-instances-must-agree ()
  (let ((classes "(package coalton-unit-test/tagged-key)
                  (define-class (LKey :k :v (:k -> :v)))
                  (define-class (RKey :k :v (:k -> :v)))
                  (define-class (TaggedKey :k :v :tag (:k -> :v)))")
        (left "(define-instance (LKey Unit :v => TaggedKey Unit :v Boolean))")
        (right "(define-instance (RKey Unit :v => TaggedKey Unit :v Char))"))
    (dolist (prefix '("(define-instance (LKey Unit Integer))
                      (define-instance (RKey Unit String))"
                     ;; New instances could satisfy the two prerequisites later.
                     ""
                     ;; This instance agrees with either neighbor, but all three
                     ;; share an improvement entry. Still compare the neighbors.
                     "(define-instance ((LKey Unit :v) (RKey Unit :v) => TaggedKey Unit :v Unit))"))
      (dolist (order (list (list left right) (list right left)))
        (let ((error (collect-compiler-error (format nil "~A~%~A~%~{~A~%~}" classes prefix order))))
          (is (search "Instance fundep conflict" error))
          (is (search "equal outputs" error)))))))

(deftest fundep-tagged-instances-can-use-a-shared-prerequisite ()
  (let ((classes "(define-class (Key :k :v (:k -> :v)) (get-key (:k -> :v)))
                  (define-class (TaggedKey :k :v :tag (:k -> :v)) (get-tagged (:k * :tag -> :v)))
                  (define-instance (Key Unit Integer) (define (get-key _) 42))")
        (left "(define-instance (Key :k :v => TaggedKey :k :v Boolean)
                 (define (get-tagged k _) (get-key k)))
               (define via-boolean (get-tagged Unit True))")
        (right "(define-instance (Key :k :out => TaggedKey :k :out Char)
                  (define (get-tagged k _) (get-key k)))
                (define via-char (get-tagged Unit #\\c))"))
    (check-coalton-types (concatenate 'string classes left) '("via-boolean" . "Integer"))
    (check-coalton-types (concatenate 'string classes left right)
                         '("via-boolean" . "Integer") '("via-char" . "Integer"))))

(deftest fundep-tagged-instances-preserve-improvement ()
  ;; If both prerequisites hold, their outputs agree. But the first row
  ;; would improve the output to List even where only the second holds.
  (signals tc:tc-error
    (check-coalton-types
     "(define-class (Key :k :v (:k -> :v)))
      (define-class (TaggedKey :k :v :tag (:k -> :v)))
      (define-instance (Key Unit Integer))
      (define-instance (Key :k (List :v) => TaggedKey :k (List :v) Boolean))
      (define-instance (Key :k :v => TaggedKey :k :v Char))")))

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

(deftest fundep-entail-without-declared-predicate ()
  ;; A declaration lacking a predicate with a functional dependency is missing
  ;; that predicate, which must be reported as such.
  (signals tc:tc-error
    (check-coalton-types
     "(define-class (C :a :b (:a -> :b))
        (m (:a -> :b)))
      (declare ignored (:b -> Boolean))
      (define (ignored _) True)
      (declare f (:a -> Boolean))
      (define (f x) (ignored (m x)))")))

(deftest fundep-entail-hidden-superclass-order ()
  (let ((classes
          "(define-class (Handles :rt :d (:rt -> :d))
             (make-handle (:rt -> :d)))
           (define-class (Handles :rt :d => WithRuntime :m :rt (:m -> :rt))
             (runtime-of (:m -> :rt)))
           (define-class (WithRuntime :m :rt => Delays :m))
           (define-class (WithRuntime :m :rt => NamedRuntime :m :rt))"))
    (flet ((program (preds)
             (format nil "~A
               (declare start (~{~A~^ ~} => :m -> :d))
               (define (start m) (make-handle (runtime-of m)))" classes preds)))
      (dolist (runtime '("(WithRuntime :m :rt)" "(NamedRuntime :m :rt)"))
        (alexandria:map-permutations
         (lambda (preds) (check-coalton-types (program preds)))
         (list "(Delays :m)" runtime "(Handles :rt :d)")))
      ;; The hidden handle type is not a caller-chosen result type.
      (dolist (preds '(("(Delays :m)" "(WithRuntime :m :rt)")
                       ("(WithRuntime :m :rt)" "(Delays :m)")))
        (signals tc:tc-error (check-coalton-types (program preds)))))))

(deftest fundep-entail-partially-hidden-superclasses ()
  ;; Neither dictionary names the full Assoc predicate on its own.
  (check-coalton-types
   "(define-class (Assoc :a :b :c (:a -> :b :c))
      (both-values (:a -> Tuple :b :c)))
    (define-class (Assoc :a :b :c => FirstOf :a :b))
    (define-class (Assoc :a :b :c => SecondOf :a :c))
    (declare both ((FirstOf :a :b) (SecondOf :a :c) => :a -> Tuple :b :c))
    (define (both x) (both-values x))
    (declare use-reader ((forall (:a :b :c) ((FirstOf :a :b) (SecondOf :a :c)
                                             => :a -> Tuple :b :c)) -> Unit))
    (define (use-reader _) Unit)
    (define reader-value (use-reader (fn (x) (both-values x))))"))

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

(deftest fundep-shared-hidden-dependent-restricts-declared-variables ()
  ;; Both :p and :q can be renamed to the hidden variable, but they
  ;; cannot both be chosen independently as the declaration promises.
  (is (search "Declared context is too general"
              (collect-compiler-error
               "(package coalton-unit-test/shared-hidden-dependent)
                (define-class (Parent :x :y (:x -> :y)))
                (define-class ((Parent :x :hidden) (Parent :z :hidden) => SameOutput :x :z))
                (declare f ((SameOutput :x :z) (Parent :x :p) (Parent :z :q) => :x * :z -> :x))
                (define (f x _) x)"))))

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

(deftest fundep-instance-conflicts-with-own-context ()
  ;; See https://github.com/coalton-lang/coalton/issues/2068
  (flet ((program (fundep)
           (format nil
                   "(define-struct (Env :input :world)
                      (input :input)
                      (world :world))

                    (define-type (Decision :act :final)
                      (Done :final)
                      (StepAct :act))

                    (define-class ((Monad :m) => Foo :m :act :input :world :final ~A)
                      (call-model (:input -> :m (Decision :act :final)))
                      (exec-action (:act * (Env :input :world) -> :m (Env :input :world))))

                    (define-instance ((Foo :m :act :input :world :final)
                                      => Foo (coalton/monad/statet:StateT (Env :input :world) :m)
                                             :act :input :world :final)
                      (define (call-model inp)
                        (coalton/monad/statet:lift-stateT (call-model inp)))
                      (define (exec-action act env)
                        (coalton/monad/statet:lift-stateT (exec-action act env))))"
                   fundep)))
    ;; The head and its context share the determinant :act, so :m would
    ;; have to be (StateT (Env :input :world) :m).
    (is (search "Instance fundep conflict"
                (handler-case
                    (progn
                      (check-coalton-types (program "(:act -> :m :input :world :final)"))
                      "")
                  (tc:tc-error (e)
                    (princ-to-string e)))))
    (check-coalton-types (program "(:m :input :act -> :world :final)"))
    (check-coalton-types (program "(:m -> :act :input :world :final)"))))

(deftest fundep-improvement-through-instance-context ()
  ;; The context of this instance determines the dependent of its head,
  ;; and improvement through it terminates when the determinant shrinks.
  (check-coalton-types
   "(define-type (Vec :a) (Vec :a))

    (define-class (Mul :a :b :c (:a :b -> :c))
      (mul (:a * :b -> :c)))

    (define-instance (Mul :a :b :c => Mul :a (Vec :b) (Vec :c))
      (define (mul a (Vec b))
        (Vec (mul a b))))

    (define (mul-vec x y)
      (mul x (Vec y)))"
   '("mul-vec" . "(Mul :a (Vec :b) (Vec :c) => :a * :b -> Vec :c)")))

(deftest fundep-solving-reduces-predicates-together ()
  ;; Independent instance reductions share a round, so the round limit
  ;; bounds the depth of improvement rather than the number of predicates.
  (let ((*package* (make-package (gensym "FUNDEP-BREADTH-") :use '("COALTON" "COALTON-PRELUDE"))))
    (unwind-protect
         (let ((source (source:make-source-string "(define-class (C :a :b (:a -> :b)))")))
           (with-open-stream (stream (source:source-stream source))
             (let* ((env (nth-value 1 (entry:entry-point
                                       (parser:with-reader-context stream
                                         (parser:read-program stream source)))))
                    (fundep-pred (tc:make-ty-predicate
                                  :class (intern "C")
                                  :types (list (tc:make-variable) (tc:make-variable))))
                    (preds (cons fundep-pred
                                 (loop :repeat (* 2 tc:+fundep-max-depth+)
                                       :collect (tc:make-ty-predicate
                                                 :class 'coalton/classes:Eq
                                                 :types (list tc:*integer-type*))))))
               (let ((solved (tc:solve-fundeps env preds nil)))
                 (is (= 1 (length solved)))
                 (is (tc:type-predicate= fundep-pred (first solved)))))))
      (delete-package *package*))))

(deftest fundep-mutually-recursive-instances-do-not-terminate ()
  ;; Each instance determines its monad through the other class on the
  ;; same :act, so improving either class never reaches a fixed point.
  (dolist (use '("(declare f (Foo :m :act => :act -> Unit))
                  (define (f _) Unit)"
                 "(define (f x) (foo x))"
                 ;; TYPE-OF reports internal type errors as type mismatches.
                 "(define (f x) (type-of (foo x)))"))
    (is (search "Functional dependency improvement does not terminate"
                (handler-case
                    (progn
                      (check-coalton-types
                       (concatenate
                        'string
                        "(define-type (W1 :a) (W1 :a))
                         (define-type (W2 :a) (W2 :a))
                         (define-class (Foo :m :act (:act -> :m))
                           (foo (:act -> :m)))
                         (define-class (Bar :m :act (:act -> :m)))
                         (define-instance (Bar :m :act => Foo (W1 :m) :act)
                           (define (foo _) (error \"unreachable\")))
                         (define-instance (Foo :m :act => Bar (W2 :m) :act))"
                        use))
                      "")
                  (tc:tc-error (e)
                    (princ-to-string e)))))))

(deftest fundep-improvement-with-context-determined-dependents ()
  ;; The instance context determines the element of the dependent, so
  ;; improving a predicate whose dependent already has that shape only
  ;; renames the element. This used to repeat until the round limit.
  (check-coalton-types
   "(define-class (C :a :b :c (:a -> :b))
      (cm (:a * :c -> :b)))

    (define-class (D :a :x (:a -> :x)))

    (define-instance (D :a :x => C :a (List :x) Boolean)
      (define (cm _ _) Nil))

    (define (f) (cm (the Integer 1) #\\c))

    (declare g (C :a (List String) Char => :a -> Unit))
    (define (g _) Unit)"
   '("f" . "(C Integer (List :a) Char => Void -> List :a)")))

(deftest fundep-improvement-with-repeated-determinant-variables ()
  ;; See https://github.com/coalton-lang/coalton/issues/2068. A determinant
  ;; that repeats a variable only applies to predicates whose types agree
  ;; there; matching other predicates against it used to escape as an
  ;; unhandled internal condition.
  (check-coalton-types
   "(define-class (C :a :b :c (:a :b -> :c))
      (cm (:a * :b -> :c)))

    (define-instance (C (List :a) :a String)
      (define (cm _ _) \"\"))

    (define (f y) (cm (the (List Integer) Nil) y))

    (define (g) (cm (the (List Integer) Nil) (the Integer 1)))"
   '("f" . "(C (List Integer) :a :b => :a -> :b)")
   '("g" . "(Void -> String)"))
  (check-coalton-types
   "(define-struct (Env :input :world)
      (input :input)
      (world :world))

    (define-type (Decision :act :final)
      (Done :final)
      (StepAct :act))

    (define-class ((Monad :m) => Foo :m :act :input :world :final
                   (:m :input :act -> :world :final)
                   (:m :input :world :final -> :act))
      (call-model (:input -> :m (Decision :act :final))))

    (define-instance ((Foo :m :act :input :world :final)
                      => Foo (coalton/monad/statet:StateT (Env :input :world) :m)
                             :act :input :world :final)
      (define (call-model inp)
        (coalton/monad/statet:lift-stateT (call-model inp))))

    (declare use ((Foo (coalton/monad/statet:StateT (Env Integer String) :m) :act :i :w :f)
                  => :i -> coalton/monad/statet:StateT (Env Integer String) :m (Decision :act :f)))
    (define (use x) (call-model x))"))

(deftest fundep-conflicting-inferred-predicates ()
  ;; Both uses of CM share the determinant :a but not its dependent.
  (dolist (body '("(g (cm x) (cm x))"
                  ;; TYPE-OF reports internal type errors as type mismatches.
                  "(type-of (g (cm x) (cm x)))"
                  "(type-of (rec loop ((i x)) (g (cm i) (cm i))))"))
    (is (search "Context conflicts with functional dependencies"
                (handler-case
                    (progn
                      (check-coalton-types
                       (format nil
                               "(define-class (C :a :b (:a -> :b))
                                  (cm (:a -> :b)))

                                (declare g (String * Integer -> Unit))
                                (define (g _ _) Unit)

                                (define (f x) ~A)"
                               body))
                      "")
                  (tc:tc-error (e)
                    (princ-to-string e)))))))

(deftest circular-and-growing-instance-prerequisites ()
  (labels ((call-in-temp-env (fn)
             (let ((*package* (make-package (gensym "CYCLE-TEST-") :use '("COALTON" "COALTON-PRELUDE")))
                   (entry:*global-environment* entry:*global-environment*))
               (unwind-protect (funcall fn)
                 (delete-package *package*))))
           (pred (class-name &rest types)
             (tc:make-ty-predicate :class (if (stringp class-name)
                                              (intern class-name *package*)
                                              class-name)
                                   :types types))
           (dict-params (class-name)
             (cdr (tc:instance-dictionary-signature
                   (first (tc:lookup-class-instances
                           entry:*global-environment*
                           (intern class-name *package*))))))
           (assert-unprovable (witness-name)
             (dolist (ty '("Integer" "String"))
               (signals tc:tc-error
                 (overlap-compile
                  (format nil "(declare prove-goal (Void -> ~A))
                               (define (prove-goal) (~A Unit))"
                          ty witness-name))))))
    ;; 1. Self-circular prerequisite does not manufacture a nullary dictionary.
    (call-in-temp-env
     (lambda ()
       (overlap-compile
        "(define-class (Rel :key :out (:key -> :out)))
         (define-instance (Rel Unit :v => Rel Unit :v))
         (declare witness (Rel :key :out => :key -> :out))
         (define (witness _) (error \"unreachable\"))")
       (let ((env entry:*global-environment*)
             (params (dict-params "REL"))
             (int-pred (pred "REL" tc:*unit-type* tc:*integer-type*))
             (str-pred (pred "REL" tc:*unit-type* tc:*string-type*)))
         (is (= 1 (length params)))
         (is (eq (intern "REL" *package*) (tc:ty-predicate-class (first params))))
         (is (not (tc:entail env nil int-pred)))
         (is (not (tc:entail env nil str-pred)))
         (is (tc:entail env (list int-pred) int-pred))
         (assert-unprovable "witness"))))

    ;; 2. Mutual circular prerequisites (same batch and separate batches).
    (dolist (separate-p '(nil t))
      (call-in-temp-env
       (lambda ()
         (let ((part1 "(define-class (Rel1 :key :out (:key -> :out)))
                       (define-class (Rel2 :key :out (:key -> :out)))
                       (define-instance (Rel2 Unit :v => Rel1 Unit :v))")
               (part2 "(define-instance (Rel1 Unit :v => Rel2 Unit :v))"))
           (if separate-p
               (progn (overlap-compile part1) (overlap-compile part2))
               (overlap-compile (concatenate 'string part1 part2))))
         (overlap-compile
          "(declare witness1 (Rel1 :key :out => :key -> :out))
           (define (witness1 _) (error \"unreachable\"))")
         (let ((env entry:*global-environment*)
               (int-pred1 (pred "REL1" tc:*unit-type* tc:*integer-type*))
               (str-pred1 (pred "REL1" tc:*unit-type* tc:*string-type*))
               (int-pred2 (pred "REL2" tc:*unit-type* tc:*integer-type*))
               (str-pred2 (pred "REL2" tc:*unit-type* tc:*string-type*)))
           (is (= 1 (length (dict-params "REL1"))))
           (is (= 1 (length (dict-params "REL2"))))
           (dolist (p (list int-pred1 str-pred1 int-pred2 str-pred2))
             (is (not (tc:entail env nil p))))
           (is (tc:entail env (list int-pred2) int-pred1))
           (assert-unprovable "witness1")))))

    ;; 3. Acyclic control with Other Unit :v => Rel Unit :v.
    (call-in-temp-env
     (lambda ()
       (overlap-compile
        "(define-class (Rel :key :out (:key -> :out)))
         (define-class (Other :key :out (:key -> :out)))
         (define-instance (Other Unit :v => Rel Unit :v))
         (declare witness (Rel :key :out => :key -> :out))
         (define (witness _) (error \"unreachable\"))")
       (let ((int-pred (pred "REL" tc:*unit-type* tc:*integer-type*))
             (str-pred (pred "REL" tc:*unit-type* tc:*string-type*)))
         (is (not (tc:entail entry:*global-environment* nil int-pred)))
         (is (not (tc:entail entry:*global-environment* nil str-pred)))
         (assert-unprovable "witness")
         (overlap-compile "(define-instance (Other Unit Integer))")
         (is (tc:entail entry:*global-environment* nil int-pred))
         (is (not (tc:entail entry:*global-environment* nil str-pred)))
         (overlap-compile "(declare prove-int (Void -> Integer))
                           (define (prove-int) (witness Unit))")
         (signals tc:tc-error
           (overlap-compile "(declare prove-str (Void -> String))
                             (define (prove-str) (witness Unit))")))))

    ;; 4. Strictly growing prerequisites terminate and retain their obligation.
    (call-in-temp-env
     (lambda ()
       (overlap-compile
        "(define-class (Growing :a))
         (define-instance (Growing (List :a) => Growing :a))
         (define-class (GrowingA :a))
         (define-class (GrowingB :a))
         (define-instance (GrowingB (List :a) => GrowingA :a))
         (define-instance (GrowingA (List :a) => GrowingB :a))
         (declare need-growing (Growing :a => :a -> Unit))
         (define (need-growing _) Unit)")
       (let ((env entry:*global-environment*)
             (params (dict-params "GROWING"))
             (int-pred (pred "GROWING" tc:*integer-type*))
             (list-int-pred (pred "GROWING" (tc:apply-type-argument tc:*list-type* tc:*integer-type*))))
         (is (= 1 (length params)))
         (is (eq (intern "GROWING" *package*) (tc:ty-predicate-class (first params))))
         (is (= 1 (length (dict-params "GROWINGA"))))
         (is (= 1 (length (dict-params "GROWINGB"))))
         (is (not (tc:entail env nil int-pred)))
         (is (not (tc:entail env nil list-int-pred)))
         (is (tc:entail env (list list-int-pred) int-pred))
         (is (not (tc:entail env nil (pred "GROWINGA" tc:*integer-type*))))
         (is (not (tc:entail env nil (pred "GROWINGB" tc:*integer-type*))))
         (signals tc:tc-error
           (overlap-compile "(define bad (need-growing (the Integer 1)))")))))

    ;; 5. Decreasing counterpart resolves nested lists of Integer.
    (call-in-temp-env
     (lambda ()
       (overlap-compile
        "(define-class (Growing :a)
           (nesting-depth (:a -> Integer)))
         (define-instance (Growing Integer)
           (define (nesting-depth _) 0))
         (define-instance (Growing :a => Growing (List :a))
           (define (nesting-depth xs)
             (match xs
               ((Nil) 1)
               ((Cons x _) (+ 1 (nesting-depth x))))))")
       (let ((env entry:*global-environment*)
             (nested-int (tc:apply-type-argument
                          tc:*list-type*
                          (tc:apply-type-argument
                           tc:*list-type*
                           (tc:apply-type-argument tc:*list-type* tc:*integer-type*))))
             (nested-str (tc:apply-type-argument
                          tc:*list-type*
                          (tc:apply-type-argument tc:*list-type* tc:*string-type*))))
         (is (tc:entail env nil (pred "GROWING" nested-int)))
         (is (not (tc:entail env nil (pred "GROWING" nested-str))))
         (is (= 3 (overlap-eval "(nesting-depth (Cons (Cons (Cons (the Integer 7) Nil) Nil) Nil))")))
         (signals tc:tc-error
           (overlap-compile "(define bad (nesting-depth (Cons (Cons \"no\" Nil) Nil)))")))))

    ;; 6. Parameterized mutually recursive derive drops only the datatype cycle.
    (let ((mut-types "(define-type UnEq UnEq)
                      (derive Eq)
                      (define-type (MutA :a) MutA0 (MutAn :a (MutB :a)))
                      (derive Eq)
                      (define-type (MutB :a) MutB0 (MutBn (MutA :a)))"))
      (check-coalton-types
       (concatenate 'string mut-types
                    "(declare same-mut (Eq :a => MutA :a * MutA :a -> Boolean))
                     (define (same-mut x y) (== x y))
                     (define ok-mut (== (MutAn (the Integer 1) (MutBn MutA0))
                                        (MutAn (the Integer 1) (MutBn MutA0))))")
       '("same-mut" . "(Eq :a => MutA :a * MutA :a -> Boolean)")
       '("ok-mut" . "Boolean"))
      (signals tc:tc-error
        (check-coalton-types
         (concatenate 'string mut-types
                      "(define bad-mut (== (MutAn UnEq MutB0) (MutAn UnEq MutB0)))"))))

    ;; 7. Direct and indirect superclass back-edges cannot manufacture nullary dictionaries.
    (dolist (program
              '("(define-class (R :key :out (:key -> :out)))
                 (define-class (R :key :out => S :key :out))
                 (define-instance (S Unit :v => R Unit :v))
                 (define-instance (S Unit :v))"
                "(define-class (R :key :out (:key -> :out)))
                 (define-class (R :key :out => S :key :out))
                 (define-instance (S Unit :v))
                 (define-instance (S Unit :v => R Unit :v))"
                "(define-class (B :a))
                 (define-class (B :a => A :a))
                 (define-class (C :a))
                 (define-class (C :a => D :a))
                 (define-instance (A Unit))
                 (define-instance (D Unit))
                 (define-instance (D Unit => B Unit))
                 (define-instance (A Unit => C Unit))"))
      (signals tc:tc-error
        (check-coalton-types program)))

    ;; 8. Non-structural user cycle through a derived type is not discharged.
    (call-in-temp-env
     (lambda ()
       (overlap-compile
        "(define-type (Phantom :a) Phantom)
         (derive Eq)
         (define-type Victim (Victim (Phantom Unit)))
         (define-instance (Eq Victim => Eq (Phantom Unit))
           (define (== _ _) True))")
       (let* ((env entry:*global-environment*)
              (victim-ty (tc:type-entry-type (tc:lookup-type env (intern "VICTIM" *package*))))
              (victim-pred (pred 'coalton/classes:Eq victim-ty))
              (inst (tc:lookup-class-instance env victim-pred)))
         (is (= 1 (length (cdr (tc:instance-dictionary-signature inst)))))
         (is (not (tc:entail env nil victim-pred)))
         (signals tc:tc-error
           (overlap-compile "(define bad (== (Victim Phantom) (Victim Phantom)))")))))

    ;; 9. Recursive derive through non-shrinking user container wrappers.
    (call-in-temp-env
     (lambda ()
       (overlap-compile
        "(define-type (MySeq :a) (MySeq (List :a)))
         (define-instance (Eq (List :a) => Eq (MySeq :a))
           (define (== (MySeq xs) (MySeq ys)) (== xs ys)))
         (define-type (MyOuterSeq :a) (MyOuterSeq (MySeq :a)))
         (define-instance (Eq (MySeq :a) => Eq (MyOuterSeq :a))
           (define (== (MyOuterSeq xs) (MyOuterSeq ys)) (== xs ys)))
         (derive Eq)
         (define-type (SeqTree :a) (SeqNode :a (MyOuterSeq (SeqTree :a))))")
       (is (overlap-eval
            "(== (SeqNode (the Integer 1) (MyOuterSeq (MySeq Nil)))
                 (SeqNode (the Integer 1) (MyOuterSeq (MySeq Nil))))"))))

    ;; 10. Finite concrete recursive field instantiations in derive (direct and List-wrapped).
    (call-in-temp-env
     (lambda ()
       (overlap-compile
        "(derive Eq)
         (define-type (AuditNest :a)
           AuditLeaf
           (AuditSmall (AuditNest Integer))
           (AuditLarge (AuditNest (List Integer))))
         (derive Eq)
         (define-type (AuditNestList :a)
           AuditListLeaf
           (AuditListSmall (List (AuditNestList Integer)))
           (AuditListLarge (List (AuditNestList (List Integer)))))")
       (is (overlap-eval "(== (the (AuditNest Integer) AuditLeaf) AuditLeaf)"))
       (is (overlap-eval "(== (the (AuditNest String) (AuditSmall (AuditLarge AuditLeaf)))
                              (AuditSmall (AuditLarge AuditLeaf)))"))
       (is (overlap-eval "(== (the (AuditNestList String) AuditListLeaf) AuditListLeaf)"))
       (is (overlap-eval "(== (the (AuditNestList String)
                                   (AuditListSmall (Cons (AuditListLarge (Cons AuditListLeaf Nil)) Nil)))
                              (AuditListSmall (Cons (AuditListLarge (Cons AuditListLeaf Nil)) Nil)))"))))

    ;; 11. Multi-step growing proofs toward givens and overlapping base instances,
    ;;     plus function, forall, and deep DAG types.
    (call-in-temp-env
     (lambda ()
       (overlap-compile
        "(define-class (G :a) (g-val (:a -> Integer)))
         (overlap)
         (define-instance (G (List :a) => G :a) (define (g-val x) (g-val (Cons x Nil))))
         (overlap)
         (define-instance (G (List (List Integer))) (define (g-val _) 42))
         (declare from-two-lists-g (G (List (List String)) => String -> Integer))
         (define (from-two-lists-g x) (g-val x))
         (define-class (GPoly :a) (gpoly-val (:a -> Integer)))
         (overlap)
         (define-instance (GPoly (List :a) => GPoly :a) (define (gpoly-val x) (gpoly-val (Cons x Nil))))
         (overlap)
         (define-instance (GPoly (List (List :a))) (define (gpoly-val _) 99))
         (define-class (GDup :a) (gdup-val (:a -> Integer)))
         (overlap)
         (define-instance (GDup (List (Tuple :a :a)) => GDup :a)
           (define (gdup-val x) (gdup-val (Cons (Tuple x x) Nil))))
         (overlap)
         (define-instance (GDup (List (Tuple (List :a) :b))) (define (gdup-val _) 77))
         (define-class (G2 :a) (g2-val (:a -> Integer)))
         (define-instance (G2 (List :a) => G2 :a) (define (g2-val x) (g2-val (Cons x Nil))))
         (declare from-two-lists (G2 (List (List :a)) => :a -> Integer))
         (define (from-two-lists x) (g2-val x))
         (define-class (WalkA :a) (walk-mark (:a -> Unit)))
         (define-class (WalkB :a))
         (overlap)
         (define-instance (WalkB (List :a) => WalkA :a) (define (walk-mark _) Unit))
         (overlap)
         (define-instance (WalkA :a => WalkB :a))
         (overlap)
         (define-instance (WalkB (List (List (List Integer)))))
         (declare observe-walk (Integer -> Unit))
         (define (observe-walk n) (walk-mark n))
         (define-class (WalkA4 :a) (walk-mark4 (:a -> Unit)))
         (define-class (WalkB4 :a))
         (overlap)
         (define-instance (WalkB4 (List :a) => WalkA4 :a) (define (walk-mark4 _) Unit))
         (overlap)
         (define-instance (WalkA4 :a => WalkB4 :a))
         (overlap)
         (define-instance (WalkB4 (List (List (List (List Integer))))))
         (declare observe-walk4 (Integer -> Unit))
         (define (observe-walk4 n) (walk-mark4 n))
         (define-class (WalkNoBaseA :a) (walk-no-base (:a -> Unit)))
         (define-class (WalkNoBaseB :a))
         (overlap)
         (define-instance (WalkNoBaseB (List :a) => WalkNoBaseA :a) (define (walk-no-base _) Unit))
         (overlap)
         (define-instance (WalkNoBaseA :a => WalkNoBaseB :a))
         (define-class (WalkC :a) (walk-c (:a -> Integer)))
         (define-class (WalkD :a))
         (define-instance (WalkD (List :a) => WalkC :a) (define (walk-c _) 17))
         (overlap)
         (define-instance (WalkC :a => WalkD :a))
         (overlap)
         (define-instance (WalkD (List (List (List Integer)))))
         (define-type (Carrier :a) (Carry :a))
         (define-class (CarrierEq :a) (carrier-eq (:a * :a -> Boolean)))
         (define-instance (CarrierEq (Carrier :a) => Eq (Carrier :a))
           (define (== x y) (carrier-eq x y)))
         (derive Eq)
         (define-type (AuditTree :a)
           (AuditLeaf :a)
           (AuditNode (Carrier (AuditTree :a))))
         (overlap)
         (define-instance (Eq (AuditTree :a) => CarrierEq (Carrier (AuditTree :a)))
           (define (carrier-eq (Carry x) (Carry y)) (== x y)))
         (overlap)
         (define-instance (CarrierEq (Carrier (AuditTree Integer)))
           (define (carrier-eq _ _) True))
         (declare ground-check (Void -> Boolean))
         (define (ground-check)
           (== (AuditNode (Carry (AuditLeaf (the Integer 1))))
               (AuditNode (Carry (AuditLeaf (the Integer 2))))))
         (define-class (Self :a) (self-val (:a -> Integer)))
         (define-instance (Self :a => Self :a) (define (self-val x) (self-val x)))
         (declare pass-self (Self :a => :a -> Integer))
         (define (pass-self x) (self-val x))")
       (is (= 42 (overlap-eval "(g-val (the Integer 0))")))
       (is (= 99 (overlap-eval "(gpoly-val (Tuple (the Integer 1) (the Integer 2)))")))
       (is (= 77 (overlap-eval "(gdup-val (Tuple (the Integer 1) (the Integer 2)))")))
       (is (eq coalton:Unit (overlap-eval "(observe-walk (the Integer 0))")))
       (is (eq coalton:Unit (overlap-eval "(observe-walk4 (the Integer 0))")))
       (is (= 17 (overlap-eval "(walk-c (the Integer 0))")))
       (is (overlap-eval "(ground-check)"))
       (signals tc:tc-error
         (overlap-compile "(define bad-audit (== (AuditNode (Carry (AuditLeaf \"a\")))
                                                 (AuditNode (Carry (AuditLeaf \"b\")))))"))
       (signals tc:tc-error
         (overlap-compile "(define bad-g (g-val \"no\"))"))
       (signals tc:tc-error
         (overlap-compile "(define bad-walk (walk-mark \"no\"))"))
       (signals tc:tc-error
         (overlap-compile "(define bad-walk4 (walk-mark4 \"no\"))"))
       (signals tc:tc-error
         (overlap-compile "(define bad-walk-no-base (walk-no-base (the Integer 0)))"))
       (signals tc:tc-error
         (overlap-compile "(define bad-walk-c (walk-c \"no\"))"))
       (let* ((env entry:*global-environment*)
              (g2-int (pred "G2" tc:*integer-type*))
              (g2-list2-int (pred "G2" (tc:apply-type-argument
                                        tc:*list-type*
                                        (tc:apply-type-argument tc:*list-type* tc:*integer-type*))))
              (fn-ty (tc:make-function-type* (list tc:*integer-type*) tc:*integer-type*))
              (b (tc:make-variable))
              (forall-ty (tc:make-forall-ty
                          :binders (list b)
                          :type (tc:qualify nil (tc:make-function-type* (list b) b))))
              (higher-fn-ty (tc:make-function-type* (list forall-ty) tc:*integer-type*))
              (dag-ty (loop :for ty := tc:*integer-type*
                              :then (tc:make-function-type* (list ty) ty)
                            :repeat 66
                            :finally (return ty)))
              (dag-pred (pred "SELF" dag-ty)))
         (is (tc:entail env (list g2-list2-int) g2-int))
         (is (not (tc:entail env nil g2-int)))
         (is (not (tc:entail env nil (pred "SELF" fn-ty))))
         (is (not (tc:entail env nil (pred "SELF" higher-fn-ty))))
         (is (= 67 (tc:predicate-height dag-pred)))
         (is (tc:predicate-embeds-p dag-pred dag-pred nil)))))

    ;; 12. Hand-written recursive contexts and non-regular derived types retain obligations.
    (call-in-temp-env
     (lambda ()
       (overlap-compile
        "(define-type (UT :a) UL (UN :a (List (UT :a))))
         (define-instance ((Eq :a) (Eq (List (UT :a))) => Eq (UT :a))
           (define (== x y)
             (match (Tuple x y)
               ((Tuple (UL) (UL)) True)
               ((Tuple (UN a xs) (UN b ys)) (and (== a b) (== xs ys)))
               (_ False))))
         (derive Eq)
         (define-type (Nst :a) NNil (NCons :a (Nst (List :a))))")
       (signals tc:tc-error
         (overlap-compile "(define bad-ut (== (the (UT Integer) UL) UL))"))
       (signals tc:tc-error
         (overlap-compile "(define bad-nst (== (the (Nst Integer) NNil) NNil))"))))

    ;; 13. FASL round-trip preserves retained circular/growing dictionary signatures.
    (call-in-temp-env
     (lambda ()
       (uiop:with-temporary-file (:stream out :pathname src :type "lisp")
         (format out "(in-package ~S)~%~
                      (coalton-toplevel~%~
                        (define-class (Rel :key :out (:key -> :out)))~%~
                        (define-instance (Rel Unit :v => Rel Unit :v))~%~
                        (define-class (Growing :a))~%~
                        (define-instance (Growing (List :a) => Growing :a)))~%"
                 (package-name *package*))
         :close-stream
         (uiop:with-temporary-file (:pathname fasl :type (pathname-type (compile-file-pathname src)))
           (let ((base entry:*global-environment*))
             (is (not (nth-value 2 (compile-file src :output-file fasl :verbose nil :print nil))))
             (setf entry:*global-environment* base)
             (load fasl :verbose nil)
             (let ((env entry:*global-environment*))
               (is (= 1 (length (dict-params "REL"))))
               (is (= 1 (length (dict-params "GROWING"))))
               (is (not (tc:entail env nil (pred "REL" tc:*unit-type* tc:*integer-type*))))
               (is (not (tc:entail env nil (pred "REL" tc:*unit-type* tc:*string-type*))))
               (is (not (tc:entail env nil (pred "GROWING" tc:*integer-type*))))))))))))

