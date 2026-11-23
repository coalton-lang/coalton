(in-package #:coalton-native-tests)

(coalton-toplevel
  (derive Eq)
  (define-type PatternNumber (PatternNumber Integer))
  (define-instance (Num PatternNumber)
    (define (+ (PatternNumber a) (PatternNumber b)) (PatternNumber (+ a b)))
    (define (- (PatternNumber a) (PatternNumber b)) (PatternNumber (- a b)))
    (define (* (PatternNumber a) (PatternNumber b)) (PatternNumber (* a b)))
    (define (fromInt x) (PatternNumber x)))
  (declare numeric-one? (Num :a => :a -> Boolean))
  (define (numeric-one? x) (match x (1 True) (_ False)))
  (declare nested-numeric-one? (Num :a => Optional :a -> Boolean))
  (define (nested-numeric-one? x) (match x ((Some 1) True) (_ False)))
  (declare delayed-numeric-one? (Num :a => :a -> (Void -> Boolean)))
  (define (delayed-numeric-one? x) (fn () (match x (1 True) (_ False)))))

(define-test test-overloaded-integer-patterns ()
  (is (numeric-one? (the Integer 1)))
  (is (numeric-one? (the IFix 1)))
  (is (numeric-one? (the UFix 1)))
  (is (numeric-one? 1.0))
  (is (numeric-one? 1.0d0))
  (is (numeric-one? (the Fraction 1)))
  (is (numeric-one? (the big-float:Big-Float 1)))
  (is (numeric-one? (PatternNumber 1)))
  (is (not (numeric-one? 2.0d0)))
  (is (not (numeric-one? (PatternNumber 2))))
  (is (nested-numeric-one? (Some 1.0d0)))
  (is (nested-numeric-one? (Some (PatternNumber 1))))
  (is (not (nested-numeric-one? (Some (PatternNumber 2)))))
  (is (not (nested-numeric-one? (the (Optional Integer) None))))
  (is ((delayed-numeric-one? (PatternNumber 1))))
  (is (not ((delayed-numeric-one? (PatternNumber 2)))))
  (let calls = (cell:new 0))
  (is (match (progn (cell:write! calls (+ 1 (cell:read calls))) 2.0d0)
        (1 False) (2 True) (_ False)))
  (is (== 1 (cell:read calls)))
  (let (Some 1) = (Some (PatternNumber 1)))
  ;; Both an inline literal scrutinee and a parameter pattern need conversion.
  (is (match 1.0d0 (1 True) (_ False)))
  (let f = (fn ((Some 1)) True))
  (is (f (Some 1.0d0))))

(coalton-toplevel 
  (repr :enum)
  (derive Eq)
  (define-type MyEnum
    Jalapeno
    Onion
    Lime))

(define-test test-match-on-enum ()
  (let ((declare f (MyEnum -> String))
        (f (fn (x)
             (match x
               ((Jalapeno) "jalapeno")
               ((Onion) "onion")
               (_ "lime?")))))
    (is (== (f Jalapeno)
            "jalapeno"))
    (is (== (f Onion)
            "onion"))
    (is (== (f Lime)
            "lime?")))

  (let ((declare f (MyEnum -> (Optional MyEnum)))
        (f (fn (x)
             (match x
               ((Jalapeno) None)
               ((Onion) None)
               (x (Some x))))))
    (is (== (f Jalapeno)
            None))
    (is (== (f Onion)
            None))
    (is (== (f Lime)
            (Some Lime))))

  (let ((declare f (Ordering -> String))
        (f (fn (x)
             (match x
               ((LT) "lt")
               ((EQ) "eq")
               ((GT) "gt")))))
    (is (== (f LT)
            "lt"))
    (is (== (f EQ)
            "eq"))
    (is (== (f GT)
            "gt"))))

(define-test test-match-on-boolean ()
  (let ((f (fn (x)
             (match x
               ((True) 1)
               ((False) 2)))))
    (is (== (f True)
            1))
    (is (== (f False)
            2)))

  (let ((f (fn (x)
             (match x
               ((True) True)
               (x x)))))
    (is (== (f True)
            True))
    (is (== (f False)
            False)))

  (let ((f (fn (x)
             (match x
               ((True) 1)
               (_ 2)))))
    (is (== (f True)
            1))
    (is (== (f False)
            2))))

(define-test test-match-on-ints ()
  (let ((f (fn (x)
             (match x
               (0 "zero")
               (1 "one")
               (2 "two")
               (_ "error")))))
    (is (== (f 0)
            "zero"))
    (is (== (f 1)
            "one"))
    (is (== (f 2)
            "two"))))

(define-test test-match-on-nums ()

  (let ((f (fn (x)
             (match x
               (0 "zero")
               (1 "one")
               (2 "two")))))
    (is (== (f (the IFix 0))
            "zero"))
    (is (== (f (the U8 1))
            "one"))
    (is (== (f (the I16 2))
            "two"))))

(define-test test-match-lists ()
  (let ((f (fn (xs)
             (match xs
               ((Nil) 0)
               ((Cons x xs) 1)))))
    (is (== (f Nil) 0))
    (is (== (f (make-list 1 2 3)) 1))))

(coalton-toplevel
  (define-type match-foo
    (MFoo Integer)
    (MBar (Tuple Integer Integer))))

(define-test test-match-constructors ()
  (let ((f (fn (x)
             (match x
               ((MFoo n) n)
               ((MBar t) (fst t))))))
    (is (== (f (MFoo 9)) 9))
    (is (== (f (MBar (Tuple 7 8))) 7))))

(define-test test-match-on-fractions ()
  (is (match 1/4
        (1/4 True)
        (_ False))))

(define-test test-match-on-single-floats ()
  (is (match 0.15f0
        (0.15f0 True)
        (_ False))))

(define-test test-match-on-double-floats ()
  (is (match 0.15d0
        (0.15d0 True)
        (_ False))))

(define-test test-match-on-strings ()
  (is (match "red"
        ("red" True)
        (_ False))))

(define-test test-match-on-chars ()
  (is (match #\c
        (#\c True)
        (_ False))))

(coalton-toplevel
 (declare prod-proj-1 (Tuple :a :b -> (Tuple :a (Tuple :a :b))))
 (define (prod-proj-1 (= tpl (Tuple a _))) (Tuple a tpl)))

(define-test test-match-bindings ()
  (let mb = (MBar (Tuple 10 20)))
  (let tpl = (match mb
               ((MFoo n)                   (Tuple n n))
               ((MBar (= tpl (Tuple _ _))) tpl)))
  (is (== tpl (Tuple 10 20)))

  (is (match mb
        ((Mbar (= tpl (Tuple (= a _) (= b _))))
         (== tpl (Tuple a b)))
        (_ False)))

  (is (match (make-list 1 2 3 4 5)
        ;; match cons with 2 in second position
        ;; but bind whole list and tail to vars
        ((= lst (Cons a (= tl (Cons 2 _))))
         (== lst (Cons a tl)))
        (_ False)))

  (let ((declare x (Tuple Integer Integer))
        (x (Tuple 1 2)))
    (is (== (Tuple 1 x) (prod-proj-1 x)))
    (let (Tuple 1 (= tpl (Tuple _ _))) = (prod-proj-1 x))
    (is (== x tpl))))

(coalton-toplevel
  (declare tuple-match-pair (Integer -> (Tuple Integer Integer)))
  (define (tuple-match-pair x)
    (Tuple x (1+ x)))

  (declare tuple-match-bind-var (Integer -> (Tuple Integer Integer)))
  (define (tuple-match-bind-var x)
    (match (tuple-match-pair x)
      (pair (Tuple (snd pair) (fst pair)))))

  (declare tuple-match-bind-constructor (Integer -> (Tuple Integer Integer)))
  (define (tuple-match-bind-constructor x)
    (match (tuple-match-pair x)
      ((Tuple a b) (Tuple b a))))

  (declare tuple-match-wildcard-eligible (Integer -> Integer))
  (define (tuple-match-wildcard-eligible x)
    (match (tuple-match-pair x)
      ((Tuple 0 b) b)
      (_ x))))

(define-test test-tuple-match-var ()
  (is (== (Tuple 11 10)
          (tuple-match-bind-var 10)))
  (is (== (tuple-match-bind-var 42)
          (tuple-match-bind-constructor 42))))

(define-test test-tuple-match-wildcard ()
  (is (== 1
          (tuple-match-wildcard-eligible 0)))
  (is (== 10
          (tuple-match-wildcard-eligible 10)))
  (is (== (tuple-match-wildcard-eligible 42)
          (fst (tuple-match-pair 42)))))

(cl:in-package #:coalton-tests)

(deftest test-superclass-fundep-concrete-instance-methods ()
  (check-coalton-types
   "(define-class (C :a :b (:a -> :b)))
    (define-instance (C Integer String))
    (define-class (C :a :b => D :a)
      (output (:a -> :b))
      (input (:b -> :a))
      (local-id (forall (:b) (:a * :b -> :b))))
    (define-instance (D Integer)
      (define (output _) \"hello\")
      (define (input text) (if (== text \"hello\") 1 0))
      (define (local-id _ value) value))"))

(deftest test-superclass-fundep-renamed-instance-methods ()
  (check-coalton-types
   "(define-class (C :a :b (:a -> :b))
      (base (:a -> :b)))
    (define-class (C :a :b => D :a)
      (output (:a -> :b))
      (same (Eq :b => :a * :b -> Boolean)))
    (define-instance (C (List :x) :y => D (List :x))
      (define (output xs) (base xs))
      (define (same xs value) (== (base xs) value)))"))

(deftest test-superclass-fundep-swapped-instance-methods ()
  (check-coalton-types
   "(define-class (C :a :b (:a -> :b)))
    (define-class (C :a :b => D :a)
      (output (:a -> :b))
      (same (Eq :b => :a * :b -> Boolean)))
    (define-instance (C (Tuple :b :c) :c => D (Tuple :b :c))
      (define (output (Tuple _ c)) c)
      (define (same (Tuple _ c) value) (== c value)))"))

(deftest test-instance-underapplied-value-warns-once ()
  (let ((warnings nil))
    (handler-bind ((source:source-warning
                    (lambda (condition)
                      (push (princ-to-string condition) warnings)
                      (muffle-warning condition))))
      (check-coalton-types
       "(define-class (C :a) (method (:a -> Integer)))
        (define-instance (C Integer)
          (define (method value)
            (fn (other) (+ value other))
            1))"))
    (is (= 1 (count-if (lambda (warning)
                        (search "Value may be underapplied" warning))
                      warnings)))))

(deftest test-runtime-repr-instance-exceptions-check-type-head ()
  (dolist (package-name '("COALTON/LISPARRAY" "COALTON/MATH/COMPLEX"))
    (let ((*package* (find-package package-name)))
      (flet ((check-instance (type)
               (coalton-impl/typechecker/define-instance::check-instance-valid
                (parse-form
                 (format nil "(define-instance (coalton/types:RuntimeRepr ~A))" type)
                 #'coalton-impl/parser/toplevel::parse-define-instance))))
        (is (null (check-instance (if (string= package-name "COALTON/LISPARRAY")
                                      "(LispArray Integer)"
                                      "(Complex Integer)"))))
        (dolist (type '("Integer" "(List Integer)" "(Tuple Integer String)" "(:f Integer)"))
          (signals tc:tc-error (check-instance type)))))))

(deftest gadt-matches-cannot-invent-equalities ()
  (dolist (definition
           '("(declare get-token (Token :a -> :a))
              (define (get-token w) (match w ((MkToken) \"not every type\")))"
             "(declare get-field (Payload -> :b))
              (define (get-field (MkPayload x)) x)"
             "(declare get-field (:a -> String))
              (define (get-field w) (match w ((MkPayload x) x)))"
             "(declare get-field ((Tuple :a Boolean) -> String))
              (define (get-field w) (match w ((Tuple (MkPayload x) _) x)))"))
    (signals tc:tc-error
      (check-coalton-types
       (format nil "(define-type (Token :a) (declare MkToken (Token :a)))
                    (define-type Payload (declare MkPayload (String -> Payload)))
                    ~A" definition)))))

(deftest gadt-existentials-are-distinct-and-scoped ()
  (dolist (definition
           '("(declare apply-packed (Packed -> String))
              (define (apply-packed p) (match p ((MkPacked x f) (f x))))"
             "(declare inspect-packed (Packed -> Boolean))
              (define (inspect-packed (MkPacked (Tuple _ _) _)) True)"
             "(declare use-outside (Packed * (:a -> String) -> String))
              (define (use-outside p f) (match p ((MkPacked x _) (f x))))"
             "(declare get-hidden (Packed -> :a))
              (define (get-hidden p) (match p ((MkPacked x _) (return x))))"
             "(declare get-hidden (Packed -> :a))
              (define (get-hidden p) (let (MkPacked x _) = p) (return x))"
             "(define-exception Trouble (GotPacked Packed))
              (declare recover ((Void -> :a) -> :a))
              (define (recover thunk) (catch (thunk) ((GotPacked (MkPacked x _)) x)))"))
    (signals tc:tc-error
      (check-coalton-types
       (format nil "(define-type Packed (declare MkPacked (:a * (:b -> String) -> Packed)))
                    ~A" definition)))))

(deftest gadt-parameter-must-cover-the-incoming-type ()
  (signals tc:tc-error
    (check-coalton-types
     "(define-type (Tag :a) (declare TagInt (Tag Integer)) (declare TagStr (Tag String)))
      (declare int-only (Tag :a -> Boolean))
      (define (int-only (TagInt)) True)")))

(deftest gadt-binder-evidence-cannot-escape ()
  (dolist (body '("(let (Pack x) = p) x"
                  "(coalton/monad/identity:run-identity (do ((Pack x) <- (pure p)) (pure x)))"
                  "(coalton/monad/identity:run-identity (do (let (Pack x) = p) (pure x)))"))
    (signals tc:tc-error
      (check-coalton-types (format nil "(define-type Packed (declare Pack (:h -> Packed)))
                                        (declare leak (Packed -> :a))
                                        (define (leak p) ~A)" body)))))

(deftest gadt-ascription-must-not-invent-generality ()
  (signals tc:tc-error
    (check-coalton-types
     "(define-type (Witness :a) (declare W (Witness :a)))
      (declare bad (forall (:a) Witness :a -> :a))
      (define (bad w) (the :a (match w ((W) \"not every type\"))))"))
  (signals tc:tc-error
    (check-coalton-types
     "(define-type (Witness :a) (declare W (Witness :a)))
      (declare bad (Witness String -> String))
      (define (bad w) (the (forall (:a) :a) (match w ((W) \"not every type\"))))")))

(deftest gadt-constructor-must-have-a-representable-signature ()
  (dolist (signature '("(Void -> Wrap)" "(Integer -> Void)" "(Integer &key (:tag String) -> Wrap)"))
    (signals parser:parse-error
      (check-coalton-types (format nil "(define-type Wrap (declare MkWrap ~A))" signature)))))

(deftest gadt-givens-do-not-justify-unrelated-types ()
  (dolist (definition
           '("(declare eq-extra (Eq :c => Same :a :b * :a * :c -> Boolean))
              (define (eq-extra w x _) (match w ((Refl) (== x x))))"
             "(declare get-extra (Same :a :b * :a -> :c))
              (define (get-extra w x) (let z = (match w ((Refl) x))) z)"
             "(declare forged (Same :a :b * :a -> String))
              (define (forged w x) (let z = (match w ((Refl) x))) z)"))
    (signals tc:tc-error
      (check-coalton-types (format nil "(define-type (Same :a :b) (declare Refl (Same :x :x))) ~A" definition)))))

(deftest gadt-decomposition-keeps-ordinary-requirements ()
  (let ((witness "(define-type (Proof :p :q :r)
                    (declare Witness (Proof (Optional :x) :y :y)))"))
    ;; The proof establishes :p = Optional :b, not that :b itself is a pair.
    ;; Test both columns and keep the partial branch strictly compile-only.
    (dolist (row '("(match (Tuple w b) ((Tuple (Witness) (Tuple x _)) (if False x other)))"
                   "(match (Tuple b w) ((Tuple (Tuple x _) (Witness)) (if False x other)))"))
      (signals tc:tc-error
        (check-coalton-types
         (format nil "~A (declare bad (Proof :p (Optional :b) :p * :b * :r -> :r))
                      (define (bad w b other) ~A)" witness row))))
    (signals tc:tc-error
      (check-coalton-types
       (format nil "~A (declare bad (Proof :p (Optional :b) :p * :b -> Boolean))
                    (define (bad (Witness) (Tuple flag _)) (the Boolean flag))" witness)))
    (check-coalton-types
     (format nil "~A (declare good (Proof :p (Optional (Tuple :r Unit)) :p * Tuple :r Unit -> :r))
                  (define (good (Witness) (Tuple x (Unit))) x)" witness)))
  ;; The representative of an inner equality can come from an enclosing
  ;; constructor's decomposition, not just from the current constructor.
  (check-coalton-types
   "(define-type (Same :a :b) (declare Refl (Same :a :a)))
    (define-type (Outer :p :b) (declare Mk (Same :b :h * :h -> Outer (List :h) :b)))
    (define (first-value (Mk (Refl) (Tuple x _))) x)"
   '("first-value" . "(Outer :p (Tuple :a :b) -> :a)")))

(deftest gadt-local-results-retain-only-proven-types ()
  (let ((proof "(define-type (Same :a :b) (declare SameProof (Same :x :x)))"))
    (dolist (signature '("(Same :a :b * :a -> Optional :b)" "(Same :a :b * :b -> Optional :a)"))
      (check-coalton-types
       (format nil "~A (declare cast ~A)
                    (define (cast w x)
                      (let ((y (progn (match w ((SameProof) (Some x))))))
                        (let copy = y) copy))" proof signature)))
    ;; Generalizing the postponed component here would let each use of y
    ;; invent an independent result type.
    (signals tc:tc-error
      (check-coalton-types
       (format nil "~A (declare bad (Same :a :b * :a -> Tuple :b String))
                    (define (bad w x) (let ((y (match w ((SameProof) x)))) (Tuple y y)))" proof)))
    (dolist (result '(":c" ":unrelated"))
      (flet ((program ()
               (check-coalton-types
                (format nil "~A (declare compose-cast (Same :a :b * Same :b :c * :a -> ~A))
                             (define (compose-cast p q x)
                               (let y = (match p ((SameProof) (match q ((SameProof) x)))))
                               y)" proof result))))
        (if (string= result ":c") (program) (signals tc:tc-error (program)))))
    ;; Only the equality-dependent part stays monomorphic at a lexical let.
    (check-coalton-types
     (format nil "~A (declare cast (forall (:a :b) Same :a :b * :a -> :b))
                  (define (cast w x)
                    (let ((p (match w ((SameProof) (Tuple x None)))))
                      (let (Tuple y _) = (the (Tuple :b (Optional Boolean)) p))
                      (let _ = (the (Tuple :b (Optional String)) p)) y))" proof))
    (check-coalton-types
     (format nil "~A (define-struct Row (label String))
                  (declare label-of (Same :a :b -> String))
                  (define (label-of w)
                    (let row = (match w ((SameProof) (Row \"value\"))))
                    (let ((get (fn () (.label row)))) (get)))" proof)))
  (check-coalton-types
   "(define-type (Parts :a) (declare Pieces (Parts (Tuple Boolean :h))))
    (declare pass (Parts :a * :a -> Optional :a))
    (define (pass w x) (let p = (match w ((Pieces) (Some x)))) p)")
  (signals tc:tc-error
    (check-coalton-types
     "(define-type (Parts :a) (declare Pieces (:h -> Parts (Tuple Boolean :h))))
      (declare bad (Parts :a -> :a))
      (define (bad w) (let p = (match w ((Pieces h) h))) p)"))
  (let ((indexed "(define-type (Item :i)
                    (declare Flag (Boolean -> Item Boolean))
                    (declare Count (Integer -> Item Integer)))"))
    (check-coalton-types
     (format nil "~A (define (get-item item)
                       (let ((v (match item ((Flag b) b) ((Count n) n)))) v))" indexed)
     '("get-item" . "(Item :i -> :i)"))))

(deftest late-match-scrutinee-specialization ()
  (let ((items "(define-type (Item :a)
                  (declare Flag (Boolean -> Item Boolean))
                  (declare AnyItem (:a -> Item :a)))"))
    (signals tc:tc-error
      (check-coalton-types
       (format nil "~A (define (read-item p)
                         (let n = (match p ((Flag b) b) ((AnyItem _) False)))
                         (let _ = (the (Item String) p)) n)" items)))
    (check-coalton-types
     (format nil "~A (define (read-item p)
                       (let n = (match p ((Flag b) b) ((AnyItem _) False)))
                       (let _ = (the (Item Boolean) p)) n)" items)
     '("read-item" . "(Item Boolean -> Boolean)"))
    ;; Redundancy is a usefulness warning, not a type incompatibility.
    (handler-bind ((source:source-warning #'muffle-warning))
      (check-coalton-types
       (format nil "~A (define (read-item p)
                         (let n = (match p ((Flag b) b) ((Flag _) True) ((AnyItem _) False)))
                         (let _ = (the (Item Boolean) p)) n)" items)))
    (check-coalton-types
     (format nil "~A (define-type (Tag :a)
                       (declare FlagTag (Tag Boolean)) (declare CountTag (Tag Integer)))
                  (declare nested (Tag :a * Item :a -> :a))
                  (define (nested tag item)
                    (match tag
                      ((FlagTag) (match item ((Flag b) b) ((AnyItem b) (not b))))
                      ((CountTag) (match item ((AnyItem n) (1+ n))))))" items))))

(deftest gadt-unsupported-constructor-constraint-diagnostic ()
  (let ((diagnostic (collect-compiler-error
                     "(package gadt-constraint-test (import coalton-prelude))
                      (define-type (Box :a) (declare Pack (Eq :a => :a -> Box :a)))")))
    (is (and diagnostic (search "typeclass constraints on constructor signatures are not supported" diagnostic)))))

(deftest gadt-coverage-still-finds-holes-in-seen-constructors ()
  (let ((diagnostic
          (collect-compiler-error
           "(package correlated-tag-test (import coalton-prelude))
            (define-type (Tag :a) (declare I (Boolean -> Tag Integer)) (declare S (Tag String)))
            (declare classify ((Tuple (Tag :a) (Tag :a)) -> Boolean))
            (define (classify p)
              (match p ((Tuple (I (True)) _) True) ((Tuple _ (S)) False)))")))
    (is (and diagnostic (search "missing case (TUPLE (I (FALSE)) (I \"_\"))" diagnostic)))))

(deftest gadt-coverage-refutes-empty-fields ()
  (flet ((packet (field)
           (format nil
                   "(define-type (Same :a :b) (declare Refl (Same :x :x)))
                    (define-type Recursive (Roll Recursive))
                    (define-type Packet Here (Other ~A))
                    (declare get-packet (Packet -> Boolean))
                    (define (get-packet (Here)) True)" field)))
    (check-coalton-types (packet "(Tuple Boolean (Same Integer String))"))
    ;; A real witness, an opaque type, or an unresolved cycle cannot refute Other.
    (dolist (field '("(Tuple Boolean (Same Integer Integer))" "Integer" "Recursive"))
      (signals tc:tc-error (check-coalton-types (packet field))))))

(defun match-analysis-report (program)
  "Compile PROGRAM, returning the source lines of the useless match cases and
the text of each non-exhaustive match warning."
  (let ((useless nil)
        (missing nil))
    (handler-bind ((source:source-warning
                     (lambda (condition)
                       (let ((text (princ-to-string condition)))
                         (cond
                           ((search "useless match case" text :test #'char-equal)
                            (let* ((arrow (search "-->" text))
                                   (start (1+ (position #\: text :start arrow))))
                              (push (parse-integer text :start start :junk-allowed t) useless)))
                           ((search "non-exhaustive match" text :test #'char-equal)
                            (push text missing))))
                       (muffle-warning condition))))
      (check-coalton-types program))
    (values (sort useless #'<) missing)))

(defun arm-lines (program &rest markers)
  "The source line of each MARKER in PROGRAM."
  (mapcar (lambda (marker)
            (1+ (count #\Newline program :end (search marker program))))
          markers))

(deftest mixed-literal-constructor-gadt-coverage ()
  ;; Literal and constructor patterns share a column when GADT evidence
  ;; from another column fixes its type differently in each row.
  (flet ((check (columns arms useless-markers &optional missing)
           (let ((program
                   (format nil "(define-type (Tag :a) (declare TextTag (Tag String)) (declare UnitTag (Tag Unit)))
(declare classify (~A -> Integer))
(define (classify p)
  (match p~{~%    ~A~}))" columns arms)))
             (multiple-value-bind (useless-lines warnings) (match-analysis-report program)
               (is (equal (apply #'arm-lines program useless-markers) useless-lines))
               (cond
                 (missing
                  (is (= 1 (length warnings)))
                  (is (search missing (first warnings))))
                 (t
                  (is (null warnings))))))))
    (check "(Tuple :a (Tag :a))"
           '("((Tuple \"\" (TextTag)) 1)" "((Tuple (Unit) (UnitTag)) 2)" "((Tuple _ (TextTag)) 3)")
           '())
    (check "(Tuple :a (Tag :a))"
           '("((Tuple (Unit) (UnitTag)) 1)" "((Tuple \"\" (TextTag)) 2)" "((Tuple _ (TextTag)) 3)")
           '())
    (check "(Tuple (Tag :a) :a)"
           '("((Tuple (TextTag) \"\") 1)" "((Tuple (UnitTag) (Unit)) 2)" "((Tuple (TextTag) _) 3)")
           '())
    (check "(Tuple :a (Tag :a))"
           '("((Tuple (Unit) (UnitTag)) 1)" "((Tuple _ (TextTag)) 2)" "((Tuple \"\" (TextTag)) 3)")
           '("((Tuple \"\" (TextTag)) 3)"))
    (check "(Tuple :a (Tag :a))"
           '("((Tuple \"\" (TextTag)) 1)" "((Tuple (Unit) (UnitTag)) 2)")
           '()
           "missing case (TUPLE \"_\" (TEXTTAG))")))

(deftest mixed-literal-constructor-overloaded-coverage ()
  ;; A literal of a type with constructors matches the values that a user
  ;; Eq instance relates to it, which the analysis cannot know. It may
  ;; report a row as useless only if the row is unreachable for every
  ;; such instance, and coverage never relies on a literal.
  (flet ((check (type arms useless-markers &optional missing)
           (let ((program
                   (format nil "(define-type Amount (Amount Integer) Zero)
(define-instance (Eq Amount)
  (define (== a b)
    (match (Tuple a b)
      ((Tuple (Amount x) (Amount y)) (== x y))
      ((Tuple (Zero) (Zero)) True)
      (_ False))))
(define-instance (Num Amount)
  (define (+ _a b) b) (define (- _a b) b) (define (* _a b) b)
  (define (fromInt n) (Amount n)))
(declare classify (~A -> Integer))
(define (classify p)
  (match p~{~%    ~A~}))" type arms)))
             (multiple-value-bind (useless-lines warnings) (match-analysis-report program)
               (is (equal (apply #'arm-lines program useless-markers) useless-lines))
               (cond
                 (missing
                  (is (= 1 (length warnings)))
                  (is (search missing (first warnings))))
                 (t
                  (is (null warnings))))))))
    (check "Amount" '("(5 1)" "((Amount _) 2)" "((Zero) 3)") '())
    (check "Amount" '("((Amount _) 1)" "((Zero) 2)" "(5 3)") '("(5 3)"))
    (check "Amount" '("((Amount 5) 1)" "(5 2)" "(_ 3)") '())
    (check "Amount" '("(5 1)" "(5 2)" "((Amount _) 3)" "((Zero) 4)") '("(5 2)"))
    (check "Amount" '("(5 1)" "((Amount _) 2)") '() "missing case (ZERO)")
    (check "(Optional Amount)"
           '("((Some (Amount _)) 1)" "((Some (Zero)) 2)" "((Some 5) 3)" "((None) 4)")
           '("((Some 5) 3)"))
    (check "(Tuple Amount Boolean)"
           '("((Tuple 5 (True)) 1)" "((Tuple (Amount _) _) 2)" "((Tuple _ (False)) 3)" "((Tuple (Zero) _) 4)")
           '())
    (check "(Tuple Boolean Amount)"
           '("((Tuple _ (Amount _)) 1)" "((Tuple (True) (Zero)) 2)" "((Tuple (True) 5) 3)")
           '("((Tuple (True) 5) 3)")
           "missing case (TUPLE (FALSE) (ZERO))")))

(deftest test-deep-constructor-pattern-analysis ()
  (let* ((specializer 'coalton-impl/analysis/pattern-exhaustiveness::specialize-matrix)
         (original (symbol-function specializer))
         (specializations 0)
         (warnings nil))
    (flet ((nested (inner)
             (dotimes (depth 16 inner)
               (setf inner (format nil "(Layer ~A)" inner)))))
      (unwind-protect
           (progn
             ;; Bound work instead of using a machine-dependent wall-clock timeout.
             (setf (symbol-function specializer)
                   (lambda (matrix pattern)
                     (when (> (incf specializations) 2000)
                       (error "Repeated constructor specialization in pattern analysis"))
                     (funcall original matrix pattern)))
             (handler-bind ((source:source-warning
                             (lambda (condition)
                               (push (princ-to-string condition) warnings)
                               (muffle-warning condition))))
               (let ((type (format nil "(~A -> Integer)" (nested "(Tuple Boolean Boolean)"))))
                 (check-coalton-types
                  (format nil
                          "(define-type (Layer :a) (Layer :a))
                           (declare deep-match ~A)
                           (define (deep-match value)
                             (match value
                               (~A 0)
                               (~A 1)
                               (~A 2)
                               (~A 3)
                               (_ 4)))"
                          type
                          (nested "(Tuple (True) (True))")
                          (nested "(Tuple (True) (False))")
                          (nested "(Tuple (False) (True))")
                          (nested "(Tuple (False) (False))"))
                  (cons "deep-match" type))))
             (is (< specializations 2000))
             (is (= 1 (count-if (lambda (warning)
                                 (search "useless match case" warning))
                               warnings)))
             (is (notany (lambda (warning)
                           (search "non-exhaustive match" warning))
                         warnings)))
        (setf (symbol-function specializer) original)))))
