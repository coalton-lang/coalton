(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

(define-test test-inline-return ()
  ;; See gh #1202
  (is (== 1 (1+
             ((fn (x) (return x)) 0)))))

(coalton-toplevel
  (declare two-arg-double-float-add
     (Double-Float -> Double-Float -> Double-Float))
  (inline)
  (define (two-arg-double-float-add x y)
    (lisp Double-Float (x y) (cl:+ x y))))

(coalton-toplevel
  (define (two-arg-double-float-add-caller x y)
    (two-arg-double-float-add x y))
  (define two-arg-double-float-add-underapply
    (two-arg-double-float-add 3.0d0))
  (define (two-arg-double-float-add-underapply-caller x)
    (two-arg-double-float-add-underapply x)))

(define-test function-inline ()
  (is (== 5.0d0 (two-arg-double-float-add-caller 2.0d0 3.0d0)))
  (is (== 5.0d0 (two-arg-double-float-add-underapply-caller 2.0d0))))

(coalton-toplevel
  (define-class (class-for-inline-method-test :a)
    (method-for-inline-test (:a -> :a -> :a)))
  (define-instance (class-for-inline-method-test Double-Float)
    (inline)
    (define (method-for-inline-test x y)
      (lisp Double-Float (x y) (cl:+ x y)))))

(coalton-toplevel
  (declare method-for-inline-test-caller
    (Double-Float -> Double-Float -> Double-Float))
  (define (method-for-inline-test-caller x y)
    (method-for-inline-test x y)))

(define-test method-inline ()
  (is (== 5.0d0 (method-for-inline-test-caller 2.0d0 3.0d0))))

(coalton-toplevel
  (inline)
  (define (generic-for-inline x y)
    (+ x y))

  (declare num-generic-for-inline (Num :a => :a -> :a))
  (inline)
  (define num-generic-for-inline
    (generic-for-inline 2)))

(coalton-toplevel
  (monomorphize)
  (declare monomorph-for-inline (Integer -> Integer))
  (define (monomorph-for-inline y)
    (num-generic-for-inline y)))

(coalton-toplevel
  (inline)
  (define (test-fact n)
    (if (== 0 n)
        1
        (test-fact (1- n)))))

(coalton-toplevel
  (define (test-fact-caller)
    (test-fact 10)))

(coalton-toplevel
  (define-class (RecursiveInlineTestClass :a)
    (test-fact-method (:a -> :a)))

  (define-instance (RecursiveInlineTestClass Integer)
    (inline)
    (define (test-fact-method n)
      (if (== 0 n)
          1
          (test-fact-method (1- n))))))

(coalton-toplevel
  (monomorphize)
  (define (test-fact-method-caller)
    (test-fact-method (the Integer 10))))


(define-test monomorphize-inline ()
  (is (== 5 (monomorph-for-inline 3))))

;; Issue on redefining inlinable functions
;; https://github.com/coalton-lang/coalton/issues/1499
;; These functions are used by inliner-tests-1.lisp, which is
;; loaded after this file.

(coalton-toplevel
  (inline)
  (define (test-inlinable-rec-1 n)
    (if (== n 0)
        111
        (test-inlinable-rec-2 (1- n))))
  (inline)
  (define (test-inlinable-rec-2 n)
    (if (== n 0)
        222
        (test-inlinable-rec-1 (1- n)))))


(in-package #:coalton-tests)

;; See gh #1293
(deftest test-inliner-rename-bound-variables ()
  (check-coalton-types
   "(declare f (Integer -> Integer))
    (define (f n)
      (when (== n 0)
        (return 0))
      (when (== n 1)
        (return 1))
      (+ (f (- n 1))
         (f (- n 2))))"))

(deftest function-inline-error ()
  (signals coalton-impl/parser:parse-error
    (check-coalton-types
     "(inline myfun)
      (define (myfun x) x)")))

(deftest method-inline-error ()
  (signals coalton-impl/parser:parse-error
    (check-coalton-types
     "(define-class (C :a)
        (m (:a -> :a -> :a)))
      (define-instance (m Double-Float)
        (inline m)
        (define (m x y)
          (lisp Double-Float (x y) (cl:+ x y))))")))

(deftest function-inline-code ()
  (is (equal '((cl:+ coalton-native-tests::x coalton-native-tests::y))
             (ast:node-lisp-form
              (ast:node-let-subexpr
               (ast:node-abstraction-subexpr
                (coalton:lookup-code
                 'coalton-native-tests::two-arg-double-float-add-caller)))))))

(deftest method-inline-code ()
  (is (equal '((cl:+ coalton-native-tests::x coalton-native-tests::y))
             (ast:node-lisp-form
              (ast:node-let-subexpr
               (ast:node-abstraction-subexpr
                (coalton:lookup-code
                 'coalton-native-tests::method-for-inline-test-caller)))))))

(deftest recursive-inline-test ()
  (check-coalton-types
   "(inline)
    (define (factorial-1 n)
      (if (== n 0)
          1
          (* n (factorial-2 (- n 1)))))
    (inline)
    (define (factorial-2 n)
      (if (== n 0)
          1
          (* n (factorial-1 (- n 1)))))"))

(deftest limit-unroll-test ()
  "Ensure that we get to a locally node,
deem the AST fully unrolled, and stop inlining.

Also ensure that running the inliner again does not further
unroll the node."
  (labels ((abstraction-second-branch (node)
             (second
              (ast:node-match-branches
               (ast:node-let-subexpr
                (ast:node-abstraction-subexpr
                 node)))))
           (branch-second-branch (node)
             (second
              (ast:node-match-branches
               (ast:node-let-subexpr
                (ast:match-branch-body
                 node)))))
           (fact-to-locally (node)
             (ast:match-branch-body
              (branch-second-branch
               (abstraction-second-branch
                node)))))
    (let ((locally-node-1
            (fact-to-locally
             (coalton:lookup-code
              'coalton-native-tests::test-fact-caller)))
          ;; Same node, but inlined again
          (locally-node-2
            (coalton-impl/codegen/inliner:inline-applications
             (fact-to-locally
              (coalton:lookup-code
               'coalton-native-tests::test-fact-caller))
             entry:*global-environment*)))
      ;; Make sure a node-locally was emitted.
      (is (typep locally-node-1 'ast:node-locally))
      ;; Check that it stops the recursion.
      (is (member 'coalton-native-tests::test-fact
                  (ast:node-locally-noinline-functions
                   locally-node-1)))

      ;; Make sure a node-locally was emitted.
      (is (typep locally-node-2 'ast:node-locally))
      ;; Check that it stops the recursion.
      (is (member 'coalton-native-tests::test-fact
                  (ast:node-locally-noinline-functions
                   locally-node-2)))

      ;; Inlining again doesn't add any more nodes to the AST.
      (is (= (traverse:count-nodes locally-node-1)
             (traverse:count-nodes locally-node-2))))))

(deftest limit-unroll-method-test ()
  "Ensure that methods don't keep recursively inlining."
  (let* ((caller-1
           (coalton:lookup-code
            'coalton-native-tests::test-fact-method-caller))
         (caller-2
           (coalton-impl/codegen/inliner:inline-applications
            caller-1
            entry:*global-environment*))
         (caller-3
           (coalton-impl/codegen/inliner:inline-applications
            caller-2
            entry:*global-environment*)))
    (is (= (traverse:count-nodes caller-1)
           (traverse:count-nodes caller-2)
           (traverse:count-nodes caller-3)))))
