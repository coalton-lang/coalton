(in-package #:coalton-tests)

;; Test that error messages containing source spans are correctly
;; printed.

(defvar *error-test-program*
  "  ;;
  ;; Kinds
  ;;

  (define-type Kind
    Star
    (Kfun Kind Kind))

  (define-instance (Eq Kind)
    (define (== k1 k2)
      (match (Tuple k1 k2)
        ((Tuple (Star) (Star)) True)
        ((Tuple (Kfun a1 a2)
                (Kfun b1 b2))
         (and (== a1 b1)
              (== a2 b2)))
        (_ False))))
")

(deftest test-error ()
  (let* ((source (source:make-source-string *error-test-program* :name "file"))
         (msg (with-output-to-string (output)
                ;; an annotating error
                (handler-case
                    (source:error "message"
                                  (source:note (source:make-location source '(76 . 321))
                                               "define instance form")
                                  (source:secondary-note (source:make-location source '(132 . 319))
                                                         "message 2")
                                  (source:secondary-note (source:make-location source '(140 . 145))
                                                         "message 3")
                                  (source:secondary-note (source:make-location source '(170 . 174))
                                                         "message 4")
                                  (source:help (source:make-location source '(289 . 291))
                                               (lambda (existing)
                                                 (concatenate 'string "*" existing "*"))
                                               "message 5"))
                  (source:source-error (c)
                    (princ c output))))))
    ;; output text
    (is (check-string= "error printer"
                       msg
                       "error: message
  --> file:9:2
    |
 9  |      (define-instance (Eq Kind)
    |  ____^
 10 | |      (define (== k1 k2)
 11 | |        (match (Tuple k1 k2)
    | | _______-
    | ||               ----- message 3
 12 | ||         ((Tuple (Star) (Star)) True)
    | ||                  ---- message 4
 13 | ||         ((Tuple (Kfun a1 a2)
 ...
 16 | ||               (== a2 b2)))
 17 | ||         (_ False))))
    | ||__________________- message 2
    | |_____________________^ define instance form
help: message 5
 16 |               (*==* a2 b2)))
    |                ----
"))))

(deftest local-binding-validation-error-priority ()
  (dolist (context '("let" "let*" "rec go" "for" "for*"))
    (flet ((diagnostic (bindings)
             (collect-compiler-error
              (format nil "(package coalton-test-binding-errors
  (import coalton-prelude))
(define (bad)
  (~A (~A) x))"
                      context bindings))))
      (let ((message (diagnostic
                      (if (member context '("for" "for*") :test #'string=)
                          "(declare missing Integer) (x 1 x) (x 2 x)"
                          "(declare missing Integer) (x 1) (x 2)"))))
        (is (search (cond
                      ((string= context "let") "Duplicate definition in let")
                      ((string= context "let*") "Duplicate binding in let*")
                      ((string= context "rec go") "Duplicate definition in rec")
                      ((string= context "for*") "Duplicate binding in for*")
                      (t "Duplicate binding in loop"))
                    message))
        (is (search (if (member context '("let*" "for*") :test #'string=)
                        "first binding here"
                        "first definition here")
                    message))
        (is (search (if (member context '("let*" "for*") :test #'string=)
                        "second binding here"
                        "second definition here")
                    message)))
      (let ((message (diagnostic
                      (if (member context '("for" "for*") :test #'string=)
                          "(declare missing Integer) (declare missing String) (x 1 x)"
                          "(declare missing Integer) (declare missing String) (x 1)"))))
        (is (search (cond
                      ((member context '("let" "let*") :test #'string=)
                       "Duplicate declaration in let")
                      ((string= context "rec go") "Duplicate declaration in rec")
                      (t "Duplicate declaration in loop"))
                    message)))
      (let ((message (diagnostic
                      (if (member context '("for" "for*") :test #'string=)
                          "(declare missing Integer) (x 1 x)"
                          "(declare missing Integer) (x 1)"))))
        (is (search (cond
                      ((member context '("let" "let*") :test #'string=)
                       "Orphan declare in let")
                      ((string= context "rec go") "Orphan declare in rec")
                      (t "Orphan declare in loop"))
                    message))
        (is (search (if (member context '("for" "for*") :test #'string=)
                        "declaration does not have an associated loop binding"
                        "declaration does not have an associated definition")
                    message))))))

(deftest repeated-type-constructors-report-the-correct-span ()
  (check-string=
   "issue 916"
   (collect-compiler-error
    "(package issue916
  (import coalton-prelude))

(define-class (C :a)
  (cf (:a -> Tuple Integer Integer -> List Tuple Integer Integer)))")
   "error: Kind mismatch
  --> test:5:43
   |
 5 |    (cf (:a -> Tuple Integer Integer -> List Tuple Integer Integer)))
   |                                             ^^^^^ Expected kind '*' but got kind '* → (* → *)'"))

(deftest explicit-forall-rejects-unbound-type-variables ()
  (let ((msg (collect-compiler-error
              "(package coalton-test-explicit-forall-errors)

(declare bad (forall (:a) (:a -> :b)))

(define bad
  (fn (x)
    x))")))
    (is (search "Unknown type variable :B" msg))
    (is (search "(declare bad (forall (:a) (:a -> :b)))" msg))))

(deftest values-binder-diagnostics ()
  (let ((msg (collect-compiler-error
              "(package coalton-test-values-errors)
               (declare bad (forall ((:r Values)) (:r -> :r)))
               (define (bad x) x)")))
    (is (search "Invalid Values variable" msg))
    (is (search "Values variable :R must occupy an entire" msg)))
  (let ((msg (collect-compiler-error
              "(package coalton-test-values-errors)
               (declare bad (forall ((:f (Type -> Values))) (Void -> Integer)))
               (define (bad) 1)")))
    (is (search "Unsupported kind" msg))
    (is (search "Values cannot occur inside an arrow kind" msg))))

(deftest explicit-forall-warns-on-unused-type-variables ()
  (let ((msg (collect-compiler-error
              "(package coalton-test-explicit-forall-errors)

(declare good (forall (:a :b) (:a -> :a)))

(define good
  (fn (x)
    x))")))
    (is (search "Unused quantified type variable" msg))
    (is (search "quantified type variable :B is not used in the declared type" msg))))

(deftest explicit-forall-rejects-duplicate-type-variables ()
  (let ((msg (collect-compiler-error
              "(package coalton-test-explicit-forall-errors)

(declare bad (forall (:a :a) (:a -> :a)))

(define bad
  (fn (x)
    x))")))
    (is (search "Duplicate quantified type variable" msg))
    (is (search "first binding here" msg))
    (is (search "second binding here" msg))))

(deftest nested-forall-rejects-duplicate-type-variables ()
  (let ((msg (collect-compiler-error
              "(package coalton-test-explicit-forall-errors)

(declare bad (forall (:a) (forall (:a) (:a -> :a))))

(define bad
  (fn (x)
    x))")))
    (is (search "Duplicate quantified type variable" msg))
    (is (search "first binding here" msg))
    (is (search "second binding here" msg))))

(deftest implicit-outer-declarations-do-not-create-scoped-type-variables ()
  (let ((msg (collect-compiler-error
              "(package coalton-test-scoped-forall-errors)

(declare bad (:a -> :a))

(define (bad x)
  (let ((declare keep-item (forall (:b) :a -> :b -> :a))
        (keep-item (fn (y _z) y)))
    (keep-item x Unit)))")))
    (is (search "Unknown type variable :A" msg))))

(deftest implicit-outer-declarations-do-not-create-scoped-type-variables-via-proxy ()
  (let ((msg (collect-compiler-error
              "(package coalton-test-scoped-forall-errors)

(declare bad (:item -> :item))

(define (bad x)
  (let ((declare reify (coalton/types:Proxy :item -> :item))
    (reify (fn (p)
                 (coalton/types:as-proxy-of x p))))
    (reify (coalton/types:proxy-of x))))")))
    (is (search "Declared type is too general" msg))
    (is (search "Proxy" msg))
    (is (search "more general than inferred type" msg))))

(deftest implicit-class-method-binders-do-not-create-scoped-type-variables ()
  (let ((msg (collect-compiler-error
              "(package coalton-test-scoped-forall-errors)

(define-type (ScopedMethodWrap :f :a)
  (ScopedMethodWrap (:f :a)))

(define-class (ScopedMethodClass :wrapper)
  (bad-scoped-method
    ((:wrapper :item) * (coalton/types:Proxy :item) -> (:wrapper :item))))

(define-instance (ScopedMethodClass (ScopedMethodWrap :f))
  (define (bad-scoped-method wrapped proxy)
    (match wrapped
      ((ScopedMethodWrap inner)
       (let ((declare rebuild
                     (forall (:ignored)
                       (coalton/types:Proxy :ignored) * (:f :item)
                       -> (ScopedMethodWrap :f :item)))
             (rebuild (fn (_other value)
                        (ScopedMethodWrap (the (:f :item) value)))))
         (rebuild proxy inner))))))")))
    (is (search "Unknown type variable :ITEM" msg))))

(deftest explicit-class-method-forall-rejects-duplicate-type-variables ()
  (let ((msg (collect-compiler-error
              "(package coalton-test-scoped-forall-errors)

(define-class (BadScopedMethodClass :wrapper)
  (bad-scoped-method
    (forall (:item :item)
      ((:wrapper :item) -> (coalton/types:Proxy :item) -> (:wrapper :item)))))")))
    (is (search "Duplicate quantified type variable" msg))
    (is (search "first binding here" msg))
    (is (search "second binding here" msg))))

(deftest explicit-class-method-forall-rejects-unbound-type-variables ()
  (let ((msg (collect-compiler-error
              "(package coalton-test-scoped-forall-errors)

(define-class (BadScopedMethodClass :wrapper)
  (bad-scoped-method
    (forall (:item)
      ((:wrapper :item) -> (coalton/types:Proxy :missing) -> (:wrapper :item)))))")))
    (is (search "Unknown type variable :MISSING" msg))))
