(in-package #:coalton-tests)

(deftest test-typed-tree-traversals ()
  (let ((type (tc:qualify nil tc:*integer-type*))
        (loc (source:make-location (source:make-source-string "x") '(0 . 1))))
    (flet ((var (name) (tc:make-node-variable :name name :type type :location loc)))
      (let* ((name (var 'bound))
             (operator (var 'f))
             (capture (var 'x))
             (forms '((+ x x)))
             (lisp-node (tc:make-node-lisp :type type :location loc
                                           :vars (list capture) :var-names '(x) :body forms))
             (option (var 'y))
             (arg (tc:make-node-application-keyword-arg :keyword :option :value option))
             (call (tc:make-node-application :type type :location loc :rator operator
                                             :rands (list lisp-node) :keyword-rands (list arg)))
             (binding (tc:make-node-let-binding :name name :value call :location loc))
             (visited nil))
        (flet ((record (node) (push node visited)))
          (let ((block (tc:make-traverse-block :variable #'record :lisp #'record
                                               :application #'record :let-binding #'record)))
            (is (null (multiple-value-list (tc:visit binding block))))))
        ;; Original nodes in postorder. The captured variable is a use; the
        ;; binding name, Lisp forms and keyword name are not.
        (is (equal (nreverse visited) (list operator capture lisp-node option call binding)))

        (let* ((replacement (var 'new))
               (rewritten
                 (tc:traverse
                  binding
                  (tc:make-traverse-block
                   :variable (constantly replacement)
                   :application
                   (lambda (node)
                     (let ((lisp-node (first (tc:node-application-rands node))))
                       (is (eq replacement (tc:node-application-rator node)))
                       (is (equal (list replacement) (tc:node-lisp-vars lisp-node)))
                       (is (eq forms (tc:node-lisp-body lisp-node))))
                     ;; A parent can consume a rewritten child and change node kind.
                     (tc:node-application-keyword-arg-value
                      (first (tc:node-application-keyword-rands node))))))))
          (is (eq name (tc:node-let-binding-name rewritten)))
          (is (eq replacement (tc:node-let-binding-value rewritten)))
          (is (eq call (tc:node-let-binding-value binding))))))))

(deftest test-typed-analysis-warning-order ()
  (let ((warnings nil))
    (handler-bind ((source:source-warning
                     (lambda (condition)
                       (push (source:message condition) warnings)
                       (muffle-warning condition))))
      (check-coalton-types
       "(declare f (Integer -> Integer))
        (define (f unused)
          (the (Result String Integer) (Ok 1))
          (the (Integer -> Integer) (fn (x) x))
          (match True ((True) 1)))
        (define-class (C :a) (method (:a -> Integer)))
        (define-instance (C Integer)
          (define (method unused)
            (the (Integer -> Integer) (fn (x) x))
            1))"))
    ;; Analyze each definition one pass at a time; unused variables are not
    ;; diagnosed in instance methods.
    (is (equal (nreverse warnings)
               '("non-exhaustive match" "Unused variable" "Value may be underapplied"
                 "Discarded Result" "Value may be underapplied")))))
