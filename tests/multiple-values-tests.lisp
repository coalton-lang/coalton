(in-package #:coalton-tests)

(uiop:define-package #:coalton-tests/multiple-values
  (:use #:coalton #:coalton-prelude)
  (:export
   #:mv-return-tuple
   #:mv-id-tuple
   #:mv-consume-tuple
   #:mv-lisp-return-tuple
   #:mv-lisp-alias-return
   #:mv-escape-global
   #:mv-project-fst-global
   #:mv-project-snd-global
   #:mv-join-point-global
   #:mv-wildcard-match-global
   #:mv-lisp-transitive-global
   #:mv-transitive-global
   #:mv-transitive-lambda))

(defun coalton-tests/multiple-values::%mv-lisp-return-helper (x)
  (declare (type integer x)
           (values integer integer &optional))
  (cl:values x (cl:1+ x)))

(with-coalton-compilation (:package #:coalton-tests/multiple-values)
  (coalton-toplevel
    (declare mv-return-tuple (Integer -> (Tuple Integer Integer)))
    (define (mv-return-tuple x)
      (Tuple x (1+ x)))

    (declare mv-id-tuple ((Tuple Integer Integer) -> (Tuple Integer Integer)))
    (define (mv-id-tuple pair)
      pair)

    (declare mv-consume-tuple ((Tuple Integer Integer) -> Integer))
    (define (mv-consume-tuple pair)
      (match pair
        ((Tuple a b) (+ a b))))

    (declare mv-lisp-return-tuple (Integer -> (Tuple Integer Integer)))
    (define (mv-lisp-return-tuple x)
      (lisp multiple-values (Tuple Integer Integer) (x)
        (%mv-lisp-return-helper x)))

    (define-type-alias IntPair (Tuple Integer Integer))

    (declare mv-lisp-alias-return (Integer -> IntPair))
    (define (mv-lisp-alias-return x)
      (lisp multiple-values IntPair (x)
        (%mv-lisp-return-helper x)))

    (declare mv-escape-global (Integer -> (Tuple Integer Integer)))
    (define (mv-escape-global x)
      (mv-id-tuple (mv-return-tuple x)))

    (declare mv-project-fst-global (Integer -> Integer))
    (define (mv-project-fst-global x)
      (fst (mv-id-tuple (mv-return-tuple x))))

    (declare mv-project-snd-global (Integer -> Integer))
    (define (mv-project-snd-global x)
      (snd (mv-id-tuple (mv-return-tuple x))))

    (declare mv-join-point-global (Integer -> Integer))
    (define (mv-join-point-global x)
      (fst
       (match (mv-return-tuple x)
         ((Tuple 0 b) (mv-id-tuple (mv-return-tuple b)))
         ((Tuple a _) (mv-id-tuple (mv-return-tuple a))))))

    (declare mv-wildcard-match-global (Integer -> Integer))
    (define (mv-wildcard-match-global x)
      (match (mv-return-tuple x)
        ((Tuple 0 b) b)
        (_ x)))

    (declare mv-lisp-transitive-global (Integer -> Integer))
    (define (mv-lisp-transitive-global x)
      (mv-consume-tuple (mv-id-tuple (mv-lisp-return-tuple x))))

    (declare mv-transitive-global (Integer -> Integer))
    (define (mv-transitive-global x)
      (mv-consume-tuple (mv-id-tuple (mv-return-tuple x))))

    (declare mv-transitive-lambda (Integer -> Integer))
    (define (mv-transitive-lambda x)
      (let make-pair = (fn (n) (Tuple n (1+ n))))
      (let pass = (fn (pair) pair))
      (let consume = (fn (pair)
                       (match pair
                         ((Tuple a b) (+ a b)))))
      (consume (pass (make-pair x))))))

(defun %mv-symbol (name)
  (declare (type simple-string name)
           (values symbol &optional))
  (or (find-symbol name '#:coalton-tests/multiple-values)
      (error "Unable to resolve symbol ~A in COALTON-TESTS/MULTIPLE-VALUES." name)))

(defun %count-direct-calls-if (node predicate)
  (declare (type ast:node node)
           (type function predicate)
           (values fixnum &optional))
  (let ((count 0))
    (traverse:traverse
     node
     (list
      (traverse:action (:after ast:node-direct-application app)
        (when (funcall predicate (ast:node-direct-application-rator app))
          (incf count))
        (values))))
    count))

(defun %count-lisp-nodes-if (node predicate)
  (declare (type ast:node node)
           (type function predicate)
           (values fixnum &optional))
  (let ((count 0))
    (traverse:traverse
     node
     (list
      (traverse:action (:after ast:node-lisp lisp-node)
        (when (funcall predicate lisp-node)
          (incf count))
        (values))))
    count))

(defun %strip-leading-binds (node)
  (declare (type ast:node node)
           (values ast:node &optional))
  (loop :while (typep node 'ast:node-bind)
        :do (setf node (ast:node-bind-body node))
        :finally (return node)))

(defun %values-rator-p (rator)
  (declare (type symbol rator)
           (values boolean &optional))
  (and (symbolp rator)
       (not (null (search "%VALUES" (symbol-name rator) :test #'char=)))))

;; Checks transitive tuple producer/identity/consumer composition
;; preserves behavior across global and local paths.
(deftest tuple-multiple-values-transitive-runtime ()
  (is (= 21
         (eval '(coalton:coalton
                 (coalton-tests/multiple-values:mv-transitive-global 10)))))
  (is (= 21
         (eval '(coalton:coalton
                 (coalton-tests/multiple-values:mv-transitive-lambda 10))))))

;; Checks escape-path results are returned as a single Lisp value and
;; remain projection-correct.
(deftest tuple-multiple-values-escape-runtime ()
  (is (= 1
         (length
          (multiple-value-list
           (eval '(coalton:coalton
                   (coalton-tests/multiple-values:mv-escape-global 10)))))))
  (is (= 10
         (eval '(coalton:coalton
                 (coalton-library/tuple:fst
                  (coalton-tests/multiple-values:mv-escape-global 10))))))
  (is (= 11
         (eval '(coalton:coalton
                 (coalton-library/tuple:snd
                  (coalton-tests/multiple-values:mv-escape-global 10)))))))

;; Checks `lisp multiple-values` tuple producers compose correctly with
;; projections and tuple consumers.
(deftest tuple-multiple-values-lisp-values-runtime ()
  (is (= 10
         (eval '(coalton:coalton
                 (coalton-library/tuple:fst
                  (coalton-tests/multiple-values:mv-lisp-return-tuple 10))))))
  (is (= 11
         (eval '(coalton:coalton
                 (coalton-library/tuple:snd
                  (coalton-tests/multiple-values:mv-lisp-return-tuple 10))))))
  (is (= 21
         (eval '(coalton:coalton
                 (coalton-tests/multiple-values:mv-lisp-transitive-global 10))))))

;; Checks alias-typed `lisp multiple-values` tuple producers preserve
;; projection semantics.
(deftest tuple-multiple-values-lisp-values-alias-runtime ()
  (is (= 10
         (eval '(coalton:coalton
                 (coalton-library/tuple:fst
                  (coalton-tests/multiple-values:mv-lisp-alias-return 10))))))
  (is (= 11
         (eval '(coalton:coalton
                 (coalton-library/tuple:snd
                  (coalton-tests/multiple-values:mv-lisp-alias-return 10)))))))

;; Checks projection helpers preserve first/second component semantics
;; through tuple pipelines.
(deftest tuple-multiple-values-projection-runtime ()
  (is (= 10
         (eval '(coalton:coalton
                 (coalton-tests/multiple-values:mv-project-fst-global 10)))))
  (is (= 11
         (eval '(coalton:coalton
                 (coalton-tests/multiple-values:mv-project-snd-global 10))))))

;; Checks join-point control flow over tuple results preserves runtime
;; behavior.
(deftest tuple-multiple-values-join-point-runtime ()
  (is (= 1
         (eval '(coalton:coalton
                 (coalton-tests/multiple-values:mv-join-point-global 0)))))
  (is (= 10
         (eval '(coalton:coalton
                 (coalton-tests/multiple-values:mv-join-point-global 10))))))

;; Checks wildcard tuple matching preserves semantics for specific and
;; fallback branches.
(deftest tuple-multiple-values-wildcard-match-runtime ()
  (is (= 1
         (eval '(coalton:coalton
                 (coalton-tests/multiple-values:mv-wildcard-match-global 0)))))
  (is (= 10
         (eval '(coalton:coalton
                 (coalton-tests/multiple-values:mv-wildcard-match-global 10))))))

;; Checks global transitive codegen uses `%values` entry points and
;; avoids direct boxed wrapper/tuple constructor calls.
(deftest tuple-multiple-values-transitive-global-codegen ()
  (let* ((node (coalton:lookup-code 'coalton-tests/multiple-values::mv-transitive-global))
         (body (%strip-leading-binds (ast:node-abstraction-subexpr node)))
         (values-call-count (%count-direct-calls-if body #'%values-rator-p))
         (boxed-call-count (%count-direct-calls-if
                            body
                            (lambda (rator)
                              (member rator
                                      (list (%mv-symbol "MV-RETURN-TUPLE")
                                            (%mv-symbol "MV-ID-TUPLE")
                                            (%mv-symbol "MV-CONSUME-TUPLE"))
                                      :test #'eq))))
         (tuple-box-count (%count-direct-calls-if
                           body
                           (lambda (rator)
                             (eq rator 'coalton-library/classes:tuple)))))
    (is (>= values-call-count 3))
    (is (= 0 boxed-call-count))
    (is (= 0 tuple-box-count))))

;; Checks local-lambda transitive codegen uses `%values` and does not
;; emit tuple constructors.
(deftest tuple-multiple-values-transitive-lambda-codegen ()
  (let* ((node (coalton:lookup-code 'coalton-tests/multiple-values::mv-transitive-lambda))
         (body (%strip-leading-binds (ast:node-abstraction-subexpr node)))
         (values-call-count (%count-direct-calls-if body #'%values-rator-p))
         (tuple-box-count (%count-direct-calls-if
                           body
                           (lambda (rator)
                             (eq rator 'coalton-library/classes:tuple)))))
    (is (>= values-call-count 3))
    (is (= 0 tuple-box-count))))

;; Checks `lisp multiple-values` transitive codegen uses `%values` entry points
;; without boxed wrapper/tuple constructor calls.
(deftest tuple-multiple-values-lisp-values-codegen ()
  (let* ((node (coalton:lookup-code 'coalton-tests/multiple-values::mv-lisp-transitive-global))
         (body (%strip-leading-binds (ast:node-abstraction-subexpr node)))
         (values-call-count (%count-direct-calls-if body #'%values-rator-p))
         (boxed-call-count (%count-direct-calls-if
                            body
                            (lambda (rator)
                              (member rator
                                      (list (%mv-symbol "MV-LISP-RETURN-TUPLE")
                                            (%mv-symbol "MV-ID-TUPLE")
                                            (%mv-symbol "MV-CONSUME-TUPLE"))
                                      :test #'eq))))
         (tuple-box-count (%count-direct-calls-if
                           body
                           (lambda (rator)
                             (eq rator 'coalton-library/classes:tuple)))))
    (is (>= values-call-count 3))
    (is (= 0 boxed-call-count))
    (is (= 0 tuple-box-count))))

;; Checks escape-path codegen still emits tuple construction when
;; values must materialize.
(deftest tuple-multiple-values-escape-boxes ()
  (let* ((node (coalton:lookup-code 'coalton-tests/multiple-values::mv-escape-global))
         (body (%strip-leading-binds (ast:node-abstraction-subexpr node)))
         (tuple-box-count (%count-direct-calls-if
                           body
                           (lambda (rator)
                             (eq rator 'coalton-library/classes:tuple)))))
    (is (>= tuple-box-count 1))))

;; Checks projection-path codegen uses `%values` and avoids direct
;; boxed wrapper/tuple constructor calls.
(deftest tuple-multiple-values-projection-codegen ()
  (let* ((node (coalton:lookup-code 'coalton-tests/multiple-values::mv-project-fst-global))
         (body (%strip-leading-binds (ast:node-abstraction-subexpr node)))
         (values-call-count (%count-direct-calls-if body #'%values-rator-p))
         (boxed-call-count (%count-direct-calls-if
                            body
                            (lambda (rator)
                              (member rator
                                      (list (%mv-symbol "MV-RETURN-TUPLE")
                                            (%mv-symbol "MV-ID-TUPLE"))
                                      :test #'eq))))
         (tuple-box-count (%count-direct-calls-if
                           body
                           (lambda (rator)
                             (eq rator 'coalton-library/classes:tuple)))))
    (is (>= values-call-count 1))
    (is (= 0 boxed-call-count))
    (is (= 0 tuple-box-count))))

;; Checks join-point codegen uses `%values` and avoids direct boxed
;; wrapper/tuple constructor calls.
(deftest tuple-multiple-values-join-point-codegen ()
  (let* ((node (coalton:lookup-code 'coalton-tests/multiple-values::mv-join-point-global))
         (body (%strip-leading-binds (ast:node-abstraction-subexpr node)))
         (values-call-count (%count-direct-calls-if body #'%values-rator-p))
         (boxed-call-count (%count-direct-calls-if
                            body
                            (lambda (rator)
                              (member rator
                                      (list (%mv-symbol "MV-RETURN-TUPLE")
                                            (%mv-symbol "MV-ID-TUPLE"))
                                      :test #'eq))))
         (tuple-box-count (%count-direct-calls-if
                           body
                           (lambda (rator)
                             (eq rator 'coalton-library/classes:tuple)))))
    (is (>= values-call-count 1))
    (is (= 0 boxed-call-count))
    (is (= 0 tuple-box-count))))

;; Checks wildcard-match codegen uses `%values` and avoids direct
;; boxed producer/tuple constructor calls.
(deftest tuple-multiple-values-wildcard-match-codegen ()
  (let* ((node (coalton:lookup-code 'coalton-tests/multiple-values::mv-wildcard-match-global))
         (body (%strip-leading-binds (ast:node-abstraction-subexpr node)))
         (values-call-count (%count-direct-calls-if body #'%values-rator-p))
         (boxed-call-count (%count-direct-calls-if
                            body
                            (lambda (rator)
                              (eq rator (%mv-symbol "MV-RETURN-TUPLE")))))
         (tuple-box-count (%count-direct-calls-if
                           body
                           (lambda (rator)
                             (eq rator 'coalton-library/classes:tuple)))))
    (is (>= values-call-count 1))
    (is (= 0 boxed-call-count))
    (is (= 0 tuple-box-count))))

;; Checks `%values` entry point for `mv-lisp-return-tuple` is tagged
;; with the `:values` return convention.
(deftest tuple-multiple-values-lisp-values-unboxed-entrypoint ()
  (let* ((node (coalton:lookup-code 'coalton-tests/multiple-values::mv-lisp-return-tuple%values))
         (lisp-values-count
            (%count-lisp-nodes-if
             (ast:node-abstraction-subexpr node)
             (lambda (lisp-node)
              (eq ':values (ast:node-lisp-return-convention lisp-node))))))
    (is (= 1 lisp-values-count))))

;; Checks alias-typed `%values` entry point is tagged with the
;; `:values` return convention.
(deftest tuple-multiple-values-lisp-values-alias-unboxed-entrypoint ()
  (let* ((node (coalton:lookup-code 'coalton-tests/multiple-values::mv-lisp-alias-return%values))
         (lisp-values-count
            (%count-lisp-nodes-if
             (ast:node-abstraction-subexpr node)
             (lambda (lisp-node)
              (eq ':values (ast:node-lisp-return-convention lisp-node))))))
    (is (= 1 lisp-values-count))))
