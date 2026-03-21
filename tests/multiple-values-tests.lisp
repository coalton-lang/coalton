(in-package #:coalton-tests)

(uiop:define-package #:coalton-tests/multiple-values
  (:use #:coalton #:coalton-prelude)
  (:export
   #:mv-return-values
   #:mv-consume-values
   #:mv-lisp-return-values
   #:mv-lisp-consume-values
   #:mv-when-zero-values
   #:mv-unless-zero-values
   #:mv-bare-return
   #:mv-lisp-return-tuple
   #:mv-lisp-alias-return
   #:mv-rec-void
   #:mv-rec-single
   #:mv-rec-pair
   #:mv-rec-nested-void
   #:mv-rec-mutual-void))

(defun coalton-tests/multiple-values::%mv-lisp-return-helper (x)
  (declare (type integer x)
           (values integer integer &optional))
  (cl:values x (cl:1+ x)))

(with-coalton-compilation (:package #:coalton-tests/multiple-values)
  (coalton-toplevel
    (declare mv-return-values (Integer -> Integer * Integer))
    (define (mv-return-values x)
      (values x (1+ x)))

    (declare mv-consume-values (Integer -> Integer))
    (define (mv-consume-values x)
      (let (values a b) = (mv-return-values x))
      (+ a b))

    (declare mv-lisp-return-values (Integer -> Integer * Integer))
    (define (mv-lisp-return-values x)
      (lisp (-> Integer * Integer) (x)
        (%mv-lisp-return-helper x)))

    (declare mv-lisp-consume-values (Integer -> Integer))
    (define (mv-lisp-consume-values x)
      (let (values a b) = (mv-lisp-return-values x))
      (+ a b))

    (define (mv-when-zero-values b)
      (when b
        (+ 1 2)))

    (define (mv-unless-zero-values b)
      (unless b
        (+ 1 2)))

    (define (mv-bare-return)
      (return))

    ;; rec returning 0, 1, and 2 values
    (declare mv-rec-void (UFix -> Void))
    (define (mv-rec-void n)
      "rec that returns zero values (Void)."
      (rec go ((i 0))
        (when (< i n)
          (go (+ i 1)))))

    (declare mv-rec-single (UFix -> UFix))
    (define (mv-rec-single n)
      "rec that returns a single value."
      (rec go ((i 0))
        (if (>= i n)
            i
            (go (+ i 1)))))

    (declare mv-rec-pair (UFix -> UFix * UFix))
    (define (mv-rec-pair n)
      "rec that returns two values."
      (rec go ((i 0))
        (if (>= i n)
            (values i (* i i))
            (go (+ i 1)))))

    (declare mv-rec-nested-void (UFix * UFix -> Void))
    (define (mv-rec-nested-void m n)
      "Nested rec loops both returning Void via when."
      (rec outer ((i 0))
        (when (< i m)
          (rec inner ((j 0))
            (when (< j n)
              (inner (+ j 1))))
          (outer (+ i 1)))))

    (declare mv-rec-mutual-void (UFix -> Void))
    (define (mv-rec-mutual-void n)
      "Mutually recursive let-bound functions both returning Void."
      (let ((ping (fn (i)
              (when (< i n)
                (pong (+ i 1)))))
            (pong (fn (j)
              (when (< j n)
                (ping (+ j 1))))))
        (ping 0)))

    (declare mv-lisp-return-tuple (Integer -> (Tuple Integer Integer)))
    (define (mv-lisp-return-tuple x)
      (let (values a b) = (mv-lisp-return-values x))
      (Tuple a b))

    (define-type-alias IntPair (Tuple Integer Integer))

    (declare mv-lisp-alias-return (Integer -> IntPair))
    (define (mv-lisp-alias-return x)
      (let (values a b) = (mv-lisp-return-values x))
      (Tuple a b))))

(defun %mv-count-direct-calls-if (node predicate)
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

(defun %mv-count-values-binds (node)
  (declare (type ast:node node)
           (values fixnum &optional))
  (let ((count 0))
    (traverse:traverse
     node
     (list
      (traverse:action (:after ast:node-values-bind _node)
        (declare (ignore _node))
        (incf count)
        (values))))
    count))

(defun %mv-count-lisp-nodes-if (node predicate)
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

(defun %mv-tuple-constructor-rator-p (rator)
  (declare (type symbol rator)
           (values boolean &optional))
  (eq rator 'coalton-library/classes:tuple))

(deftest direct-multiple-values-runtime ()
  (is (equal '(10 11)
             (multiple-value-list
              (eval '(coalton:coalton
                      (coalton-tests/multiple-values:mv-return-values 10))))))
  (is (equal '(10 11)
             (multiple-value-list
              (eval '(coalton:coalton
                      (coalton-tests/multiple-values:mv-lisp-return-values 10))))))
  (is (= 21
         (eval '(coalton:coalton
                 (coalton-tests/multiple-values:mv-consume-values 10)))))
  (is (= 21
         (eval '(coalton:coalton
                 (coalton-tests/multiple-values:mv-lisp-consume-values 10)))))
  (is (null
       (multiple-value-list
        (eval '(coalton:coalton
                (coalton-tests/multiple-values:mv-when-zero-values coalton:True))))))
  (is (null
       (multiple-value-list
        (eval '(coalton:coalton
                (coalton-tests/multiple-values:mv-unless-zero-values coalton:False))))))
  (is (null
       (multiple-value-list
        (eval '(coalton:coalton
                (coalton-tests/multiple-values:mv-bare-return)))))))

(deftest rec-multiple-values-runtime ()
  ;; rec returning Void (0 values)
  (is (null
       (multiple-value-list
        (eval '(coalton:coalton
                (coalton-tests/multiple-values:mv-rec-void 5))))))
  ;; rec returning a single value
  (is (= 5
         (eval '(coalton:coalton
                 (coalton-tests/multiple-values:mv-rec-single 5)))))
  ;; rec returning two values
  (is (equal '(5 25)
             (multiple-value-list
              (eval '(coalton:coalton
                      (coalton-tests/multiple-values:mv-rec-pair 5))))))
  ;; nested rec both returning Void via when
  (is (null
       (multiple-value-list
        (eval '(coalton:coalton
                (coalton-tests/multiple-values:mv-rec-nested-void 3 4))))))
  ;; mutual recursion returning Void via when
  (is (null
       (multiple-value-list
        (eval '(coalton:coalton
                (coalton-tests/multiple-values:mv-rec-mutual-void 5)))))))

(deftest multiple-values-to-tuple-runtime ()
  (is (= 10
         (eval '(coalton:coalton
                 (coalton-library/tuple:fst
                  (coalton-tests/multiple-values:mv-lisp-return-tuple 10))))))
  (is (= 11
         (eval '(coalton:coalton
                 (coalton-library/tuple:snd
                  (coalton-tests/multiple-values:mv-lisp-return-tuple 10))))))
  (is (= 10
         (eval '(coalton:coalton
                 (coalton-library/tuple:fst
                  (coalton-tests/multiple-values:mv-lisp-alias-return 10))))))
  (is (= 11
         (eval '(coalton:coalton
                 (coalton-library/tuple:snd
                  (coalton-tests/multiple-values:mv-lisp-alias-return 10)))))))

(deftest direct-multiple-values-codegen ()
  (let* ((consumer-node (coalton:lookup-code 'coalton-tests/multiple-values::mv-consume-values))
         (lisp-node (coalton:lookup-code 'coalton-tests/multiple-values::mv-lisp-return-values))
         (consumer-body (ast:node-abstraction-subexpr consumer-node))
         (lisp-body (ast:node-abstraction-subexpr lisp-node)))
    (is (= 1 (%mv-count-values-binds consumer-body)))
    (is (= 0 (%mv-count-direct-calls-if consumer-body #'%mv-tuple-constructor-rator-p)))
    (is (= 1 (%mv-count-lisp-nodes-if
              lisp-body
              (lambda (node)
                (= 2 (tc:multiple-value-output-arity (ast:node-type node)))))))))
