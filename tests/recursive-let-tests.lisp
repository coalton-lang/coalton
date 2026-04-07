(in-package #:coalton-tests)

(uiop:define-package #:coalton-tests/recursive-let-tests
  (:use #:coalton #:coalton/classes)
  (:export #:MyList #:mylist-zero-circle #:mylist-circle?

           #:list-zero-circle))

(deftest recursively-construct-mylist ()
  "Test that it's possible to recursively construct a pure-Coalton definition of a classic linked list."
  (with-coalton-compilation (:package #:coalton-tests/recursive-let-tests)
    (coalton-toplevel
      (repr :lisp)
      (define-type MyList
        MyNil
        (MyCons IFix MyList))
      (define (mylist-zero-circle)
        (let ((lst (MyCons 0 lst)))
          lst))
      (define (mylist-circle? lst)
        (match lst
          ((MyNil) False)
          ((MyCons _ tail) (coalton/functions:unsafe-pointer-eq? lst tail))))))
  ;; hacky eval of quoted form to only compile this code after compiling the previous
  ;; `with-coalton-compilation' form.
  (is (eval '(coalton:coalton (coalton-tests/recursive-let-tests:mylist-circle?
                               (coalton-tests/recursive-let-tests:mylist-zero-circle))))))

(deftest recursively-construct-list ()
  (with-coalton-compilation (:package #:coalton-tests/recursive-let-tests)
    (coalton-toplevel
      (define (list-zero-circle)
        (let ((lst (Cons 0 lst)))
          lst))))
  (let* ((list (eval '(coalton:coalton (coalton-tests/recursive-let-tests:list-zero-circle)))))
    (is list)
    (is (eq list (cdr list)))))

(deftest recursive-let-the ()
  "Test that recusive `let`-bindings whose initforms are `the` are accepted as long as the inner initform is acceptable."
  (check-coalton-types
   "(define foo
      (let ((circle (the (List Integer)
                         (Cons 0 circle))))
        circle))"
   '("foo" . "(List Integer)"))

  (check-coalton-types
   "(define foo
      (let ((loop-times (the (UFix -> Void)
                             (fn (n)
                               (unless (== n 0)
                                 (loop-times (- n 1)))))))
        (loop-times 100)))"
   '("foo" . "Void")))

(deftest rec-does-not-capture-return ()
  (with-coalton-compilation (:package #:coalton-tests/recursive-let-tests)
    (coalton-toplevel
      (declare rec-return-regression (UFix -> UFix))
      (define (rec-return-regression n)
        (+ 100
           (rec go ((i 0))
             (if (>= i n)
                 (return i)
                 (go (+ i 1))))))))
  (is (= 5
         (eval '(coalton:coalton
                 (coalton-tests/recursive-let-tests::rec-return-regression 5))))))

(deftest rec-short-circuit-tail-calls-are-allowed ()
  (with-coalton-compilation (:package #:coalton-tests/recursive-let-tests)
    (coalton-toplevel
      (declare rec-tail-under-and (UFix -> Boolean))
      (define (rec-tail-under-and n)
        (rec go ((i 0))
          (if (>= i n)
              True
              (and True
                   (go (+ i 1))))))

      (declare rec-tail-under-or (UFix -> Boolean))
      (define (rec-tail-under-or n)
        (rec go ((i 0))
          (if (>= i n)
              True
              (or False
                  (go (+ i 1))))))))
  (is (eval '(coalton:coalton
              (coalton-tests/recursive-let-tests::rec-tail-under-and 5))))
  (is (eval '(coalton:coalton
              (coalton-tests/recursive-let-tests::rec-tail-under-or 5)))))

(deftest rec-catch-tail-calls-are-allowed ()
  (with-coalton-compilation (:package #:coalton-tests/recursive-let-tests)
    (coalton-toplevel
      (declare rec-tail-under-catch (UFix -> UFix * UFix))
      (define (rec-tail-under-catch n)
        (rec go ((i 0))
          (catch (if (>= i n)
                     (values i (* i i))
                     (go (+ i 1)))
            (_ (values 0 0)))))))
  (is (equal '(5 25)
             (multiple-value-list
              (eval '(coalton:coalton
                      (coalton-tests/recursive-let-tests::rec-tail-under-catch 5)))))))

(deftest recursive-let-constant-propagation ()
  "Test that constant let bindings are propagated to the other bindings. See GitHub issue #1442."
  (check-coalton-types
   "(define x
      (let ((p (the UFix 3))
            (q (1+ p)))
        q))"
   '("x" . "UFix"))

  (check-coalton-types
   "(define x
      (let ((q (1+ p))
            (p (the UFix 3)))
        q))"
   '("x" . "UFix"))

  (is (= 3 (coalton:coalton (coalton:let ((a b) (b c) (c d) (d 3)) a))))

  (is (= 3 (coalton:coalton (coalton:let ((a (coalton:let ((b c) (c d)) d)) (d 3)) a))))

  (let* ((start (/ (get-internal-real-time) internal-time-units-per-second))
         (value (eval (read-from-string "(coalton:coalton (coalton:make-list 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0))")))
         (end (/ (get-internal-real-time) internal-time-units-per-second)))

    (is (< (- end start) 1))
    (is (equalp value '(1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0)))))

(deftest sequential-let-star-bindings ()
  (is (= 1
         (coalton:coalton
          (coalton:let* ((a 1)
                         (b a))
            b)))))

(deftest sequential-let-star-bindings-are-non-recursive ()
  (check-coalton-types
   "(define foo
      (let ((declare x UFix)
            (x 1))
        (let* ((declare x UFix)
               (x (1+ x)))
          x)))"
   '("foo" . "UFix")))

(deftest sequential-let-star-binding-declare-mismatch ()
  (signals coalton-impl/typechecker:tc-error
    (check-coalton-types
     "(define bad
        (let* ((declare y String)
               (x 1)
               (y x))
          y))")))

(deftest sequential-let-star-self-reference-without-outer-binding-is-unbound ()
  (let ((msg (collect-compiler-error
              "(package coalton-test-let-star-errors
  (import coalton-prelude))

(define bad
  (let* ((x (1+ x)))
    x))")))
    (is (search "Unknown variable" msg))
    (is (search "(let* ((x (1+ x)))" msg))))
