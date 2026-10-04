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

(defun rec-test-local-function-arities (name)
  "Return the arities of local functions in NAME's generated IR."
  (let ((arities nil))
    (traverse:traverse
     (ast:node-abstraction-subexpr
      (tc:lookup-code entry:*global-environment* (intern name)))
     (list
      (traverse:action (:after ast:node-abstraction node)
        (push (length (ast:node-abstraction-vars node)) arities)
        (values))))
    (sort arities #'<)))

(deftest rec-reduces-concrete-dictionaries ()
  (with-codegen-test-environment
    (codegen-test-compile
     "(declare accumulate (UFix * F64 -> F64))
      (define (accumulate count increment)
        (rec next ((i 0) (total 0.0d0))
          (if (< i count)
              (next (+ i 1) (+ total increment))
              total)))
      (declare from-result (UFix -> F64))
      (define (from-result count)
        (rec next ((i count) (total 0))
          (if (== i 0)
              total
              (next (- i 1) (+ total 1)))))
      (define-class (Scalar :a :b (:a -> :b)) (scalar-value (:a -> :b)))
      (define-instance (Scalar UFix F64) (define (scalar-value _) 0.5d0))
      (define (from-fundep count)
        (rec next ((i (the UFix count)) (total 0))
          (if (== i 0)
              total
              (next (- i 1) (+ total (scalar-value i))))))")
    (is (= 2.5d0 (codegen-test-eval "(accumulate 5 0.5d0)")))
    (is (= 5.0d0 (codegen-test-eval "(from-result 5)")))
    (is (= 2.5d0 (codegen-test-eval "(from-fundep 5)")))
    ;; Only the two source parameters should remain, including when the
    ;; enclosing declaration or a fundep makes the accumulator concrete.
    (is (equal '(2) (rec-test-local-function-arities "ACCUMULATE")))
    (is (equal '(2) (rec-test-local-function-arities "FROM-RESULT")))
    (is (equal '(2) (rec-test-local-function-arities "FROM-FUNDEP")))))

(deftest rec-retains-polymorphic-dictionaries ()
  (with-codegen-test-environment
    (codegen-test-compile
     "(declare accumulate (Num :a => UFix * :a -> :a))
      (define (accumulate count initial)
        (rec next ((i count) (total initial))
          (if (== i 0)
              total
              (next (- i 1)
                    (if (== total initial)
                        (+ initial total)
                        (+ total initial))))))
      (declare mixed (Num :a => UFix * :a -> F64 * :a))
      (define (mixed count initial)
        (rec next ((i count) (counter 0) (total initial))
          (if (== i 0)
              (values counter total)
              (next (- i 1) (+ counter 1) (+ total initial)))))")
    (is (= 12 (codegen-test-eval "(accumulate 3 (the Integer 3))")))
    (is (= 2.0d0 (codegen-test-eval "(accumulate 3 0.5d0)")))
    ;; The concrete counter's dictionaries disappear, while unresolved Num
    ;; and Eq evidence retains its original order and multiplicity.
    (is (equal '(5) (rec-test-local-function-arities "ACCUMULATE")))
    ;; The result declaration fixes COUNTER only after local inference, so
    ;; final reduction must drop its dictionary and retain Num for TOTAL.
    (is (equal '(3.0d0 12)
               (multiple-value-list (codegen-test-eval "(mixed 3 (the Integer 3))"))))
    (is (equal '(3.0d0 2.0d0)
               (multiple-value-list (codegen-test-eval "(mixed 3 0.5d0)"))))
    (is (equal '(4) (rec-test-local-function-arities "MIXED")))))

(deftest rec-reduction-preserves-nested-and-dependent-recursion ()
  (with-codegen-test-environment
    (codegen-test-compile
     "(declare nested (UFix -> F64))
      (define (nested count)
        (rec outer ((i count) (total 0.0d0))
          (if (== i 0)
              total
              (outer (- i 1)
                     (rec inner ((j i) (subtotal total))
                       (if (== j 0)
                           subtotal
                           (inner (- j 1) (+ subtotal 0.5d0))))))))
      (declare dependent (Num :a => UFix * :a -> :a))
      (define (dependent count initial)
        (rec next ((i count) (x y) (y initial))
          (if (== i 0)
              x
              (next (- i 1) (+ x y) y))))")
    (is (= 5.0d0 (codegen-test-eval "(nested 4)")))
    (is (equal '(2 2) (rec-test-local-function-arities "NESTED")))
    ;; An init dependency still takes the ordinary recursive-let path.
    (is (= 12 (codegen-test-eval "(dependent 3 (the Integer 3))")))
    (is (= 2.0d0 (codegen-test-eval "(dependent 3 0.5d0)")))))

(deftest rec-reduction-preserves-method-and-argument-effects ()
  (with-codegen-test-environment
    (codegen-test-event-recorder)
    (codegen-test-compile
     "(define-class (Step :a) (step (:a -> :a)))
      (define-instance (Step Integer)
        (define (step x) (observe (+ x 10))))
      (declare run (Integer -> Integer))
      (define (run count)
        (rec next ((i (observe count)) (total (observe 0)))
          (observe i)
          (if (== i 0)
              total
              (next (observe (- i 1)) (step total)))))")
    (is (= 20 (codegen-test-eval "(run 2)")))
    (is (equal '(2 0 2 1 10 1 0 20 0) (reverse *codegen-events*)))))

(deftest rec-reduction-retains-contextual-value-dictionaries ()
  (with-codegen-test-environment
    (codegen-test-event-recorder)
    (codegen-test-compile
     "(define-class (LoopValue :a) (loop-value :a))
      (define-instance (LoopValue Integer) (define loop-value 0))
      (define-instance (LoopValue :a => LoopValue (List :a))
        (define loop-value (progn (observe 9) Nil)))
      (declare run (UFix -> List Integer))
      (define (run count)
        (rec next ((i count) (x Nil))
          (if (== i 0) x (next (- i 1) loop-value))))")
    ;; A contextual value method must not be moved into the recursive body.
    ;; In the existing dictionary path it is not evaluated by these calls.
    (dolist (count '(0 1 3))
      (setf *codegen-events* nil)
      (codegen-test-eval (format nil "(run ~D)" count))
      (is (null *codegen-events*)))
    ;; Keep its dictionary even after the enclosing declaration fixes :a.
    (is (equal '(3) (rec-test-local-function-arities "RUN")))))

(deftest rec-reduction-preserves-overlapping-instance-selection ()
  (with-codegen-test-environment
    (codegen-test-compile
     "(define-class (Pick :a) (pick (:a -> Integer)))
      (overlap) (define-instance (Pick :a) (define (pick _) 1))
      (declare run (Pick :a => UFix * :a -> Integer))
      (define (run count value)
        (rec next ((i count) (total 0))
          (if (== i 0)
              total
              (next (- i 1) (+ total (pick value))))))")
    ;; A blanket overlap instance is not a stable choice for a type variable.
    (codegen-test-compile
     "(overlap) (define-instance (Pick Integer) (define (pick _) 2))")
    (is (= 6 (codegen-test-eval "(run 3 (the Integer 0))")))
    (is (= 3 (codegen-test-eval "(run 3 True)"))))
  (with-codegen-test-environment
    (codegen-test-compile
     "(define-class (Pick :a) (pick (:a -> Integer)))
      (overlap) (define-instance (Pick :a) (define (pick _) 1))
      (declare run (UFix -> Integer))
      (define (run count)
        (rec next ((i count) (total 0))
          (if (== i 0)
              total
              (next (- i 1) (+ total (pick (the Integer 0)))))))")
    ;; A concrete choice is recorded, so changing its selected instance is
    ;; rejected just as it is for an ordinary function body.
    (signals tc:stale-instance-selection-error
      (codegen-test-compile
       "(overlap) (define-instance (Pick Integer) (define (pick _) 2))"))
    (is (= 3 (codegen-test-eval "(run 3)")))))

(deftest rec-reduction-preserves-development-method-redefinition ()
  (unless (coalton-impl/settings:coalton-release-p)
    (with-codegen-test-environment
      ;; Keep this method call dynamic even when heuristic inlining is enabled.
      ;; Copied method bodies have the same redefinition behavior as any other
      ;; inlined function; dictionary reduction must preserve the explicit call.
      (codegen-test-compile
       "(define-class (Step :a) (step (:a -> :a)))
        (define-instance (Step Integer) (define (step x) (+ x 1)))
        (declare run (UFix -> Integer))
        (define (run count)
          (rec next ((i count) (total 0))
            (if (== i 0)
                total
                (next (- i 1) (noinline (step total))))))")
      (is (equal '(2) (rec-test-local-function-arities "RUN")))
      (let ((caller (fdefinition (intern "RUN"))))
        (is (= 4 (funcall caller 4)))
        (codegen-test-compile
         "(define-instance (Step Integer) (define (step x) (+ x 2)))")
        (is (eq caller (fdefinition (intern "RUN"))))
        (is (= 8 (funcall caller 4)))))))

(deftest rec-reduction-matches-ordinary-method-redefinition ()
  (unless (coalton-impl/settings:coalton-release-p)
    (with-codegen-test-environment
      (codegen-test-compile
       "(define-class (Step :a) (step (:a -> :a)))
        (define-instance (Step Integer) (define (step x) (+ x 1)))
        (declare once (Integer -> Integer))
        (define (once value) (step value))
        (declare run (UFix -> Integer))
        (define (run count)
          (rec next ((i count) (total 0))
            (if (== i 0) total (next (- i 1) (step total)))))")
      (is (equal '(2) (rec-test-local-function-arities "RUN")))
      (let ((caller (fdefinition (intern "RUN")))
            (ordinary (fdefinition (intern "ONCE"))))
        (is (= 4 (funcall caller 4)))
        (codegen-test-compile
         "(define-instance (Step Integer) (define (step x) (+ x 2)))")
        (is (= 2 (codegen-test-eval "(step (the Integer 0))")))
        ;; Use the same previously compiled callers. Heuristic inlining may
        ;; copy the old method into either body; REC must agree with ordinary
        ;; calls without imposing a particular inlining decision.
        (let ((expected 0))
          (dotimes (iteration 4) (setf expected (funcall ordinary expected)))
          (is (= expected (funcall caller 4))))))))

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
