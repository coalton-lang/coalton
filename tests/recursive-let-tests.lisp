(in-package #:coalton-tests)

(uiop:define-package #:coalton-tests/recursive-let-tests
  (:use #:coalton #:coalton-library/classes)
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
      (define (mylist-zero-circle _)
        (let ((lst (MyCons 0 lst)))
          lst))
      (define (mylist-circle? lst)
        (match lst
          ((MyNil) False)
          ((MyCons _ tail) (coalton-library/functions:unsafe-pointer-eq? lst tail))))))
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
      (let ((loop-times (the (UFix -> Unit)
                             (fn (n)
                               (unless (== n 0)
                                 (loop-times (- n 1)))))))
        (loop-times 100)))"
   '("foo" . "Unit")))
