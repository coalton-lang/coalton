(in-package #:coalton-tests)

(deftest test-repr-form-position ()
  (signals coalton-impl::coalton-parse-error
    (run-coalton-typechecker
     ;; No next form
     '((coalton:repr :lisp))))
  (signals coalton-impl::coalton-parse-error
    (run-coalton-typechecker
     ;; Wrong next form
     '((coalton:repr :lisp)
       (coalton:define foo "foo")))))

(deftest test-repr-form-arity ()
  (signals coalton-impl::coalton-parse-error
    (run-coalton-typechecker
     ;; Too few arguments
     '((coalton:repr)
       (coalton:define-type Foo Foo))))
  (signals coalton-impl::coalton-parse-error
    (run-coalton-typechecker
     ;; Too many arguments
     '((coalton:repr :lisp :what-am-i-doing-here)
       (coalton:define-type Foo Foo)))))

(deftest test-repr-form-argument ()
  (signals warning
    (run-coalton-typechecker
     ;; Not a meaningful repr choice
     '((coalton:repr :something-weird)
       (coalton:define-type Foo Foo))))
  (finishes
    (run-coalton-typechecker
     ;; Perfect!
     '((coalton:repr :lisp)
       (coalton:define-type Foo Foo)))))
