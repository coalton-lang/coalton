(in-package #:coalton-tests)

(deftest test-bad-toplevel-type ()
  (signals coalton-impl::coalton-parse-error
    (run-coalton-toplevel-walker
     ;; Atoms don't make sense at toplevel.
     '(:this-is-a-stray-atom)))
  (signals coalton-impl::coalton-parse-error
    (run-coalton-toplevel-walker
     ;; Neither do lists with cars that are not symbols.
     '((1729)))))

(deftest test-bad-toplevel-form ()
  (signals coalton-impl::coalton-parse-error
    (run-coalton-toplevel-walker
     ;; Expressions representing values don't make sense there either.
     '((coalton:Cons coalton:Nil coalton:Nil)))))

(deftest test-repr-form-position ()
  (signals coalton-impl::coalton-parse-error
    (run-coalton-toplevel-walker
     ;; No next form
     '((coalton:repr :lisp))))
  (signals coalton-impl::coalton-parse-error
    (run-coalton-toplevel-walker
     ;; Wrong next form
     '((coalton:repr :lisp)
       (coalton:define foo "foo")))))

(deftest test-repr-form-arity ()
  (signals coalton-impl::coalton-parse-error
    (run-coalton-toplevel-walker
     ;; Too few arguments
     '((coalton:repr)
       (coalton:define-type Foo Foo))))
  (signals coalton-impl::coalton-parse-error
    (run-coalton-toplevel-walker
     ;; Too many arguments
     '((coalton:repr :lisp :what-am-i-doing-here)
       (coalton:define-type Foo Foo)))))

(deftest test-repr-form-argument ()
  (signals style-warning
    (run-coalton-toplevel-walker
     ;; Not a meaningful repr choice
     '((coalton:repr :something-weird)
       (coalton:define-type Foo Foo))))
  (finishes
    (run-coalton-toplevel-walker
     ;; Perfect!
     '((coalton:repr :lisp)
       (coalton:define-type Foo Foo)))))

(deftest test-toplevel-walker-return-value ()
  (let ((trivial-value (run-coalton-toplevel-walker '())))
    (is (listp trivial-value))
    (is (hash-table-p
         (getf trivial-value 'coalton-impl::repr-table)))))
