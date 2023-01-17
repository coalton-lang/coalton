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
      (define (mylist-zero-circle)
        (let ((lst (MyCons 0 lst)))
          lst))
      (define (mylist-circle? lst)
        (match lst
          ((MyNil) False)
          ((MyCons _ tail) (coalton-library/addressable:eq? lst tail))))))
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

#+broken
(deftest recursively-construct-via-non-constructor-function ()
  (signals coalton-impl/typechecker::self-recursive-non-constructor-call
    (run-coalton-typechecker
     '((coalton:declare flipped-cons ((coalton:List :elt) coalton:-> :elt coalton:-> (coalton:List :elt)))
       (coalton:define (flipped-cons list new-head)
         (coalton:Cons new-head list))

       (coalton:declare make-loop (:elt -> (coalton:List :elt)))
       (coalton:define (make-loop elt)
         (coalton:let ((loop (flipped-cons loop elt))) loop))))))

#+broken
(deftest recursively-construct-bad-repr-types ()
  (signals coalton-impl/typechecker::self-recursive-non-default-repr
    (run-coalton-typechecker
     '((coalton:repr :transparent)
       (coalton:define-type (MyLoop :elt) (MyLoop (MyLoop :elt)))

       (coalton:declare make-loop (coalton:Unit coalton:-> (MyLoop :any)))
       (coalton:define (make-loop)
         (coalton:let ((loop (MyLoop loop))) loop)))))

  (signals coalton-impl/typechecker::self-recursive-non-default-repr
    (run-coalton-typechecker
     '((coalton:repr :native (cl:or cl:null mynullableloop))
       (coalton:define-type (MyNullableLoop :elt)
         (NonNull :elt)
         (MyLoop (MyNullableLoop :elt)))

       (coalton:declare make-loop (coalton:Unit coalton:-> (MyNullableLoop :any)))
       (coalton:define (make-loop)
         (coalton:let ((lp (MyLoop lp)))
           lp))))))

#+broken
(deftest recursive-partial-constructor-application ()
  (signals coalton-impl/typechecker::self-recursive-partial-application
    (run-coalton-typechecker
     '((coalton:define-type (PairedLoop :elt)
         (PairedLoop (:elt coalton:-> (PairedLoop :elt)) :elt))

       (coalton:declare make-partial-loop (coalton:Unit coalton:-> :elt coalton:-> (PairedLoop :elt)))
       (coalton:define (make-partial-loop)
         (coalton:let ((lp (PairedLoop lp))) lp))))))

#+broken
(deftest recursive-indirect-constructor-application ()
  (signals coalton-impl/typechecker::self-recursive-non-constructor-call
    (run-coalton-typechecker
     '((coalton:define-type (MyLoop :elt) (MyLoop (MyLoop :elt)))
       (coalton:declare make-circular-list-with-deranged-shadowing (:elt -> (coalton:List :elt)))
       (coalton:define (make-circular-list-with-deranged-shadowing elt)
         (coalton:let ((MyLoop coalton:Cons))
           (coalton:let ((lp (MyLoop elt lp)))
             lp)))))))

#+broken
(deftest mutually-recursive-data-and-function ()
  (signals coalton-impl/typechecker::mutually-recursive-function-and-data
    (run-coalton-typechecker
     '((coalton:define-type LoopFn
         (LoopFn (coalton:Unit coalton:-> LoopFn)))
       (coalton:declare make-loop-fn (coalton:Unit -> LoopFn))
       (coalton:define (make-loop-fn)
         (coalton:let ((lp (LoopFn func))
                       (func (coalton:fn () lp)))
           lp))))))
