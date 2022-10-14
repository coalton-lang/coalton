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

(deftest recursively-construct-via-non-constructor-function ()
  (signals tc:tc-error
    (check-coalton-types
     "(declare flipped-cons (List :elt -> :elt -> List :elt))
      (define (flipped-cons list new-head)
        (Cons new-head list))

       (declare make-loop (:elt -> List :elt))
       (coalton:define (make-loop elt)
         (coalton:let ((loop (flipped-cons loop elt))) loop))")))

(deftest recursively-construct-bad-repr-types ()
  (signals tc:tc-error
    (check-coalton-types
     "(repr :transparent)
      (define-type (MyLoop :elt) (MyLoop (MyLoop :elt)))

       (declare make-loop (Unit -> (MyLoop :any)))
       (define (make-loop)
         (let ((loop (MyLoop loop))) loop))"))

  (signals tc:tc-error
    (check-coalton-types
     "(repr :native (cl:or cl:null mynullableloop))
      (define-type (MyNullableLoop :elt)
        (NonNull :elt)
        (MyLoop (MyNullableLoop :elt)))

      (declare make-loop (Unit -> (MyNullableLoop :any)))
      (define (make-loop)
         (let ((lp (MyLoop lp)))
           lp)))")))

(deftest recursive-partial-constructor-application ()
  (signals tc:tc-error
    (check-coalton-types
     "(define-type (PairedLoop :elt)
        (PairedLoop (:elt coalton:-> (PairedLoop :elt)) :elt))

      (declare make-partial-loop (Unit -> :elt -> (PairedLoop :elt)))
      (define (make-partial-loop)
        (let ((lp (PairedLoop lp))) lp))")))

(deftest recursive-indirect-constructor-application ()
  (signals tc:tc-error
    (check-coalton-types
     "(define-type (MyLoop :elt) (MyLoop (MyLoop :elt)))
      (declare make-circular-list-with-deranged-shadowing (:elt -> (List :elt)))
      (define (make-circular-list-with-deranged-shadowing elt)
         (let ((MyLoop Cons))
           (let ((lp (MyLoop elt lp)))
             lp)))")))

(deftest mutually-recursive-data-and-function ()
  (signals tc:tc-error
    (check-coalton-types
     "(define-type LoopFn
         (LoopFn (coalton:Unit coalton:-> LoopFn)))
      (declare make-loop-fn (Unit -> LoopFn))
      (define (make-loop-fn)
        (let ((lp (LoopFn func))
                       (func (fn () lp)))
           lp)))")))
