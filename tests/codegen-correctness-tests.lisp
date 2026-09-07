(in-package #:coalton-tests)

(defvar *codegen-events* nil)

(defmacro with-codegen-test-environment (&body body)
  `(let ((*package* (make-package (gensym "CODEGEN-")
                                  :use '("COALTON" "COALTON-PRELUDE")))
         (entry:*global-environment* entry:*global-environment*)
         (*codegen-events* nil))
     (unwind-protect
          (handler-bind ((style-warning #'muffle-warning)) ,@body)
       (delete-package *package*))))

(defun codegen-test-compile (text)
  (let ((source (source:make-source-string text)))
    (with-open-stream (stream (source:source-stream source))
      (eval (entry:compile-coalton-toplevel
             (parser:with-reader-context stream (parser:read-program stream source)))))))

(defun codegen-test-eval (text)
  (eval (list 'coalton:coalton (read-from-string text))))

(defun codegen-test-event-recorder ()
  (codegen-test-compile
   "(declare observe (Integer -> Integer))
    (define (observe x)
      (lisp (-> Integer) (x) (cl:push x coalton-tests::*codegen-events*) x))"))

(deftest codegen-match-preserves-evaluation ()
  (with-codegen-test-environment
    (codegen-test-event-recorder)
    (is (= 1 (codegen-test-eval "(match (observe 1) (x (observe 2) x))")))
    (is (equal '(1 2) (reverse *codegen-events*)))
    (setf *codegen-events* nil)
    (codegen-test-compile
     "(declare delayed (Void -> (Void -> Integer)))
      (define (delayed) (match (observe 1) (x (fn () x))))")
    (let ((function (codegen-test-eval "(delayed)")))
      (is (equal '(1) *codegen-events*))
      (is (= 1 (funcall function)))
      (is (= 1 (funcall function)))
      (is (equal '(1) *codegen-events*)))
    (setf *codegen-events* nil)
    (is (= 0 (codegen-test-eval "(match (observe 3) (x (if False x 0)))")))
    (is (equal '(3) *codegen-events*))
    (setf *codegen-events* nil)
    (is (= 3 (codegen-test-eval
              "(match (Tuple (observe 1) (observe 2))
                 ((Tuple x y) (observe 3) (+ x y)))")))
    (is (equal '(1 2 3) (reverse *codegen-events*)))))

(deftest codegen-inline-preserves-argument-order ()
  (with-codegen-test-environment
    (codegen-test-event-recorder)
    (codegen-test-compile
     "(inline)
      (declare first-arg (Integer * Integer * Integer -> Integer))
      (define (first-arg x _y _z) x)
      (declare call-first (Void -> Integer))
      (define (call-first) (first-arg (observe 1) (observe 2) (observe 3)))")
    (is (= 1 (codegen-test-eval "(call-first)")))
    (is (equal '(1 2 3) (reverse *codegen-events*)))
    (setf *codegen-events* nil)
    (is (= 1 (codegen-test-eval
              "(first-arg (first-arg (observe 1) (observe 2) (observe 3))
                          (observe 4) (observe 5))")))
    (is (equal '(1 2 3 4 5) (reverse *codegen-events*)))
    (setf *codegen-events* nil)
    (codegen-test-compile
     "(declare fail-argument (Void -> Integer))
      (define (fail-argument) (lisp (-> Integer) () (cl:error \"argument failure\")))")
    (signals simple-error
      (codegen-test-eval "(first-arg (fail-argument) (observe 2) (observe 3))"))
    (is (null *codegen-events*))))

(deftest codegen-loop-snapshots-survive-updates ()
  (with-codegen-test-environment
    (dolist (loop-name '("for" "for*"))
      (dolist (binding '("(let ((snapshot i)) BODY)"
                        "(progn (let snapshot = i) BODY)"
                        "(match i (snapshot BODY))"))
        (let* ((save "(coalton/cell:write! saved
                       (Cons (fn () snapshot) (coalton/cell:read saved)))")
               (body (concatenate 'string (subseq binding 0 (search "BODY" binding))
                                  save (subseq binding (+ 4 (search "BODY" binding)))))
               (functions (codegen-test-eval
                           (format nil
                            "(let ((saved (coalton/cell:new Nil)))
                               (~A ((i (the Integer 0) (+ i 1))) :repeat 3 ~A)
                               (coalton/cell:read saved))" loop-name body))))
          (is (equal '(2 1 0) (mapcar #'funcall functions))))))))

(deftest codegen-retains-functions-imported-by-lisp ()
  (with-codegen-test-environment
    (is (= 4 (codegen-test-eval
              "(let ((f (fn (x) (+ x (the Integer 1)))))
                 (f 2)
                 (lisp (-> Integer) (f) (cl:funcall f 3)))")))
    (let ((function (codegen-test-eval
                     "(let ((f (fn (x) (+ x (the Integer 1)))))
                        (f 2)
                        (fn () (lisp (-> Integer) (f) (cl:funcall f 4))))")))
      (is (= 5 (funcall function)))
      (is (= 5 (funcall function))))))

(deftest codegen-transparent-nested-patterns ()
  (with-codegen-test-environment
    (codegen-test-compile
     "(repr :transparent)
      (define-type IntList (IntList (List Integer)))
      (declare head-or-zero (IntList -> Integer))
      (define (head-or-zero xs)
        (match xs ((IntList (Cons x _)) x) ((IntList (Nil)) 0)))
      (repr :transparent)
      (define-type (Wrapped :a) (Wrapped (Optional (Tuple :a :a))))
      (declare first-or-zero (Wrapped Integer -> Integer))
      (define (first-or-zero x)
        (match x ((Wrapped (Some (Tuple a _))) a) ((Wrapped (None)) 0)))")
    (is (= 42 (codegen-test-eval "(head-or-zero (IntList (make-list 42 43)))")))
    (is (= 0 (codegen-test-eval "(head-or-zero (IntList Nil))")))
    (is (= 42 (codegen-test-eval "(first-or-zero (Wrapped (Some (Tuple 42 43))))")))
    (is (= 0 (codegen-test-eval "(first-or-zero (Wrapped None))")))))

(deftest codegen-inline-loop-initializer-scope ()
  (with-codegen-test-environment
    (codegen-test-compile
     "(inline)
      (declare sequential-init (Integer -> Integer))
      (define (sequential-init n)
        (for* ((i n (+ i 1)) (j i (+ j 1))) :returns j :repeat 1 (values)))
      (inline)
      (declare recursive-init (Integer -> Integer))
      (define (recursive-init n)
        (for ((j i (+ j 1)) (i n (+ i 1))) :returns j :repeat 1 (values)))
      (inline)
      (declare shadow-init (Integer -> Integer))
      (define (shadow-init i)
        (for* ((i (+ i 1) (+ i 1)) (j i (+ j i))) :returns j :repeat 1 (values)))")
    (is (= 11 (codegen-test-eval "(sequential-init 10)")))
    (is (= 11 (codegen-test-eval "(recursive-init 10)")))
    (is (= 23 (codegen-test-eval "(shadow-init 10)")))
    (is (= 32 (codegen-test-eval "(+ (sequential-init 10) (sequential-init 20))")))))

