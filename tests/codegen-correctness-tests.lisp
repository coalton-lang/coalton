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

