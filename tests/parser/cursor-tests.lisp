(fiasco:define-test-package #:coalton-impl/parser/cursor-tests
  (:use
   #:cl)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:cursor #:coalton-impl/parser/cursor)
   (#:cst #:concrete-syntax-tree)))

(in-package #:coalton-impl/parser/cursor-tests)

(defun make-cursor (string)
  (with-input-from-string (stream string)
    (parser:with-reader-context stream
      (cursor:make-cursor (parser:maybe-read-form stream parser::*coalton-eclector-client*)))))

(deftest read-forward ()
  (let ((c (make-cursor "(1 2 3)")))
    (is (cst:consp (cursor:cursor-value c)))
    (is (eql 1 (cursor:next c)))
    (is (eql 2 (cursor:next c)))
    (is (eql 3 (cursor:next c)))
    (is (cursor:empty-p c))
    (signals cursor:syntax-error
      (cursor:next c))))

(deftest collect-symbols ()
  (let ((c (make-cursor "(a b c)")))
    (is (equal '(a b c)
               (cursor:collect-symbols c))))

  (signals cursor:syntax-error
    (cursor:collect-symbols (make-cursor "(a () c)"))))
