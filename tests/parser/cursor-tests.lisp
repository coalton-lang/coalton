(fiasco:define-test-package #:coalton-impl/parser/cursor-tests
  (:use
   #:cl)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:cursor #:coalton-impl/parser/cursor)
   (#:cst #:concrete-syntax-tree)))

(in-package #:coalton-impl/parser/cursor-tests)

(defun make-cursor (string)
  (let ((source (source:make-source-string string)))
    (with-open-stream (stream (source:source-stream source))
      (parser:with-reader-context stream
        (cursor:make-cursor source
                            (parser:maybe-read-form stream
                                                    parser::*coalton-eclector-client*))))))

(deftest read-forward ()
  (let ((c (make-cursor "(1 2 3)")))
    (is (cst:consp (cursor:cursor-value c)))
    (is (eql 1 (cursor:next c)))
    (is (eql 2 (cursor:next c)))
    (is (eql 3 (cursor:next c)))
    (is (cursor:empty-p c))
    (signals source:source-error
      (cursor:next c))))

(deftest collect-symbols ()
  (let ((c (make-cursor "(a b c)")))
    (is (equal '(a b c)
               (cursor:collect-symbols c))))

  (signals source:source-error
    (cursor:collect-symbols (make-cursor "(a () c)"))))
