(cl:in-package #:coalton-native-tests)

(define-test write-then-read ()
  (let ((path (lisp String () (cl:symbol-name (cl:gensym "coalton-test-tempfile-"))))
        (test-data (lisp String () (cl:symbol-name (cl:gensym "coalton-test-data-")))))
    (progn
      (expect "Writing as output failed"
              (file:with-char-output! (file:config file:Output)
                (into path)
                (fn (out) (char-io:write-line! out test-data))))
      (is (== test-data
              (expect "Reading as input failed"
                      (file:with-char-input! (file:config file:Input)
                        (into path)
                        (fn (in) (char-io:read-line! in)))))))))
