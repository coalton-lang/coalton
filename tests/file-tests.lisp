(cl:in-package #:coalton-native-tests)

(define-test write-then-read ()
  (let ((path (lisp String () (cl:symbol-name (cl:gensym "coalton-test-tempfile-"))))
        (test-data (lisp String () (cl:symbol-name (cl:gensym "coalton-test-data-")))))
    (progn
      (expect "Opening output file failed"
              (file:with-output-file! (out path)
                (expect "Write failed"
                        (file:write-line! out test-data))))
      (is (== test-data
              (expect "Opening input file failed"
                      (file:with-input-file! (in path)
                        (expect "Read failed"
                                (file:read-line! in)))))))))
