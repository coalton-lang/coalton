(cl:in-package #:coalton-native-tests)

(define-test write-then-read ()
  (let ((path (lisp String () (cl:symbol-name (cl:gensym "coalton-test-tempfile-"))))
        (test-data (lisp String () (cl:symbol-name (cl:gensym "coalton-test-data-")))))
    (progn
      (expect "Opening output file failed"
              (file:with-char-output! file:default-file-options
                (into path)
                (fn (out) (expect "Write failed"
                                  (char-io:write-line! out test-data)))))
      (is (== test-data
              (expect "Opening input file failed"
                      (file:with-char-input! file:default-file-options
                        (into path)
                        (fn (in) (expect "Read failed"
                                         (char-io:read-line! in))))))))))
