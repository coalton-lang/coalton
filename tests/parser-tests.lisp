(in-package #:coalton-tests)

(deftest test-parser ()
  (labels ((test-files (pattern)
             (let ((files (directory (test-file pattern))))
               (when (endp files)
                 (error "No test files match pattern '~A'" pattern))
               files))

           (parse-file (file)
             (let ((source (source:make-source-file file :name "test")))
               (with-open-stream (stream (source-error:source-stream source))
                 (parser:with-reader-context stream
                   (parser:read-program stream source ':file)))))

           (parse-error-text (file)
             (let ((source (source:make-source-file file :name "test")))
               (with-open-stream (stream (source-error:source-stream source))
                 (handler-case
                     (parser:with-reader-context stream
                       (entry:entry-point
                        (parser:read-program stream source ':file))
                       "no errors")
                   (se:source-base-error (c)
                     (princ-to-string c)))))))
    (dolist (file (test-files "tests/parser-test-files/bad-files/*.coal"))
      (let ((error-file (make-pathname :type "error"
                                       :defaults file)))
        (cond ((uiop:file-exists-p error-file)
               (check-string= (format nil "expected error ~A (A) and generated error (B)" error-file)
                              (alexandria:read-file-into-string error-file)
                              (parse-error-text file)))
              (t
               (signals parser:parse-error
                 (parse-file file))))))

    (dolist (file (test-files "tests/parser-test-files/good-files/*.coal"))
      (parse-file file))))

(deftest test-parser-test-files ()
  (let ((*features* (cons ':coalton-lisp-toplevel *features*)))
    (run-test-file "tests/parser-test-files/lisp-toplevel.txt"))
  (run-test-file "tests/parser-test-files/lisp-toplevel-forbid.txt")
  (run-test-file "tests/parser-test-files/package.txt")
  (run-test-file "tests/parser-test-files/hashtable.txt")
  (run-test-file "tests/parser-test-files/fundeps.txt")
  (run-test-file "tests/parser-test-files/recursive-let.txt")
  (run-test-file "tests/parser-test-files/struct.txt")
  (run-test-file "tests/parser-test-files/type-inference.txt")
  (run-test-file "tests/parser-test-files/unused-variables.txt")
  (run-test-file "tests/parser-test-files/define.txt")
  (run-test-file "tests/parser-test-files/define-class.txt"))
