(in-package #:coalton-tests)

(deftest test-parser ()
  (labels ((test-files (pattern)
             (let ((files (directory (merge-pathnames pattern (asdf:system-source-directory "coalton/tests")))))
               (when (endp files)
                 (error "No test files match pattern '~A'" pattern))
               files))

           (parse-file (file)
             (with-open-file (stream file
                                     :direction :input
                                     :element-type 'character)
               (parser:with-reader-context stream
                 (parser:read-program stream (se:make-file :stream stream :name (namestring file)) :mode :file))))

           (parse-error-text (file)
             (with-open-file (stream file
                                     :direction :input
                                     :element-type 'character)
               (handler-case
                   (parser:with-reader-context stream
                     (entry:entry-point
                      (parser:read-program stream (se:make-file :stream stream :name "test") :mode :file))
                     "no errors")
                 (se:source-base-error (c)
                   (princ-to-string c))))))
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
