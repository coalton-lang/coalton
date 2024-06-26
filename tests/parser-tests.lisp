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
               (let ((stream (make-instance 'parser:position-stream :stream stream)))
                 (parser:with-reader-context stream
                   (parser:read-program stream (se:make-file :stream stream :name (namestring file)) :mode :file)))))

           (parse-error-text (file)
             (with-open-file (stream file
                                     :direction :input
                                     :element-type 'character)
               (let ((stream (make-instance 'parser:position-stream :stream stream)))
                 (handler-case
                     (parser:with-reader-context stream
                       (entry:entry-point
                        (parser:read-program stream (se:make-file :stream stream :name "test") :mode :file))
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

(deftest test-character-offsets ()
  "Check that read assigned source ranges to toplevel and nested forms, and that source ranges are expressed as character offsets."
  (let (from-string
        from-file)
    (with-input-from-string (stream "(\"マグロをもう少しいただけますか?\")")
      (let ((stream (make-instance 'parser:position-stream :stream stream)))
        (parser:with-reader-context stream
          (setf from-string (parser:maybe-read-form stream)))))
    (with-open-file (stream (merge-pathnames "tests/parser-test-files/utf-8.txt"
                                             (asdf:system-source-directory "coalton/tests")))
      (let ((stream (make-instance 'parser:position-stream :stream stream)))
        (parser:with-reader-context stream
          (setf from-file (parser:maybe-read-form stream)))))
    (is (equal (cst:source from-file)
               (cst:source from-string)))
    (is (equal (cst:source from-file)
               '(0 . 20)))
    (is (equal (cst:source (cst:first from-file))
               (cst:source (cst:first from-string))))
    (is (equal (cst:source (cst:first from-file))
               '(1 . 19)))))
