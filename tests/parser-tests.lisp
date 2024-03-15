(in-package #:coalton-tests)

(deftest test-parser ()
  (labels ((test-files (pattern)
             (directory (merge-pathnames pattern (asdf:system-source-directory "coalton/tests"))))

           (parse-file (file)
             (with-open-file (stream file)
               (parser:with-reader-context stream
                 (parser:read-program stream (error:make-coalton-file :stream stream :name (namestring file)) :mode :file))))

           (parse-error-text (file)
             (with-open-file (stream file)
               (handler-case
                   (parser:with-reader-context stream
                     (parser:read-program stream (error:make-coalton-file :stream stream :name "test") :mode :file))
                 (error:coalton-base-error (c)
                   (format nil "~A" c))))))
    (loop :for file :in (test-files "tests/parser/*.bad.coalton")
          :do (let ((error-file (make-pathname :type "error"
                                               :defaults file)))
                (cond ((uiop:file-exists-p error-file)
                       (check-string= (format nil "expected error ~A (A) and generated error (B)" error-file)
                                      (alexandria:read-file-into-string error-file)
                                      (parse-error-text file)))
                      (t
                       (signals parser:parse-error
                         (parse-file file))))))

    (loop :for file :in (test-files "tests/parser/*.good.coalton")
          :do (parse-file file))))
