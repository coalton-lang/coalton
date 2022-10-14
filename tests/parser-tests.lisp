(in-package #:coalton-tests)

(deftest test-parser ()
  (let* ((glob (merge-pathnames "tests/parser/*.bad.coalton" (asdf:system-source-directory "coalton/tests")))

         (files (directory glob)))

    (loop :for file :in files
          :do (with-open-file (stream file)
                (signals parser:parse-error
                  (parser:read-program stream (error:make-coalton-file :stream stream :name (namestring file)) :mode :file)))))

  (let* ((glob (merge-pathnames "tests/parser/*.good.coalton" (asdf:system-source-directory "coalton/tests")))

         (files (directory glob)))

    (loop :for file :in files
          :do (with-open-file (stream file)
                (parser:read-program stream (error:make-coalton-file :stream stream :name (namestring file)) :mode :file)))))
