(in-package #:coalton-tests)

(deftest parse-files ()
  (let* ((glob (merge-pathnames "tests/parser/*.bad.coalton" (asdf:system-source-directory "coalton/tests")))

         (files (directory glob :resolve-symlinks nil)))

    (loop :for file :in files
          :do (signals parser:parse-error
                (parser:parse-file file))))

  (let* ((glob (merge-pathnames "tests/parser/*.good.coalton" (asdf:system-source-directory "coalton/tests")))

         (files (directory glob :resolve-symlinks nil)))

    (loop :for file :in files
          :do (parser:parse-file file))))
