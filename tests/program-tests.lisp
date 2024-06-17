(in-package #:coalton-tests)

(defun test-program (file)
  (merge-pathnames file
                   (merge-pathnames "tests/test-files/"
                                    (asdf:system-source-directory "coalton"))))

(deftest test-lisp-toplevel ()
  "LISP-TOPLEVEL form: test that embedded lisp is read into the correct package, and is callable."
  (with-open-file (stream (test-program "lisp-toplevel.coal"))
    (entry:compile stream))
  (let ((pkg (find-package "COALTON-TEST-PROGRAMS/LISP-TOPLEVEL")))
    (is (funcall (find-symbol "MY-ZERO-P" pkg) 0))
    (is (= 120 (funcall (find-symbol "FACT" pkg) 5)))))
