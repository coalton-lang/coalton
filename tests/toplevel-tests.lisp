(in-package #:coalton-tests)

(defun parse-form (string fn)
  "Parse the form in STRING."
  (let ((source (source:make-source-string string :name "test")))
    (with-open-stream (stream (source:source-stream source))
      (parser:with-reader-context stream
        (funcall fn (parser:maybe-read-form stream parser::*coalton-eclector-client*) source)))))

(defun parse-package (string)
  (parse-form string
              (lambda (form source)
                (coalton-impl/parser/toplevel::parse-package
                 (coalton-impl/parser/cursor:make-cursor form source "Unit Test")))))

(deftest test-lisp-package ()
  "Lisp packages can be constructed from parsed Coalton package forms."
  (flet ((del-pkg (package-designator)
           (when (find-package package-designator)
             (delete-package package-designator)))
         (ext-syms (p)
           (let ((symbols nil))
             (do-external-symbols (s p)
               (pushnew s symbols))
             symbols))
         (use-pkgs (p)
           (sort (mapcar #'package-name (package-use-list p)) #'string<)))

    (del-pkg 'coalton-unit-test/package-b)
    (del-pkg 'coalton-unit-test/package-a)
    (del-pkg 'coalton-unit-test/package-c)

    (let* ((pkg-a (parse-package
                   "(package coalton-unit-test/package-a
                     (export a b c))"))
           (lisp-pkg-a (coalton-impl/parser/toplevel::lisp-package pkg-a)))
      (is (= 3 (length (ext-syms lisp-pkg-a))))
      (is (equal '("COALTON")
                 (use-pkgs lisp-pkg-a))))

    (let* ((pkg-b (parse-package
     "(package coalton-unit-test/package-b
        (import coalton-unit-test/package-a
          (coalton-library/list as list))
        (export d e f))"))
           (lisp-pkg-b (coalton-impl/parser/toplevel::lisp-package pkg-b)))
      (is (= 3 (length (ext-syms lisp-pkg-b))))
      (is (equal '("COALTON" "COALTON-UNIT-TEST/PACKAGE-A")
                 (use-pkgs lisp-pkg-b))))

    (let* ((pkg-c (parse-package
     "(package coalton-unit-test/package-c
        (shadow not))"))
           (lisp-pkg-c (coalton-impl/parser/toplevel::lisp-package pkg-c)))
      (is (= 1 (length (package-shadowing-symbols lisp-pkg-c))))
      (is (equal "NOT"
                 (symbol-name (first
                               (package-shadowing-symbols lisp-pkg-c))))))))
