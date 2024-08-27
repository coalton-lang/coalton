(in-package #:coalton-tests)

(defun check-package (string fn)
  "Parse the package form present in STRING."
  (let ((source (source:make-source-string string :name "test")))
    (with-open-stream (stream (source:source-stream source))
      (parser:with-reader-context stream
        (let* ((form (parser:maybe-read-form stream parser::*coalton-eclector-client*))
               (package (coalton-impl/parser/toplevel::parse-package
                         (coalton-impl/parser/cursor:make-cursor source form))))
          (funcall fn package))))))

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

    (check-package
     "(package coalton-unit-test/package-a
        (export a b c))"
     (lambda (pkg-a)
       (let ((lisp-pkg-a (coalton-impl/parser/toplevel::lisp-package pkg-a)))
         (is (= 3 (length (ext-syms lisp-pkg-a))))
         (is (equal '("COALTON")
                    (use-pkgs lisp-pkg-a))))))

    (check-package
     "(package coalton-unit-test/package-b
        (import coalton-unit-test/package-a
          (coalton-library/list as list))
        (export d e f))"
     (lambda (pkg-b)
       (let ((lisp-pkg-b (coalton-impl/parser/toplevel::lisp-package pkg-b)))
         (is (= 3 (length (ext-syms lisp-pkg-b))))
         (is (equal '("COALTON" "COALTON-UNIT-TEST/PACKAGE-A")
                    (use-pkgs lisp-pkg-b))))))

    (check-package
     "(package coalton-unit-test/package-c
        (shadow not))"
     (lambda (pkg-c)
       (let ((lisp-pkg-c (coalton-impl/parser/toplevel::lisp-package pkg-c)))
         (is (= 1 (length (package-shadowing-symbols lisp-pkg-c))))
         (is (equal "NOT"
                    (symbol-name (first
                                  (package-shadowing-symbols lisp-pkg-c))))))))))
