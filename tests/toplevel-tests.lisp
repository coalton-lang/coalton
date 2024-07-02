(in-package #:coalton-tests)

(defun parse-package (string)
  "Parse the package form pressent in STRING."
  (with-input-from-string (stream string)
    (parser:with-reader-context stream
      (coalton-impl/parser/toplevel::parse-package
       (parser:maybe-read-form stream parser::*coalton-eclector-client*)
       (se:make-file :stream stream :name "<string>")))))

(deftest test-parse-package ()
  "Coalton toplevel package forms are successfully parsed."
  (is (parse-package
       "(package coalton-unit-test/lib-example-simple)"))
  (is (parse-package
       "(package coalton-unit-test/lib-example-complex
          (import
            coalton-library/classes
            (coalton-library/hash as hash))
          (import-from
            coalton-library/list
            filter)
          (export
            first
            second
            third))")))

(deftest test-lisp-package ()
  "Lisp packages can be constructed from parsed Coalton package forms."
  (flet ((del-pkg (package-designator)
           (when (find-package package-designator)
             (delete-package package-designator)))
         (ext-syms (p)
           (let (l)
             (do-external-symbols (s p)
               (pushnew s l))
             l))
         (use-pkgs (p)
           (sort (mapcar #'package-name (package-use-list p)) #'string<)))

    (del-pkg 'coalton-unit-test/package-b)
    (del-pkg 'coalton-unit-test/package-a)

    (let* ((pkg-a (parse-package
                   "(package coalton-unit-test/package-a
                      (export a b c))"))
           (lisp-pkg-a (coalton-impl/parser/toplevel::lisp-package pkg-a)))
      (is (= 3 (length (ext-syms lisp-pkg-a))))
      (is (equal '("COALTON")
                 (use-pkgs lisp-pkg-a)))
      (let* ((pkg-b (parse-package
                     "(package coalton-unit-test/package-b
                        (import coalton-unit-test/package-a
                          (coalton-library/list as list))
                        (export d e f))"))
             (lisp-pkg-b (coalton-impl/parser/toplevel::lisp-package pkg-b)))
        (is (= 3 (length (ext-syms lisp-pkg-b))))
        (is (equal '("COALTON" "COALTON-UNIT-TEST/PACKAGE-A")
                   (use-pkgs lisp-pkg-b)))))))

(deftest test-lisp-package-error ()
  "An error is signaled when attempting to construct packages with missing dependencies."
  (signals error
    (let ((package (parse-package
                    "(package coalton-unit-test/package-c
                       (import coalton-unit-test/package-d))")))
      (coalton-impl/parser/toplevel::lisp-package package))))
