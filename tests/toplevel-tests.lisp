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
          (coalton/list as list))
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

(defun declaim-under-locally-p (form &optional under-locally-p)
  (cond
    ((atom form)
     nil)
    ((and under-locally-p
          (eq 'cl:declaim (first form)))
     t)
    (t
     (let ((under-locally-p (or under-locally-p
                                (eq 'cl:locally (first form)))))
       (some (lambda (subform)
               (declaim-under-locally-p subform under-locally-p))
             form)))))

(deftest test-generated-declaims-remain-top-level ()
  "Generated DECLAIM forms must not be nested inside LOCALLY."
  (let* ((package-name (format nil "COALTON-DECL-TOPLEVEL-~A" (gensym)))
         (package (make-package package-name
                                :use '("COALTON" "COALTON-PRELUDE"))))
    (unwind-protect
         (let* ((*package* package)
                (source (source:make-source-string
                         "(declare *width* Integer)
                          (define *width* 400)

                          (declare *aspect-ratio* Fraction)
                          (define *aspect-ratio* 16/9)

                          (declare height-from-aspect-ratio
                            (Integer * Fraction -> Integer))
                          (define (height-from-aspect-ratio width ratio)
                            (floor (/ (into width) ratio)))"
                         :name "test"))
                (program
                  (with-open-stream (stream (source:source-stream source))
                    (parser:with-reader-context stream
                      (parser:read-program stream source))))
                (lisp-form (entry:entry-point program)))
           (is (not (declaim-under-locally-p lisp-form))))
      (delete-package package))))

(defun run-file-program-in-fresh-package (package-name text)
  (when (find-package package-name)
    (delete-package package-name))
  (unwind-protect
       (let ((entry:*global-environment* (tc:make-default-environment))
             (source (source:make-source-string text :name "test")))
         (with-open-stream (stream (source:source-stream source))
           (parser:with-reader-context stream
             (entry:entry-point (parser:read-program stream source ':file)))))
    (when (find-package package-name)
      (delete-package package-name))))

(deftest exported-define-without-declare-signals-deprecation-warning ()
  "Exported definitions without declares are currently deprecated."
  (let* ((package-name (format nil "COALTON-EXPORTED-NO-DECLARE-~A" (gensym)))
         (captured nil))
    (handler-bind ((coalton:deprecation-warning
                     (lambda (condition)
                       (setf captured condition)
                       (muffle-warning condition))))
      (run-file-program-in-fresh-package
       package-name
       (format nil "(package ~A~%  (export f))~%~%(define (f x) x)"
               package-name)))
    (is captured)
    (is (typep captured 'style-warning))
    (is (typep captured 'source:source-warning))))

(deftest exported-define-with-declare-does-not-signal-deprecation-warning ()
  "Exported definitions with declares do not warn."
  (let* ((package-name (format nil "COALTON-EXPORTED-WITH-DECLARE-~A" (gensym)))
         (captured nil))
    (handler-bind ((coalton:deprecation-warning
                     (lambda (condition)
                       (setf captured condition)
                       (muffle-warning condition))))
      (run-file-program-in-fresh-package
       package-name
       (format nil "(package ~A~%  (export f))~%~%(declare f (:a -> :a))~%(define (f x) x)"
               package-name)))
    (is (null captured))))

(deftest exported-function-valued-define-without-declare-signals-deprecation-warning ()
  "Function-valued definitions also require declares."
  (let* ((package-name (format nil "COALTON-EXPORTED-FN-VALUE-NO-DECLARE-~A" (gensym)))
         (captured nil))
    (handler-bind ((coalton:deprecation-warning
                     (lambda (condition)
                       (setf captured condition)
                       (muffle-warning condition))))
      (run-file-program-in-fresh-package
       package-name
       (format nil "(package ~A~%  (export f))~%~%(define f (fn (x) x))"
               package-name)))
    (is captured)
    (is (typep captured 'style-warning))))

(deftest exported-value-without-declare-signals-deprecation-warning ()
  "All exported definitions require declares, including non-functions."
  (let* ((package-name (format nil "COALTON-EXPORTED-VALUE-NO-DECLARE-~A" (gensym)))
         (captured nil))
    (handler-bind ((coalton:deprecation-warning
                     (lambda (condition)
                       (setf captured condition)
                       (muffle-warning condition))))
      (run-file-program-in-fresh-package
       package-name
       (format nil "(package ~A~%  (export x))~%~%(define x \"value\")"
               package-name)))
    (is captured)
    (is (typep captured 'style-warning))))

(deftest exported-value-with-declare-does-not-signal-deprecation-warning ()
  "Declared exported non-function definitions do not warn."
  (let* ((package-name (format nil "COALTON-EXPORTED-VALUE-WITH-DECLARE-~A" (gensym)))
         (captured nil))
    (handler-bind ((coalton:deprecation-warning
                     (lambda (condition)
                       (setf captured condition)
                       (muffle-warning condition))))
      (run-file-program-in-fresh-package
       package-name
       (format nil "(package ~A~%  (export x))~%~%(declare x String)~%(define x \"value\")"
               package-name)))
    (is (null captured))))

(deftest exported-define-without-declare-can-be-configured-as-error ()
  "The deprecation can be promoted to an error for migration testing."
  (let ((package-name (format nil "COALTON-EXPORTED-DECLARE-ERROR-~A" (gensym)))
        (coalton-impl/settings:*coalton-deprecation-warnings-as-errors* t))
    (signals coalton-impl/typechecker:tc-error
      (run-file-program-in-fresh-package
       package-name
       (format nil "(package ~A~%  (export f))~%~%(define (f x) x)"
               package-name)))))
