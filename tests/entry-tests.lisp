(in-package #:coalton-tests)

(defun compile-test-file ()
  (test-file "examples/small-coalton-programs/src/fact-fib.coal"))

(defun source-forms (string)
  (let ((*readtable* (copy-readtable nil)))
    (with-input-from-string (stream string)
      (loop :for form := (read stream nil nil)
            :while form
            :collect form))))

(deftest test-compile-to-lisp ()
  "Test that the Coalton compiler compiles a test file into something that looks like Lisp source."
  (let* ((file (source:make-source-file (compile-test-file)))
         (source-form-types (mapcar #'first
                                    (source-forms (entry:codegen file)))))
    (dolist (expect-type '(defpackage in-package eval-when locally))
      (is (position expect-type source-form-types)
          "Missing expected ~A form in generated code" expect-type))))

(deftest test-compile-to-fasl ()
  "Test that the Coalton compiler compiles a test file into a working .fasl that persists environment updates."
  (flet ((test-sym ()
           (let ((p (find-package "SMALL-COALTON-PROGRAMS/FACT-FIB")))
             (when p
               (intern "FACT" p)))))
    (let ((fact (test-sym)))
      (fmakunbound fact)
      (setf entry:*global-environment* (tc:unset-function entry:*global-environment* fact)))
    (let ((file (source:make-source-file (compile-test-file) :name "test")))
      (let ((*readtable* (copy-readtable nil)))
        (entry:compile file :load t))
      (let ((fact (test-sym)))
        (is (fboundp fact)
            "Test function was bound as side effect of loading fasl")
        (is (= 120 (funcall fact 5))
            "Test function is callable")
        (is (equalp (tc:make-function-env-entry :name fact :arity 1 :inline-p nil)
                    (tc:lookup-function entry:*global-environment* fact :no-error t))
            "Environment was restored")))))
