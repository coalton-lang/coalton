(in-package #:coalton-tests)

(defun compile-test-file ()
  (merge-pathnames "examples/small-coalton-programs/src/fact-fib.coal"
                   (asdf:system-source-directory "coalton")))

(deftest test-compile-to-lisp ()
  "Test that the Coalton compiler compiles a test file into Lisp source."
  (let ((source (entry:compile-coalton (compile-test-file) :format ':source)))
    (with-input-from-string (stream source)
      (let ((prologue (read stream))
            (env (read stream))
            (defpkg (read stream))
            (inpkg (read stream))
            (defs (read stream)))
        (is (eql 'eval-when (first prologue)))
        (is (eql 'COALTON-IMPL/TYPECHECKER/ENVIRONMENT:SET-VALUE-TYPE
                 (first (third (first (cddr env))))))
        (is (eql 'defpackage (first defpkg)))
        (is (eql 'in-package (first inpkg)))
        (is (string= "FACT"
                     (symbol-name (second (fourth defs)))))))))

(deftest test-compile-to-fasl ()
  "Test that the Coalton compiler compiles a test file into a working .fasl that persists environment updates."
  (flet ((test-sym ()
           (let ((p (find-package "SMALL-COALTON-PROGRAMS/FACT-FIB")))
             (when p
               (intern "FACT" p)))))
    (let ((fact (test-sym)))
      (fmakunbound fact)
      (setf entry:*global-environment*
            (tc:unset-function entry:*global-environment* fact)))
    (let ((fasl-file (merge-pathnames (format nil "~A-test.fasl" (gensym))
                                      (uiop:temporary-directory))))
      (entry:compile-coalton (compile-test-file)
                             :output-file fasl-file
                             :format ':default)
      (load fasl-file :verbose t :print t)
      (let ((fact (test-sym)))
        (is (fboundp fact))
        (is (= 120 (funcall fact 5)))
        (is (equalp (tc:make-function-env-entry
                     :name fact
                     :arity 1)
                    (tc:lookup-function entry:*global-environment*
                                        fact
                                        :no-error t)))))))
