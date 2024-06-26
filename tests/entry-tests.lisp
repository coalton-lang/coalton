(in-package #:coalton-tests)

(defun compile-test-file ()
  (test-file "examples/small-coalton-programs/src/classical.coal"))

(deftest test-compile-to-lisp ()
  "Test that the Coalton compiler compiles a test file into Lisp source."
  (let ((source (entry:compile-coalton (compile-test-file) :format ':source)))
    (with-input-from-string (stream source)
      (let ((prologue (read stream))
            (env (read stream))
            (defs (read stream)))
        (is (eql 'eval-when (first prologue)))
        (is (eql 'COALTON-IMPL/TYPECHECKER/ENVIRONMENT:SET-VALUE-TYPE
                 (first (third (first (cddr env))))))
        (is (string= "FACT"
                     (symbol-name (second (fourth defs)))))))))

;; Package needs definition outside of .coal test file until the
;; native package form supports the definition of imports and exports.

(defpackage #:small-coalton-programs/classical
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/math)
  (:local-nicknames (#:list #:coalton-library/list))
  (:export
   #:fact))

(deftest test-compile-to-fasl ()
  "Test that the Coalton compiler compiles a test file into a working .fasl that persists environment updates."
  (fmakunbound 'small-coalton-programs/classical:fact)
  (setf entry:*global-environment*
        (tc:unset-function entry:*global-environment*
                           'small-coalton-programs/classical:fact))
  (let ((fasl-file (merge-pathnames (format nil "~A-test.fasl" (gensym))
                                    (uiop:temporary-directory))))
    (entry:compile-coalton (compile-test-file)
                           :output-file fasl-file
                           :format ':default)
    (load fasl-file :verbose t :print t)
    (is (= 120 (small-coalton-programs/classical:fact 5)))
    (is (equalp (tc:make-function-env-entry
                 :name 'small-coalton-programs/classical:fact
                 :arity 1)
                (tc:lookup-function entry:*global-environment*
                                   'small-coalton-programs/classical:fact
                                   :no-error t)))))
