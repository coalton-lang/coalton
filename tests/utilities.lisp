(in-package #:coalton-tests)

(defun run-coalton-tests ()
  (run-package-tests
   :packages '(:coalton-tests
               :quil-coalton-tests
               :thih-coalton-tests)
   :interactive t))

(defun set-equalp (set1 set2)
  (null (set-exclusive-or set1 set2 :test #'equalp)))

(defun dag-equalp (dag1 dag2)
  ;; XXX: This will not check ordering of edges within vertices
  (set-equalp dag1 dag2))

(defun check-coalton-types (toplevel expected-types)
  (multiple-value-bind (form env)
      (coalton-impl::process-coalton-toplevel toplevel *package* coalton-impl::*global-environment*)
    (declare (ignore form))

    (loop :for (symbol . type) :in expected-types
          :do (is (coalton-impl/typechecker::type-scheme=
                   (tc:lookup-value-type env symbol)
                   (tc:parse-and-resolve-type env type))))))

(defun run-coalton-toplevel-walker (toplevel)
  (coalton-impl::collect-toplevel-forms toplevel))

(defun run-coalton-typechecker (toplevel)
  (coalton-impl::process-coalton-toplevel toplevel *package* coalton-impl::*global-environment*))

(defun compile-and-load-forms (coalton-forms)
  "Write the COALTON-FORMS to a temporary file, compile it to a fasl, then load the compiled file.

Returns (values SOURCE-PATHNAME COMPILED-PATHNAME)."
  (uiop:with-temporary-file (:stream out-stream
                             :pathname input-file
                             :suffix "lisp"
                             :direction :output
                             :keep t)
    (dolist (expr coalton-forms)
      (prin1 expr out-stream)
      (terpri out-stream))
    :close-stream
    (uiop:with-temporary-file (:pathname output-file
                               :type #+ccl (pathname-type ccl:*.fasl-pathname*)
                                     #+(not ccl) "fasl"
                               :keep t)
      (compile-file input-file :output-file output-file)
      (load output-file)
      (values input-file output-file))))

(defmacro with-coalton-compilation ((&key package (muffle 'cl:style-warning)) &body coalton-code)
  `(handler-bind
       ((,muffle #'muffle-warning))
     (compile-and-load-forms '(,@(when package `((cl:in-package ,package)))
                               ,@coalton-code))))
