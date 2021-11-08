(in-package #:coalton-tests)

(defun run-coalton-tests ()
  (run-package-tests
   :packages '(:coalton-tests
               :quil-coalton-tests
               :thih-coalton-tests
               :coalton-json-tests)
   :interactive t))

(defun set-equalp (set1 set2)
  (null (set-exclusive-or set1 set2 :test #'equalp)))

(defun dag-equalp (dag1 dag2)
  ;; XXX: This will not check ordering of edges within vertices
  (set-equalp dag1 dag2))

(defun check-coalton-types (toplevel expected-types)
  (multiple-value-bind (form env)
      (coalton-impl::process-coalton-toplevel toplevel coalton-impl::*initial-environment*)
    (declare (ignore form))

    (loop :for (symbol . type) :in expected-types
          :do (is (coalton-impl/typechecker::type-scheme=
                   (coalton-impl::lookup-value-type env symbol)
                   (coalton-impl/typechecker::parse-and-resolve-type env type))))))

(defun run-coalton-typechecker (toplevel)
  (coalton-impl::process-coalton-toplevel toplevel coalton-impl::*initial-environment*))

(defun reintern (form package)
  (typecase form
    (keyword form)
    (symbol (intern (symbol-name form) package))
    (list (mapcar (lambda (subform) (reintern subform package))
                  form))
    (t form)))

(defun compile-and-load-tempfile (package-name form
                                  &aux (package (find-package package-name)))
  (uiop:with-safe-io-syntax (:package package) 
    (uiop:with-temporary-file (:stream out
                               :pathname source-file
                               :type "lisp"
                               :direction :output)
      (dolist (expr form)
        (prin1 (reintern expr package) out))
      :close-stream
      (uiop:with-temporary-file (:pathname compiled-file
                                 :type "fasl")
        (handler-bind ((style-warning #'muffle-warning)
                       #+sbcl (sb-ext:compiler-note #'muffle-warning))
          (compile-file source-file
                        :output-file compiled-file
                        :print nil
                        :verbose nil))
        (load compiled-file)))))

(defmacro with-toplevel-compilation ((&key (package :coalton-test-user)) &body body)
  `(compile-and-load-tempfile ',package ',body))
