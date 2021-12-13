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

(defun run-coalton-walker (toplevel)
  (coalton-impl::collect-toplevel-forms toplevel))

(defun run-coalton-typechecker (toplevel)
  (coalton-impl::process-coalton-toplevel toplevel coalton-impl::*initial-environment*))
