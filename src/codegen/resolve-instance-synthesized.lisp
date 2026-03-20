(defpackage #:coalton-impl/codegen/resolve-instance-synthesized
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:import-from
   #:coalton-impl/codegen/resolve-instance
   #:pred-context
   #:pred-type
   #:register-static-dict-resolver))

;;;; Support for compiler-synthesized static dictionaries for type shapes that
;;;; cannot be expressed as ordinary source-level instances.
;;;;
;;;; This file does not export a public interface of its own. Its purpose is to
;;;; define `resolve-synthesized-static-dict` and register it in
;;;; `coalton-impl/codegen/resolve-instance`'s `*static-dict-resolvers*`
;;;; registry as a load-time side effect.
;;;;
;;;; Today this file synthesizes `RuntimeRepr` dictionaries for general
;;;; function types such as `(Integer * String -> Boolean * Char)`. Coalton can
;;;; represent those types internally, but there is no ordinary source-level
;;;; instance head that can quantify over arbitrary function arity and result
;;;; arity in that shape.
;;;;
;;;; A plausible future use for this file would be other compiler-only
;;;; structural types that are not directly nameable in source. For example, if
;;;; Coalton later grows row-like internal types for records or effects, they
;;;; might need synthesized dictionaries here for classes like `RuntimeRepr`
;;;; without exposing special-purpose source instances.

(in-package #:coalton-impl/codegen/resolve-instance-synthesized)

(defun void-node-type ()
  (tc:make-result-ty :output-types nil))

(defun proxy-symbol ()
  (util:find-symbol "PROXY" "COALTON/TYPES"))

(defun lisp-type-type (env)
  (tc:type-entry-type
   (tc:lookup-type env (util:find-symbol "LISPTYPE" "COALTON/TYPES"))))

(defun proxy-type (type env)
  (nth-value
   0
   (tc:apply-type-argument
    (tc:type-entry-type
     (tc:lookup-type env (proxy-symbol)))
    type)))

(defun make-synthesized-runtime-repr-dict (pred env)
  (declare (type tc:ty-predicate pred)
           (type tc:environment env)
           (values node &optional))
  (let* ((runtime-repr-class (tc:lookup-class env (tc:ty-predicate-class pred)))
         (proxy-var (gensym "PROXY-"))
         (method-type
           (tc:make-function-type
            (proxy-type (first (tc:ty-predicate-types pred)) env)
            (lisp-type-type env)))
         (method-node
           (make-node-abstraction
            :type method-type
            :vars (list proxy-var)
            :keyword-params nil
            :subexpr (make-node-lisp
                      :type (lisp-type-type env)
                      :vars nil
                      :form (list
                             (util:runtime-quote
                              'coalton-impl/runtime/function-entry:function-entry))))))
    (make-node-application
     :type (pred-type pred env)
     :properties '()
     :rator (make-node-variable
             :type (tc:make-function-type (node-type method-node) (pred-type pred env))
             :value (tc:ty-class-codegen-sym runtime-repr-class))
     :rands (list method-node)
     :keyword-rands nil)))

(defun resolve-synthesized-static-dict (pred context env)
  (declare (type tc:ty-predicate pred)
           (type pred-context context)
           (type tc:environment env)
           (values (or null node) boolean &optional))
  (declare (ignore context))
  (multiple-value-bind (_ foundp)
      (tc:synthesized-class-instance-constraints pred)
    (declare (ignore _))
    (unless foundp
      (return-from resolve-synthesized-static-dict (values nil nil)))
    (let ((runtime-repr (util:find-symbol "RUNTIMEREPR" "COALTON/TYPES")))
      (if (eq (tc:ty-predicate-class pred) runtime-repr)
          (values (make-synthesized-runtime-repr-dict pred env) t)
          (values nil nil)))))

(register-static-dict-resolver 'resolve-synthesized-static-dict)
