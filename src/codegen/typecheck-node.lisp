(defpackage #:coalton-impl/codegen/typecheck-node
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:typecheck-node ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/typecheck-node)

(defgeneric typecheck-node (expr env)
  (:documentation "Check that EXPR is valid. Currently only verifies
  that applied functions match their arguments.")
  (:method ((expr node-literal) env)
    (declare (type tc:environment env)
             (ignore env)
             (values tc:ty))
    (node-type expr))

  (:method ((expr node-variable) env)
    (declare (type tc:environment env)
             (ignore env)
             (values tc:ty))
    (node-type expr))

  (:method ((expr node-application) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (assert (not (null (node-application-rands expr))))

    (let ((type (typecheck-node (node-application-rator expr) env))

          (subs nil))
      (loop :for arg :in (node-application-rands expr)
            :for arg-ty := (typecheck-node arg env) :do
              (progn
                (setf subs (tc:unify subs (tc:function-type-from type) arg-ty))
                (setf subs (tc:unify subs arg-ty (tc:function-type-from type)))
                (setf type (tc:function-type-to type))))
      (node-type expr)))

  (:method ((expr node-direct-application) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (assert (not (null (node-direct-application-rands expr))))

    (let ((type (node-direct-application-rator-type expr))

          (subs nil))
      (loop :for arg :in (node-direct-application-rands expr)
            :for arg-ty := (typecheck-node arg env) :do
              (progn
                (setf subs (tc:unify subs (tc:function-type-from type) arg-ty))
                (setf subs (tc:unify subs arg-ty (tc:function-type-from type)))
                (setf type (tc:function-type-to type))))
      (node-type expr)))

  (:method ((expr node-abstraction) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (assert (not (null (node-abstraction-vars expr))))

    (let ((type (node-type expr))

          (subs nil))
      (loop :for name :in (node-abstraction-vars expr) :do
        (progn
          (setf type (tc:function-type-to type))))

      (let ((subexpr-ty (typecheck-node (node-abstraction-subexpr expr) env)))
        (setf subs (tc:unify subs type subexpr-ty))
        (setf subs (tc:unify subs subexpr-ty type))
        (node-type expr))))

  (:method ((expr node-let) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (loop :for (name . node) :in (node-let-bindings expr) :do
      (typecheck-node node env))

    (let ((subexpr-ty (typecheck-node (node-let-subexpr expr) env))

          (subs nil))
      (setf subs (tc:unify subs subexpr-ty (node-type expr)))
      (setf subs (tc:unify subs (node-type expr) subexpr-ty))
      subexpr-ty))

  (:method ((expr node-lisp) env)
    (declare (type tc:environment env)
             (ignore env)
             (values tc:ty))
    (node-type expr))

  (:method ((expr match-branch) env)
    (declare (type tc:environment env)
             (values tc:ty &optional))
    (typecheck-node (match-branch-body expr) env))

  (:method ((expr node-match) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (let ((type (node-type expr))

          (subs nil))

      (loop :for branch :in (node-match-branches expr)
            :for subexpr-ty := (typecheck-node branch env) :do
              (progn
                (setf subs (tc:unify subs type subexpr-ty))
                (setf subs (tc:unify subs subexpr-ty type))))
      type))

  (:method ((expr node-seq) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (assert (not (null (node-seq-nodes expr))))
    (loop :for node :in (node-seq-nodes expr) :do
      (typecheck-node node env))

    (let ((last-node (car (last (node-seq-nodes expr))))

          (subs nil))
      (setf subs (tc:unify subs (node-type expr) (node-type last-node)))
      (setf subs (tc:unify subs (node-type last-node) (node-type expr)))
      (node-type last-node)))

  (:method ((expr node-return) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (typecheck-node (node-return-expr expr) env)
    (node-type expr))

  (:method ((expr node-field) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (typecheck-node (node-field-dict expr) env)
    (node-type expr))

  (:method ((expr node-dynamic-extent) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (typecheck-node (node-dynamic-extent-node expr) env)
    (tc:unify
     nil
     (node-type expr)
     (typecheck-node (node-dynamic-extent-body expr) env))
    (node-type expr))

  (:method ((expr node-bind) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (typecheck-node (node-bind-expr expr) env)
    (tc:unify
     nil
     (node-type expr)
     (typecheck-node (node-bind-body expr) env))
    (node-type expr)))


