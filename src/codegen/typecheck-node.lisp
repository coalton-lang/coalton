(defpackage #:coalton-impl/codegen/typecheck-node
  (:use
   #:cl
   #:coalton-impl/util
   #:coalton-impl/codegen/ast)
  (:local-nicknames
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

    (let ((type (typecheck-node (node-application-rator expr) env)))
      (loop :for arg :in (node-application-rands expr)
            :for arg-ty := (typecheck-node arg env) :do
              (progn
                (tc:match (tc:function-type-from type) arg-ty)
                (tc:match arg-ty (tc:function-type-from type))
                (setf type (tc:function-type-to type))))
      (node-type expr)))

  (:method ((expr node-direct-application) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (assert (not (null (node-direct-application-rands expr))))

    (let ((type (node-direct-application-rator-type expr)))
      (loop :for arg :in (node-direct-application-rands expr)
            :for arg-ty := (typecheck-node arg env) :do
              (progn
                (tc:match (tc:function-type-from type) arg-ty)
                (tc:match arg-ty (tc:function-type-from type))
                (setf type (tc:function-type-to type))))
      (node-type expr)))

  (:method ((expr node-abstraction) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (assert (not (null (node-abstraction-vars expr))))

    (let ((type (node-type expr)))
      (loop :for name :in (node-abstraction-vars expr) :do
        (progn
          (setf type (tc:function-type-to type))))

      (let ((subexpr-ty (typecheck-node (node-abstraction-subexpr expr) env)))
        (tc:match type subexpr-ty)
        (tc:match subexpr-ty type)
        (node-type expr))))

  (:method ((expr node-bare-abstraction) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (assert (not (null (node-bare-abstraction-vars expr))))

    (let ((type (node-type expr)))
      (loop :for name :in (node-bare-abstraction-vars expr) :do
        (progn
          (setf type (tc:function-type-to type))))

      (let ((subexpr-ty (typecheck-node (node-bare-abstraction-subexpr expr) env)))
        (tc:match type subexpr-ty)
        (tc:match subexpr-ty type)
        (node-type expr))))

  (:method ((expr node-let) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (loop :for (name . node) :in (node-let-bindings expr) :do
      (typecheck-node node env))

    (let ((subexpr-ty (typecheck-node (node-let-subexpr expr) env)))
      (tc:match subexpr-ty (node-type expr))
      (tc:match (node-type expr) subexpr-ty)
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
    (let ((type (node-type expr)))
      (loop :for branch :in (node-match-branches expr)
            :for subexpr-ty := (typecheck-node branch env) :do
              (progn
                (tc:match type subexpr-ty)
                (tc:match subexpr-ty type)))
      type))

  (:method ((expr node-seq) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (assert (not (null (node-seq-nodes expr))))
    (loop :for node :in (node-seq-nodes expr) :do
      (typecheck-node node env))

    (let ((last-node (car (last (node-seq-nodes expr)))))
      (tc:match (node-type expr) (node-type last-node))
      (tc:match (node-type last-node) (node-type expr))
      (node-type last-node)))

  (:method ((expr node-return) env)
    (typecheck-node (node-return-expr expr) env)
    (node-type expr)))
