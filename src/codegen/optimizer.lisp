(in-package #:coalton-impl/codegen)

(serapeum:defstruct-read-only (optimizer
                               (:constructor optimizer (env toplevel-functions)))
  (env :type environment)
  (toplevel-functions :type list))

(defun make-optimizer (env)
  (declare (type environment env)
           (values optimizer))
  (optimizer env (directly-applicable-functions env)))

(defun optimize-node (optimizer node)
  (declare (type optimizer optimizer)
           (type typed-node node)
           (values typed-node))

  ;; Ensure that the node is valid
  (coalton-impl/typechecker::check-node-type node (optimizer-env optimizer))

  (let* ((node (pointfree-transform node optimizer))
         (node (direct-application-transform node optimizer))
         (node (match-constructor-lift-transform node optimizer)))


    ;; Ensure that the node is still valid after transformations are
    ;; applied
    (coalton-impl/typechecker::check-node-type node (optimizer-env optimizer))
    node))

(defun optimize-bindings (optimizer bindings)
  (declare (type optimizer optimizer)
           (type typed-binding-list bindings))

  (let ((optimized-bindings nil))
    (loop :for (name . node) :in bindings
          :do (push (cons name (optimize-node optimizer node)) optimized-bindings))
    optimized-bindings))
