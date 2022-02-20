(in-package #:coalton-impl/codegen)

(defstruct
    (optimizer
     (:constructor optimizer (env toplevel-functions)))
  (env                (required 'env)                :type environment :read-only t)
  (toplevel-functions (required 'toplevel-functions) :type list        :read-only t))

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

  ;; Run direct application twice so that options for direct
  ;; application are not removed by the pointfree transform
  (let* ((node (direct-application-transform node optimizer))
         ;; Classes cannot be stack allocated
         (node (pointfree-transform node optimizer))
         (node (direct-application-transform node optimizer))
         (node (if (coalton-impl:coalton-release-p)
                   (match-constructor-lift-transform node optimizer)
                   node))
         (node (if coalton-impl:*coalton-compile-hook*
                   (funcall coalton-impl:*coalton-compile-hook* node)
                   node))) 



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
