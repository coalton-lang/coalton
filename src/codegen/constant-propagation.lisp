(defpackage #:coalton-impl/codegen/constant-propagation
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker))
  (:import-from
   #:coalton-impl/codegen/traverse
   #:action
   #:count-applications
   #:count-nodes
   #:make-traverse-let-action-skipping-cons-bindings
   #:*traverse*
   #:traverse
   #:traverse-with-binding-list)
  (:export
   #:propagate-constants))

(in-package #:coalton-impl/codegen/constant-propagation)

(defvar *constant-bindings*)
(setf (documentation '*constant-bindings* 'variable)
      "Bound inside CONSTANT-PROPAGATION.")

(defun constant-var-value (var &key no-error)
  (the (or node null)
       (or (cdr (assoc var *constant-bindings*))
           (unless no-error
             (error "Expected VAR ~S to be a constant but is not" var)))))

(defun propagate-constants (node env)
  (declare (optimize debug))
  (labels ((constant-node-p (node)
             ;; FIXME: Do we need more nodes classified as constants?
             (or (node-literal-p node)
                 (and (node-variable-p node)
                      (tc:lookup-instance-by-codegen-sym env (node-variable-value node) :no-error t))
                 (and (node-variable-p node)
                      (constant-var-value (node-variable-value node) :no-error t))))
           (constant-node-value (node &key no-error)
             ;; FIXME: Do we need more nodes classified as constants?
             (cond ((node-literal-p node)
                    node)
                   ((and (node-variable-p node)
                         (tc:lookup-instance-by-codegen-sym env (node-variable-value node) :no-error t))
                    node)
                   ((node-variable-p node)
                    (constant-var-value (node-variable-value node) :no-error no-error))
                   (no-error
                    nil)
                   (t
                    (error "Expected the node to be a constant but is not ~%~S" node))))

           (propagate-constants-node-variable (node call-stack)
             (declare (ignore call-stack)
                      (type node-variable node))
             (or (constant-node-value node :no-error t)
                 node))

           (propagate-constants-node-let (node call-stack)
             (declare (type node-let node))
             (let ((node-bindings (node-let-bindings node))
                   (new-constant-bindings nil)
                   (nonconstant-bindings nil))
               (loop :for (var . value) :in node-bindings
                     :for propagated-value-node := (funcall *traverse* value call-stack)
                     :do (if (constant-node-p propagated-value-node)
                             (push (cons var propagated-value-node)
                                   new-constant-bindings)
                             (push (cons var propagated-value-node)
                                   nonconstant-bindings)))
               (let ((*constant-bindings* (append new-constant-bindings *constant-bindings*)))
                 (if nonconstant-bindings
                     (make-node-let
                      :type (node-type node)
                      :bindings nonconstant-bindings
                      :subexpr (funcall *traverse* (node-let-subexpr node) call-stack))
                     (funcall *traverse* (node-let-subexpr node) call-stack)))))

           (propagate-constants-node-lisp (node call-stack)
             (declare (type node-lisp node)
                      (ignore call-stack))
             (let* ((new-lisp-vars nil)
                    (let-bindings
                      (loop :for (lisp-var . coalton-var) :in (node-lisp-vars node)
                            :for constant-value := (constant-var-value coalton-var :no-error t)
                            :if constant-value
                              :collect (let ((new-coalton-var (gentemp (symbol-name coalton-var))))
                                         (push (cons lisp-var new-coalton-var) new-lisp-vars)
                                         (cons new-coalton-var constant-value))
                            :else
                              :do (push (cons lisp-var coalton-var) new-lisp-vars))))
               (if let-bindings
                   (make-node-let
                    :type (node-type node)
                    :bindings let-bindings
                    :subexpr (make-node-lisp
                              :type (node-type node)
                              :vars new-lisp-vars
                              :form (node-lisp-form node)))
                   (make-node-lisp
                    :type (node-type node)
                    :vars new-lisp-vars
                    :form (node-lisp-form node)))))

           (direct-application-better-infer-types (node call-stack)
             ;; FIXME: Can we do something similar for non-direct APPLICATION?
             (let ((constant-propagated-rands
                     (mapcar (lambda (rand)
                               (funcall *traverse* rand call-stack))
                             (node-direct-application-rands node))))
               (make-node-direct-application
                :type (node-type node)
                :rator-type (tc:make-function-type*
                             (mapcar #'node-type constant-propagated-rands)
                             (tc:function-return-type (node-type node)))
                :rator (node-direct-application-rator node)
                :rands constant-propagated-rands))))

    (let ((*constant-bindings* nil))
      (traverse
       node
       (list
        (action (:after node-variable) #'propagate-constants-node-variable)
        (action (:after node-let) #'propagate-constants-node-let)
        (action (:after node-lisp) #'propagate-constants-node-lisp)
        (action (:after node-direct-application) #'direct-application-better-infer-types)
        (make-traverse-let-action-skipping-cons-bindings))
       nil))))
