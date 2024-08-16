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

(defun constant-var-value (var constant-bindings &key no-error)
  (declare (type symbol var)
           (values (or node null) &optional))
  (let ((may-be-value (cdr (assoc var constant-bindings))))
    (cond (may-be-value
           ;; Whenever it exists, it is always a NODE (thus, non-NIL)
           may-be-value)
          (no-error
           nil)
          (t
           (error "Expected VAR ~S to be a constant but is not" var)))))

(defun propagate-constants (node env)
  (declare (optimize debug))
  (labels ((constant-node-p (node constant-bindings)
             ;; FIXME: Do we need more nodes classified as constants?
             (or (node-literal-p node)
                 (and (node-variable-p node)
                      (tc:lookup-instance-by-codegen-sym env (node-variable-value node) :no-error t))
                 (and (node-variable-p node)
                      (constant-var-value (node-variable-value node) constant-bindings :no-error t))))
           (propagate-constants-node-variable (node constant-bindings)
             ;; FIXME: Do we need more nodes classified as constants?
             (declare (type node-variable node))
             (if (tc:lookup-instance-by-codegen-sym env (node-variable-value node) :no-error t)
                 node
                 (constant-var-value (node-variable-value node) constant-bindings :no-error t)))

           (propagate-constants-node-let (node constant-bindings)
             (declare (type node-let node))
             (let ((node-bindings (node-let-bindings node))
                   (new-constant-bindings nil)
                   (nonconstant-bindings nil))
               (loop :for (var . value) :in node-bindings
                     :for propagated-value-node := (funcall *traverse* value constant-bindings)
                     :do (if (constant-node-p propagated-value-node constant-bindings)
                             (push (cons var propagated-value-node)
                                   new-constant-bindings)
                             (push (cons var propagated-value-node)
                                   nonconstant-bindings)))
               (let ((inner-constant-bindings (append new-constant-bindings constant-bindings)))
                 (if nonconstant-bindings
                     (make-node-let
                      :type (node-type node)
                      :bindings nonconstant-bindings
                      :subexpr (funcall *traverse* (node-let-subexpr node) inner-constant-bindings))
                     (funcall *traverse* (node-let-subexpr node) inner-constant-bindings)))))

           (propagate-constants-node-lisp (node constant-bindings)
             (declare (type node-lisp node))
             (let* ((new-lisp-vars nil)
                    (let-bindings
                      (loop :for (lisp-var . coalton-var) :in (node-lisp-vars node)
                            :for constant-value := (constant-var-value coalton-var constant-bindings :no-error t)
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

           (direct-application-better-infer-types (node constant-bindings)
             (declare (type node-direct-application node)
                      (ignore constant-bindings))
             ;; FIXME: Can we do something similar for non-direct APPLICATION?
             (make-node-direct-application
              :type (node-type node)
              :rator-type (tc:make-function-type*
                           (mapcar #'node-type (node-direct-application-rands node))
                           (tc:function-return-type (node-type node)))
              :rator (node-direct-application-rator node)
              :rands (node-direct-application-rands node))))

    (traverse
     node
     (list
      (action (:after node-variable) #'propagate-constants-node-variable)
      (action (:traverse node-let) #'propagate-constants-node-let)
      (action (:after node-lisp) #'propagate-constants-node-lisp)
      (action (:after node-direct-application) #'direct-application-better-infer-types))
     nil)))
