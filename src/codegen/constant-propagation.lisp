(defpackage #:coalton-impl/codegen/constant-propagation
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:propagate-constants))

(in-package #:coalton-impl/codegen/constant-propagation)

(defun constant-node-p (env node constant-bindings)
  (declare (type node node))
  ;; FIXME: We need more nodes classified as constants
  (cond ((node-literal-p node)
         t)
        ((and (node-variable-p node)
              (tc:lookup-instance-by-codegen-sym env (node-variable-value node) :no-error t))
         t)
        ((node-variable-p node)
         (let* ((name (node-variable-value node))
                (variable-value-node
                  (or (cdr (assoc name  constant-bindings))
                      (tc:lookup-function env name :no-error t))))
           (when variable-value-node
             (constant-node-p env variable-value-node constant-bindings))))
        (t
         nil)))

(defun constant-var-value (var constant-bindings)
  (cdr (assoc var constant-bindings)))

(defun constant-node-value (env node constant-bindings)
  (declare (type node node))
  ;; FIXME: We need more nodes classified as constants
  (cond ((node-literal-p node)
         node)
        ((and (node-variable-p node)
              (tc:lookup-instance-by-codegen-sym env (node-variable-value node) :no-error t))
         node)
        ((node-variable-p node)
         (let ((variable-value-node
                 (or (constant-var-value (node-variable-value node) constant-bindings)
                     (let ((fn (tc:lookup-function env (node-variable-value node) :no-error t)))
                       (if (and fn (tc:function-env-entry-inline-p fn))
                           (tc:lookup-code env (tc:function-env-entry-name fn) :no-error t)
                           nil)))))
           (if variable-value-node
               (constant-node-value env variable-value-node constant-bindings)
               nil)))
        (t
         nil)))

(defun propagate-constants-with-bindings (env node constant-bindings)
  "Returns two values: the first is the propagted value, the second indicates success if non-NIl"
  ;; (terpri)
  ;; (print (list node bindings))
  (cond
    ((node-literal-p node)
     node)
    ((node-variable-p node)
     ;; FIXME: Can or Should we use an existing structure to handle bindings
     (or (constant-node-value env node constant-bindings)
         node))
    ((node-let-p node)
     (let ((node-bindings (node-let-bindings node))
           (new-constant-bindings nil)
           (nonconstant-bindings nil))
       (loop :for (var . value) :in node-bindings
             :for propagated-value-node := (propagate-constants-with-bindings env value constant-bindings)
             :do (if (constant-node-p env propagated-value-node constant-bindings)
                     (push (cons var propagated-value-node)
                           new-constant-bindings)
                     (push (cons var propagated-value-node)
                           nonconstant-bindings)))
       (let ((constant-bindings (append new-constant-bindings constant-bindings)))
         (if nonconstant-bindings
             (make-node-let
              :type (node-type node)
              :bindings nonconstant-bindings
              :subexpr (propagate-constants-with-bindings
                        env
                        (node-let-subexpr node)
                        constant-bindings))
             (propagate-constants-with-bindings
              env
              (node-let-subexpr node)
              constant-bindings)))))
    ((node-abstraction-p node)
     (make-node-abstraction
      ;; FIXME: A more specific type?
      :type (node-type node)
      :vars (node-abstraction-vars node)
      :subexpr (propagate-constants-with-bindings
                env
                (node-abstraction-subexpr node)
                constant-bindings)))
    ((node-application-p node)
     (let ((constant-propagated-rands
             (mapcar (lambda (rand)
                       (propagate-constants-with-bindings
                        env
                        rand
                        constant-bindings))
                     (node-application-rands node))))
       (make-node-application
        ;; FIXME: A more specific type
        :type (node-type node)
        :rator (node-application-rator node)
        :rands constant-propagated-rands)))
    ((node-direct-application-p node)
     (let ((constant-propagated-rands
             (mapcar (lambda (rand)
                       (propagate-constants-with-bindings
                        env
                        rand
                        constant-bindings))
                     (node-direct-application-rands node))))
       (make-node-direct-application
        :type (node-type node)
        :rator-type (tc:make-function-type*
                     (mapcar #'node-type constant-propagated-rands)
                     (tc:function-return-type (node-type node)))
        :rator (node-direct-application-rator node)
        :rands constant-propagated-rands)))
    ((node-seq-p node)
     (make-node-seq
      :type (node-type node)
      :nodes (mapcar (lambda (node)
                       (propagate-constants-with-bindings env node constant-bindings))
                     (node-seq-nodes node))))
    ((node-lisp-p node)
     ;; FIXME: This looks too convoluted.
     (let ((new-lisp-vars nil))
       (make-node-let
        :type (node-type node)
        :bindings (loop :for (lisp-var . coalton-var) :in (node-lisp-vars node)
                        :for constant-value := (constant-var-value coalton-var constant-bindings)
                        :if constant-value
                          :collect (let ((new-coalton-var (gentemp (symbol-name coalton-var))))
                                     (push (cons lisp-var new-coalton-var) new-lisp-vars)
                                     (cons new-coalton-var constant-value))
                        :else
                          :do (push (cons lisp-var coalton-var) new-lisp-vars))
        :subexpr (make-node-lisp
                  :type (node-type node)
                  :vars new-lisp-vars
                  :form (node-lisp-form node)))))
    (t
     ;; TODO: Propagate constants in other nodes
     (error "Don't know how to propagate constants in ~S" node))))

(defun propagate-constants (node env)
  (declare (type node node))
  ;; FIXME: Can we use TRAVERSE?
  ;; The trouble is: TRAVERSE expects a symbol-list, while we want a binding list
  (propagate-constants-with-bindings env node ()))
