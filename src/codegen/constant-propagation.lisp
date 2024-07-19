(defpackage #:coalton-impl/codegen/constant-propagation
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:propagate-constants))

(in-package #:coalton-impl/codegen/constant-propagation)

(defun constant-var-value (var constant-bindings)
  (cdr (assoc var constant-bindings)))

(defun constant-node-p (env node constant-bindings)
  (declare (type node node))
  ;; FIXME: We need more nodes classified as constants
  (cond ((node-literal-p node)
         t)
        ((and (node-variable-p node)
              (tc:lookup-instance-by-codegen-sym env (node-variable-value node) :no-error t))
         t)
        ((node-variable-p node)
         (constant-var-value (node-variable-value node) constant-bindings))
        (t
         nil)))

(defun constant-node-value (env node constant-bindings)
  (declare (type node node))
  ;; FIXME: We need more nodes classified as constants
  (cond ((node-literal-p node)
         node)
        ((and (node-variable-p node)
              (tc:lookup-instance-by-codegen-sym env (node-variable-value node) :no-error t))
         node)
        ((node-variable-p node)
         (constant-var-value (node-variable-value node) constant-bindings))
        (t
         nil)))

;; FIXME: Can or Should we use an existing structure to handle bindings
;; FIXME: Can we use TRAVERSE?
;; The trouble is: TRAVERSE expects a symbol-list, while we want a binding list
(defgeneric propagate-constants (node env &optional constant-bindings))

(defmethod propagate-constants (node env &optional constant-bindings)
  (cerror "Don't know how to propagate constants in ~S" node)
  node)

(defmethod propagate-constants ((node node-literal) env &optional constant-bindings)
  (declare (ignore env constant-bindings))
  node)

(defmethod propagate-constants ((node node-variable) env &optional constant-bindings)
  (or (constant-node-value env node constant-bindings)
      node))

(defmethod propagate-constants ((node node-let) env &optional constant-bindings)
  (let ((node-bindings (node-let-bindings node))
        (new-constant-bindings nil)
        (nonconstant-bindings nil))
    (loop :for (var . value) :in node-bindings
          :for propagated-value-node := (propagate-constants value env constant-bindings)
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
           :subexpr (propagate-constants
                     (node-let-subexpr node)
                     env
                     constant-bindings))
          (propagate-constants
           (node-let-subexpr node)
           env
           constant-bindings)))))

(defmethod propagate-constants ((node node-abstraction) env &optional constant-bindings)
  (make-node-abstraction
   ;; FIXME: A more specific type?
   :type (node-type node)
   :vars (node-abstraction-vars node)
   :subexpr (propagate-constants
             (node-abstraction-subexpr node)
             env
             constant-bindings)))

(defmethod propagate-constants ((node node-application) env &optional constant-bindings)
  (let ((constant-propagated-rands
          (mapcar (lambda (rand)
                    (propagate-constants
                     rand
                     env
                     constant-bindings))
                  (node-application-rands node))))
    (make-node-application
     ;; FIXME: A more specific type
     :type (node-type node)
     :rator (node-application-rator node)
     :rands constant-propagated-rands)))

(defmethod propagate-constants ((node node-direct-application) env &optional constant-bindings)
  (let ((constant-propagated-rands
          (mapcar (lambda (rand)
                    (propagate-constants
                     rand
                     env
                     constant-bindings))
                  (node-direct-application-rands node))))
    (make-node-direct-application
     :type (node-type node)
     :rator-type (tc:make-function-type*
                  (mapcar #'node-type constant-propagated-rands)
                  (tc:function-return-type (node-type node)))
     :rator (node-direct-application-rator node)
     :rands constant-propagated-rands)))

(defmethod propagate-constants ((node node-seq) env &optional constant-bindings)
  (make-node-seq
   :type (node-type node)
   :nodes (mapcar (lambda (node)
                    (propagate-constants node env constant-bindings))
                  (node-seq-nodes node))))

(defmethod propagate-constants ((node node-lisp) env &optional constant-bindings)
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
