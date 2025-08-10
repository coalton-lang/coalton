(defpackage #:coalton-impl/codegen/constant-propagation
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker))
  (:import-from
   #:coalton-impl/codegen/traverse
   #:action
   #:*traverse*
   #:traverse)
  (:import-from
   #:coalton-impl/codegen/monomorphize
   #:dictionary-node-p)
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
           (error "Expected VAR ~S to be a constant but it is not" var)))))

(defun constant-node-p (env node constant-bindings)
  "If NODE is a constant, returns the constant value corresponding to it.
If not, returns NIL"
  (declare (type tc:environment env)
           (type node node)
           (type list constant-bindings))
  (cond ((node-literal-p node)
         (if (or (numberp (node-literal-value node)) (characterp (node-literal-value node)))
             node
             nil))
        ((node-variable-p node)
         (cond ((and (eq 'coalton:Boolean (node-type node))
                     (member (node-variable-value node) '(coalton:True coalton:False)))
                node)
               ((tc:lookup-instance-by-codegen-sym env (node-variable-value node) :no-error t)
                node)
               (t
                (constant-var-value (node-variable-value node) constant-bindings :no-error t))))
        ((node-lisp-p node)
         (if (cl:constantp (node-lisp-form node))
             node
             nil))
        ;; special support for dictionaries
        ((dictionary-node-p node env)
         node)
        (t
         nil)))

(defun limit-name (x)
  (if (< (length x) 16)
      x
      (subseq x 0 16)))

(defun propagate-constants (node env)
  (declare (optimize debug))
  (labels ((propagate-constants-node-variable (node constant-bindings)
             (declare (type node-variable node))
             (let ((x (constant-node-p env node constant-bindings)))
               (if (not (null x))
                   x
                   node)))

           (propagate-constants-node-let (node constant-bindings)
             (declare (type node-let node))
             (let ((node-bindings (node-let-bindings node))
                   (new-constant-bindings nil)
                   (nonconstant-bindings nil))
               (loop :for (var . value) :in node-bindings
                     :for propagated-value-node := (funcall *traverse* value constant-bindings)
                     :do (cond
                           ((constant-node-p env propagated-value-node constant-bindings)
                            (push (cons var propagated-value-node)
                                  new-constant-bindings))
                           (t
                            (push (cons var propagated-value-node)
                                  nonconstant-bindings))))

               ;; Propagate new constant bindings into the values of
               ;; the remaining nonconstant bindings.
               ;;
               ;; Warning : This loop attempts to compute a fixed
               ;; point among the let bindings with respect to
               ;; constant propagation. The loop will still be correct
               ;; if `continue` is initialized to `t`. However, when
               ;; `new-constant-bindings` is empty, the loop has no
               ;; effect on the result, but creates an exponential
               ;; compile-time cost in some cases (such as a
               ;; `make-list` with many elements).
               (loop :with continue := (not (endp new-constant-bindings))
                     :while continue
                     :do (setf continue nil)
                         (loop :for binding :in nonconstant-bindings
                               :for (var . value) := binding
                               :for new-value := (funcall *traverse* value new-constant-bindings)
                               :when (constant-node-p env new-value (append new-constant-bindings constant-bindings))
                                 :do (push (cons var new-value) new-constant-bindings)
                                     (setf nonconstant-bindings (delete var nonconstant-bindings :test #'eq :key #'car))
                                     (setf continue t)))

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
                              :collect (let ((new-coalton-var (gentemp (limit-name (symbol-name coalton-var)))))
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
              :properties (node-direct-application-properties node)
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
