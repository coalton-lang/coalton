(defpackage #:coalton-impl/codegen/ast-substitutions
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/traverse
   #:action
   #:*traverse*
   #:traverse)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:util #:coalton-impl/util))
  (:export
   #:ast-substitution                   ; STRUCT
   #:make-ast-substitution              ; CONSTRUCTOR
   #:ast-substitution-from              ; ACCESSOR
   #:ast-substitution-to                ; ACCESSOR
   #:ast-substitution-list              ; TYPE
   #:apply-ast-substitution             ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/ast-substitutions)

(defstruct ast-substitution
  (from (util:required 'from) :type parser:identifier :read-only t)
  (to   (util:required 'to)   :type node              :read-only t))

(defun ast-substitution-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ast-substitution-p x)))

(deftype ast-substitution-list ()
  '(satisfies ast-substitution-list-p))

(defun apply-ast-substitution (subs node &optional (rename-bound-variables nil))
  "Substitute variables in the tree of `node` with other nodes specified
in `subs`. Throw an error if a variable to be substituted is bound in
a subtree of `node`. Also rename all bound variables if `rename-bound-variables`
is true."
  (declare (type ast-substitution-list subs)
           (type node node)
           (type boolean rename-bound-variables)
           (values node &optional))

  ;; In the case that `rename-bound-variables` is `t`, we keep track
  ;; of bound variable renaming in `new-subs`.  Otherwise, `new-subs`
  ;; will remain `nil` throughout the traversal.
  (traverse
   node
   (list
    (action (:after node-variable node new-subs)
      (alexandria:when-let
          ((res (or (find (node-variable-value node) subs :key #'ast-substitution-from)
                    (find (node-variable-value node) new-subs :key #'ast-substitution-from))))
        (ast-substitution-to res)))
    (action (:after node-lisp node new-subs)
      (multiple-value-bind (let-bindings lisp-var-bindings)
          (loop :for (lisp-var . coalton-var) :in (node-lisp-vars node)
                :for new-var := (gensym (symbol-name coalton-var))
                :for res := (or (find coalton-var subs :key #'ast-substitution-from)
                                (find coalton-var new-subs :key #'ast-substitution-from))
                :if (and res (node-variable-p (ast-substitution-to res)))
                  :collect (cons lisp-var (node-variable-value (ast-substitution-to res)))
                    :into lisp-var-bindings
                :else :if res
                        :collect (cons lisp-var new-var) :into lisp-var-bindings
                        :and :collect (cons new-var (ast-substitution-to res))
                               :into let-bindings
                :else
                  :collect (cons lisp-var coalton-var) :into lisp-var-bindings
                :finally (return (values let-bindings lisp-var-bindings)))
        (let ((new-lisp-node (make-node-lisp
                              :type (node-type node)
                              :vars lisp-var-bindings
                              :form (node-lisp-form node))))
          (if (endp let-bindings)
              new-lisp-node
              (make-node-let
               :type (node-type node)
               :bindings let-bindings
               :subexpr new-lisp-node)))))
    (action (:before node-direct-application node new-subs)
      (when (find (node-direct-application-rator node) subs :key #'ast-substitution-from)
        (util:coalton-bug
         "Failure to apply ast substitution on variable ~A to node-direct-application"
         (node-direct-application-rator node)))
      (alexandria:when-let
          ((res (find (node-direct-application-rator node) new-subs :key #'ast-substitution-from)))
        (make-node-direct-application
         :type (node-type node)
         :rator-type (node-direct-application-rator-type node)
         :rator (node-variable-value (ast-substitution-to res))
         :rands (node-direct-application-rands node))))
    (action (:traverse node-let node new-subs)
      (loop :for (name . expr) :in (node-let-bindings node)
            :do (when (find name subs :key #'ast-substitution-from)
                  (util:coalton-bug
                   "Failure to apply ast substitution on variable ~A to node-let"
                   name))
            :do (when rename-bound-variables
                  (push (make-ast-substitution
                         :from name
                         :to (make-node-variable
                              :type (node-type expr)
                              :value (gensym (symbol-name name))))
                        new-subs)))
      (make-node-let
       :type (node-type node)
       :bindings
       (loop :for (name . node) :in (node-let-bindings node)
             :collect
             (cons (if rename-bound-variables
                       (node-variable-value
                        (ast-substitution-to
                         (find name new-subs :key #'ast-substitution-from)))
                       name)
                   (funcall *traverse* node new-subs)))
       :subexpr (funcall *traverse* (node-let-subexpr node) new-subs))))
   nil))
