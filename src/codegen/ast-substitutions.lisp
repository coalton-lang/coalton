(defpackage #:coalton-impl/codegen/ast-substitutions
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/traverse
   #:action
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

(defun apply-ast-substitution (subs node)
  "Substitute variables in the tree of `node` with other nodes specified
in `subs`. Throw an error if a variable to be substituted is bound in
a subtree of `node`."
  (declare (type ast-substitution-list subs)
           (type node node)
           (values node &optional))
  (traverse
   node
   (list
    (action (:after node-variable node)
      (alexandria:when-let
          ((res (find (node-variable-value node) subs :key #'ast-substitution-from)))
        (ast-substitution-to res)))
    (action (:after node-lisp node)
      (multiple-value-bind (let-bindings lisp-var-bindings)
          (loop :for (lisp-var . coalton-var) :in (node-lisp-vars node)
                :for new-var := (gensym (symbol-name coalton-var))
                :for res := (find coalton-var subs :key #'ast-substitution-from)
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
    (action (:before node-direct-application node)
      (when (find (node-direct-application-rator node) subs :key #'ast-substitution-from)
        (util:coalton-bug
         "Failure to apply ast substitution on variable ~A to node-direct-application"
         (node-direct-application-rator node))))
    (action (:before node-let node)
      (loop :for (name . _) :in (node-let-bindings node)
            :do (when (find name subs :key #'ast-substitution-from)
                  (util:coalton-bug
                   "Failure to apply ast substitution on variable ~A to node-let"
                   name)))))))
