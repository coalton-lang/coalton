(defpackage #:coalton-impl/codegen/ast-substitutions
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/transformations
   #:traverse)
  (:local-nicknames
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
  (from (util:required 'from) :type symbol :read-only t)
  (to   (util:required 'to)   :type node   :read-only t))

(defun ast-substitution-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ast-substitution-p x)))

(deftype ast-substitution-list ()
  '(satisfies ast-substitution-list-p))

(defun apply-ast-substitution (subs node)
  (declare (type ast-substitution-list subs)
           (type node node)
           (values node &optional))
  (traverse
   node
   (list
    (cons :variable
          (lambda (node)
            (declare (type node-variable node)
                     (values node &optional))
            (alexandria:if-let
                ((res (find (node-variable-value node) subs :key #'ast-substitution-from)))
              (ast-substitution-to res)
              node)))
    (cons :lisp
          (lambda (node)
            (declare (type node-lisp node)
                     (values (or node-lisp node-let) &optional))
            (multiple-value-bind (let-bindings lisp-var-bindings)
                (loop :for (lisp-var . coalton-var) :in (node-lisp-vars node)
                      :for new-var := (gentemp (symbol-name coalton-var))
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
                (if (null let-bindings)
                    new-lisp-node
                    (make-node-let
                     :type (node-type node)
                     :bindings let-bindings
                     :subexpr new-lisp-node))))))
    (cons :before-direct-application
          (lambda (node)
            (declare (type node-direct-application node)
                     (values))
            (when (find (node-direct-application-rator node) subs :key #'ast-substitution-from)
              (util:coalton-bug
               "Failure to apply ast substitution on variable ~A to node-direct-application"
               (node-direct-application-rator node)))))
    (cons :before-let
          (lambda (node)
            (declare (type node-let node)
                     (values))
            (loop :for (name . _) :in (node-let-bindings node)
                  :do (when (find name subs :key #'ast-substitution-from)
                        (util:coalton-bug
                         "Failure to apply ast substitution on variable ~A to node-let"
                         name))))))))
