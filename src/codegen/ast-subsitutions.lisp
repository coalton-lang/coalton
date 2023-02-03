(defpackage #:coalton-impl/codegen/ast-substitutions
  (:use
   #:cl
   #:coalton-impl/util
   #:coalton-impl/codegen/ast)
  (:export
   #:ast-substitution                   ; STRUCT
   #:make-ast-substitution              ; CONSTRUCTOR
   #:ast-substitution-from              ; ACCESSOR
   #:ast-substitution-to                ; ACCESSOR
   #:ast-substitution-list              ; TYPE
   ))

(in-package #:coalton-impl/codegen/ast-substitutions)

(defstruct ast-substitution
  (from (required 'from) :type symbol :read-only t)
  (to   (required 'to)   :type node   :read-only t))

(defun ast-substitution-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ast-substitution-p x)))

(deftype ast-substitution-list ()
  '(satisfies ast-substitution-list-p))

(defgeneric apply-ast-substitution (subs node)
  (:method (subs (list list))
    (loop :for node :in list
          :collect (apply-ast-substitution subs node)))

  (:method (subs (node node-literal))
    (declare (type ast-substitution-list subs)
             (ignore subs)
             (values node))
    node)

  (:method (subs (node node-lisp))
    (declare (type ast-substitution-list subs)
             (ignore subs)
             (values node))
    node)

  (:method (subs (node node-variable))
    (declare (type ast-substitution-list subs)
             (values node))
    (let ((res (find (node-variable-value node) subs :key #'ast-substitution-from)))
      (if res
          (ast-substitution-to res)
          node)))

  (:method (subs (node node-application))
    (declare (type ast-substitution-list subs)
             (values node))
    (make-node-application
     :type (node-type node)
     :rator (apply-ast-substitution subs (node-application-rator node))
     :rands (apply-ast-substitution subs (node-application-rands node))))

  (:method (subs (node node-direct-application))
    (declare (type ast-substitution-list subs)
             (values node))

    (when (find (node-direct-application-rator node) subs :key #'ast-substitution-from)
      (coalton-bug
       "Failure to apply ast substitution on variable ~A to node-direct-application"
       (node-direct-application-rator node)))

    (make-node-direct-application
     :type (node-type node)
     :rator-type (node-direct-application-rator-type node)
     :rator (node-direct-application-rator node)
     :rands (apply-ast-substitution subs (node-direct-application-rands node))))

  (:method (subs (node node-abstraction))
    (declare (type ast-substitution-list subs)
             (values node))

    (make-node-abstraction
     :type (node-type node)
     :vars (node-abstraction-vars node)
     :subexpr (apply-ast-substitution subs (node-abstraction-subexpr node))))

  (:method (subs (node node-let))
    (declare (type ast-substitution-list subs)
             (values node))

    (make-node-let
     :type (node-type node)
     :bindings (loop :for (name . node) :in (node-let-bindings node)
                     :do (when (find name subs :key #'ast-substitution-from)
                           (coalton-bug
                            "Failure to apply ast substitution on variable ~A to node-let"
                            name))
                     :collect (cons name (apply-ast-substitution subs node)))
     :subexpr (apply-ast-substitution subs (node-let-subexpr node))))

  (:method (subs (node match-branch))
    (declare (type ast-substitution-list subs)
             (values match-branch))
    (make-match-branch
     :pattern (match-branch-pattern node)
     :body (apply-ast-substitution subs (match-branch-body node))))

  (:method (subs (node node-match))
    (declare (type ast-substitution-list subs)
             (values node))
    (make-node-match
     :type (node-type node)
     :expr (apply-ast-substitution subs (node-match-expr node))
     :branches (apply-ast-substitution subs (node-match-branches node))))

  (:method (subs (node node-seq))
    (declare (type ast-substitution-list subs)
             (values node))
    (make-node-seq
     :type (node-type node)
     :nodes (apply-ast-substitution subs (node-seq-nodes node))))

  (:method (subs (node node-return))
    (declare (type ast-substitution-list subs)
             (values node))
    (make-node-return
     :type (node-type node)
     :expr (apply-ast-substitution subs (node-return-expr node))))

  (:method (subs (node node-field))
    (declare (type ast-substitution-list subs)
             (values node))
    (make-node-field
     :type (node-type node)
     :name (node-field-name node)
     :dict (apply-ast-substitution subs (node-field-dict node))))

  (:method (subs (node node-dynamic-extent))
    (declare (type ast-substitution-list subs)
             (values node))
    (make-node-dynamic-extent
     :type (node-type node)
     :name (node-dynamic-extent-name node)
     :node (apply-ast-substitution subs (node-dynamic-extent-node node))
     :body (apply-ast-substitution subs (node-dynamic-extent-body node))))

  (:method (subs (node node-bind))
    (declare (type ast-substitution-list subs)
             (values node))
    (make-node-bind
     :type (node-type node)
     :name (node-bind-name node)
     :expr (apply-ast-substitution subs (node-bind-expr node))
     :body (apply-ast-substitution subs (node-bind-body node)))))
