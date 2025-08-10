(defpackage #:coalton-impl/codegen/transformations
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast
   #:coalton-impl/codegen/traverse)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:rename-type-variables
   #:node-variables
   #:node-free-p
   #:localize-returns))

(in-package #:coalton-impl/codegen/transformations)

(defmethod tc:apply-substitution (subs (node node))
  "Substitute type variables in the tree of `node` with other types
specified in `subs`."
  (declare (type tc:substitution-list subs)
           (values node &optional))
  (traverse
   node
   (list
    (action (:after node node)
      (copy-node node
                 (tc:apply-substitution subs (node-type node))))
    (action (:after node-direct-application node)
      (make-node-direct-application
       :type (node-type node)
       :properties (node-properties node)
       :rator-type (tc:apply-substitution subs (node-direct-application-rator-type node))
       :rator (node-direct-application-rator node)
       :rands (node-direct-application-rands node)))
    (action (:after node-match node)
      (make-node-match
       :type (node-type node)
       :expr (node-match-expr node)
       :branches (mapcar
                  (lambda (branch)
                    (make-match-branch
                     :pattern (tc:apply-substitution subs (match-branch-pattern branch))
                     :body (match-branch-body branch)))
                  (node-match-branches node))))
    (action (:after node-while-let node)
      (make-node-while-let
       :type (node-type node)
       :label (node-while-let-label node)
       :pattern (tc:apply-substitution subs (node-while-let-pattern node))
       :expr (node-while-let-expr node)
       :body (node-while-let-body node))))))

(defmethod tc:type-variables ((node node))
  "Collect all type variables from nodes and patterns in the tree of `node`."
  (declare (values tc:tyvar-list &optional))
  (let ((tyvars nil))
    (traverse
     node
     (list
      (action (:after node node)
        (alexandria:unionf tyvars
                           (tc:type-variables (node-type node)))
        (values))
      (action (:after node-direct-application node)
        (alexandria:unionf tyvars
                           (tc:type-variables (node-direct-application-rator-type node)))
        (values))
      (action (:after node-match node)
        (dolist (branch (node-match-branches node))
          (alexandria:unionf tyvars
                             (tc:type-variables (match-branch-pattern branch))))
        (values))
      (action (:after node-while-let node)
        (alexandria:unionf tyvars
                           (tc:type-variables (node-while-let-pattern node)))
        (values))))
    tyvars))

(defun rename-type-variables (node &optional (renamer #'tc:fresh-type-renamer))
  "Rename the type variables of `node` and its subnodes according to the
function `renamer`, returning the new node tree."
  (declare (type node     node)
           (type function renamer)
           (values node &optional))
  (alexandria:if-let ((old-type-variables (tc:type-variables node)))
    (tc:apply-substitution
     (mapcar (lambda (tyvar)
               (tc:make-substitution
                :from tyvar
                :to (funcall renamer tyvar)))
             old-type-variables)
     node)
    node))

(defun node-variables (node &key (variable-namespace-only nil))
  "Return a deduplicated list of identifiers representing variables in
both CL namespaces appearing in `node`"
  (declare (type node    node)
           (type boolean variable-namespace-only)
           (values parser:identifier-list &optional))
  (let ((node-vars nil))
    (traverse
     node
     (list
      (action (:after node-variable node)
        (setf node-vars
              (adjoin (node-variable-value node) node-vars))
        (values))
      (action (:after node-direct-application node)
        (unless variable-namespace-only
          (setf node-vars
                (adjoin (node-direct-application-rator node) node-vars)))
        (values))
      (action (:after node-lisp node)
        (alexandria:unionf node-vars
                           (mapcar #'cdr (node-lisp-vars node)))
        (values))))
    node-vars))

(defun node-free-p (node bound-variables)
  "Return true if every variable in `node` is free with respect to
`bound-variables`"
  (declare (type node                   node)
           (type parser:identifier-list bound-variables)
           (values boolean &optional))
  (null (intersection (node-variables node) bound-variables)))
