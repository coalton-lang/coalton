(defpackage #:coalton-impl/codegen/intrinsic-applications
  (:use #:cl)
  (:local-nicknames
   (#:ast #:coalton-impl/codegen/ast)
   (#:traverse #:coalton-impl/codegen/traverse))
  (:export
   #:transform-intrinsic-application
   #:transform-intrinsic-applications))
(in-package #:coalton-impl/codegen/intrinsic-applications)

(defgeneric transform-intrinsic-application (rator-name node)
  (:documentation "Methods for implementing intrinsic-applications to transform
codegen AST during optimization.

EQL-specialize on symbol `rator-name'.
`node' will be of type `ast:node-application' or `ast:node-direct-application'.

Returns a new `ast:node' when a transformation is applicable, otherwise `nil'."))

(defmethod transform-intrinsic-application ((rator-name t) node) nil)

(defmethod transform-intrinsic-application ((rator-name (eql 'coalton:inline)) node)
  (let ((child (first (ast:node-application-rands node))))
    (typecase child
      (ast:node-application
       (ast:make-node-application
        :type (ast:node-type child)
        :properties '(:inline t)
        :rator (ast:node-application-rator child)
        :rands (ast:node-application-rands child)))
      (ast:node-direct-application
       (ast:make-node-direct-application 
        :type (ast:node-type child)
        :properties '(:inline t)
        :rator-type (ast:node-direct-application-rator-type child)
        :rator (ast:node-direct-application-rator child)
        :rands (ast:node-direct-application-rands child)))
      (t
       child))))

(defmethod transform-intrinsic-application ((rator-name (eql 'coalton:noinline)) node)
  (let ((child (first (ast:node-application-rands node))))
    (typecase child
      (ast:node-application
       (ast:make-node-application
        :type (ast:node-type child)
        :properties '(:noinline t)
        :rator (ast:node-application-rator child)
        :rands (ast:node-application-rands child)))
      (ast:node-direct-application
       (ast:make-node-direct-application 
        :type (ast:node-type child)
        :properties '(:noinline t)
        :rator-type (ast:node-direct-application-rator-type child)
        :rator (ast:node-direct-application-rator child)
        :rands (ast:node-direct-application-rands child)))
      (t
       child))))

(defun transform-intrinsic-applications (node)
  "Traverse node, transforming all intrinsic-applications for optimization."
  (declare (type ast:node node)
           (values ast:node &optional))

  (traverse:traverse
   node
   (list
    (traverse:action (:traverse ast:node-direct-application node)
      (alexandria:if-let
          ((transformed (transform-intrinsic-application
                         (ast:node-rator-name node)
                         node)))
        (funcall traverse:*traverse* transformed)
        node))
    (traverse:action (:traverse ast:node-application node)
      (alexandria:if-let
          ((transformed (transform-intrinsic-application
                         (ast:node-rator-name node)
                         node)))
        (funcall traverse:*traverse* transformed)
        node)))))
