(defpackage #:coalton-impl/codegen/faux-applications
  (:use #:cl)
  (:local-nicknames
   (#:ast #:coalton-impl/codegen/ast)
   (#:traverse #:coalton-impl/codegen/traverse))
  (:export
   #:transform-faux-application
   #:transform-faux-applications))
(in-package #:coalton-impl/codegen/faux-applications)

(defmethod transform-faux-application ((rator t) node) nil)

(defmethod transform-faux-application ((rator (eql 'coalton:inline)) node)
  (let ((child (first (ast:node-application-rands node))))
    (etypecase child
      (ast:node-application
       (ast:make-node-application
        :type (ast:node-type child)
        :inlinep t
        :noinlinep nil
        :rator (ast:node-application-rator child)
        :rands (ast:node-application-rands child)))
      (ast:node-direct-application
       (ast:make-node-direct-application 
        :type (ast:node-type child)
        :inlinep t
        :noinlinep nil
        :rator-type (ast:node-direct-application-rator-type child)
        :rator (ast:node-direct-application-rator child)
        :rands (ast:node-direct-application-rands child)))
      (t
       child))))

(defmethod transform-faux-application ((rator (eql 'coalton:noinline)) node)
  (let ((child (first (ast:node-application-rands node))))
    (etypecase child
      (ast:node-application
       (ast:make-node-application
        :type (ast:node-type child)
        :inlinep nil
        :noinlinep t
        :rator (ast:node-application-rator child)
        :rands (ast:node-application-rands child)))
      (ast:node-direct-application
       (ast:make-node-direct-application 
        :type (ast:node-type child)
        :inlinep nil
        :noinlinep t
        :rator-type (ast:node-direct-application-rator-type child)
        :rator (ast:node-direct-application-rator child)
        :rands (ast:node-direct-application-rands child)))
      (t
       child))))

(defun transform-faux-applications (node)
  (traverse:traverse
   node
   (list
    (traverse:action (:traverse ast:node-application node)
      (alexandria:if-let
          ((transformed (transform-faux-application
                         (ast:node-rator-name node)
                         node)))
        (funcall traverse:*traverse* transformed)
        node)))))
