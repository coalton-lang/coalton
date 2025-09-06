(defpackage #:coalton-impl/codegen/canonicalizer
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/traverse
   #:action
   #:traverse)
  (:export
   #:canonicalize))

(in-package #:coalton-impl/codegen/canonicalizer)

(defun canonicalize (node)
  "Canonicalize the applications of NODE. \"Canonicalize\" means to
supply as many arguments as possible to nested, partially applied
functions. For example, the canonicalization of

    ((FOO BAR) BAZ)

would be:

    (FOO BAR BAZ)"
  (declare (type node node)
           (values node &optional))
  (labels ((rewrite-application (node)
             (let ((rator (node-application-rator node))
                   (rands (node-application-rands node)))
               (when (node-application-p rator)
                 (make-node-application
                  :type (node-type node)
                  :properties (node-properties node)
                  :rator (node-application-rator rator)
                  :rands (append
                          (node-application-rands rator)
                          rands))))))
    (traverse
     node
     (list
      (action (:after node-application) #'rewrite-application)))))
