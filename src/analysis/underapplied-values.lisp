(defpackage #:coalton-impl/analysis/underapplied-values
  (:use
   #:cl
   #:coalton-impl/source)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:find-underapplied-values))

(in-package #:coalton-impl/analysis/underapplied-values)

(defun find-underapplied-values (binding)
  (tc:traverse
   (tc:binding-value binding)
   (tc:make-traverse-block
    :body (lambda (node)
            (declare (type tc:node-body node))

            (loop :for elem :in (tc:node-body-nodes node)
                  :when (and (typep elem 'tc:node)
                             (tc:function-type-p (tc:qualified-ty-type (tc:node-type elem))))
                    :do (source-warning "Value may be underapplied"
                                        (make-note elem
                                                   "discard explicitly with (let _ = ...) to ignore this warning")))
            node))))
