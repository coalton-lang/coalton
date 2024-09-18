(defpackage #:coalton-impl/analysis/underapplied-values
  (:use
   #:cl)
  (:local-nicknames
   (#:source #:coalton-impl/source)
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
                    :do (source:warn "Value may be underapplied"
                                     (source:note elem
                                                  "discard explicitly with (let _ = ...) to ignore this warning")))
            node))))
