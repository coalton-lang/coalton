(defpackage #:coalton-impl/analysis/underapplied-values
  (:use
   #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:find-underapplied-values           ; FUNCTION
   ))

(in-package #:coalton-impl/analysis/underapplied-values)

(define-condition underapplied-value-warning (error:coalton-base-warning)
  ())

(defun find-underapplied-values (binding)
  (tc:traverse
   (tc:binding-value binding)
   (tc:make-traverse-block
    :body (lambda (node)
            (declare (type tc:node-body node))

            (loop :for elem :in (tc:node-body-nodes node)
                  :when (and (typep elem 'tc:node)
                             (tc:function-type-p (tc:qualified-ty-type (tc:node-type elem))))
                    :do (warn 'underapplied-value-warning
                              :location (tc:node-source elem)
                              :message "Value may be underapplied"
                              :primary-note "discard explicitly with (let _ = ...) to ignore this warning"))

            node))))
