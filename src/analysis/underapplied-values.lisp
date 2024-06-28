(defpackage #:coalton-impl/analysis/underapplied-values
  (:use
   #:cl)
  (:local-nicknames
   (#:se #:source-error)
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:find-underapplied-values           ; FUNCTION
   ))

(in-package #:coalton-impl/analysis/underapplied-values)

(define-condition underapplied-value-warning (se:source-base-warning)
  ())

(defun find-underapplied-values (binding file)
  (tc:traverse
   (tc:binding-value binding)
   (tc:make-traverse-block
    :body (lambda (node)
            (declare (type tc:node-body node))

            (loop :for elem :in (tc:node-body-nodes node)
                  :when (and (typep elem 'tc:node)
                             (tc:function-type-p (tc:qualified-ty-type (tc:node-type elem))))
                    :do (warn 'underapplied-value-warning
                              :err (se:source-error
                                    :type :warn
                                    :file file
                                    :span (tc:node-source elem)
                                    :message "Value may be underapplied"
                                    :primary-note "discard explicitly with (let _ = ...) to ignore this warning")))

            node))))
