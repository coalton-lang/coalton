(defpackage #:coalton-impl/analysis/discarded-results
  (:use
   #:cl)
  (:local-nicknames
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:find-discarded-results))

(in-package #:coalton-impl/analysis/discarded-results)

(defun result-type-p (type)
  "Is TYPE an application of the standard library's `Result` type?"
  (let ((result (and (find-package "COALTON/CLASSES")
                     (find-symbol "RESULT" "COALTON/CLASSES"))))
    (and result
         (let ((head (first (tc:flatten-type type))))
           (and (tc:tycon-p head)
                (eq (tc:tycon-name head) result))))))

(defun warn-if-discarded-result (node)
  "Warn if NODE, whose value is discarded, produces a `Result`."
  (when (and (typep node 'tc:node)
             (result-type-p (tc:qualified-ty-type (tc:node-type node))))
    (source:warn "Discarded Result"
                 (source:note node
                              "the error in this Result is ignored; discard it explicitly with (let _ = ...) to ignore this warning"))))

(defun find-discarded-results (binding)
  "Warn about `Result` values that are discarded in the body of BINDING,
which silently ignores the errors they may contain."
  (tc:traverse
   (tc:binding-value binding)
   (tc:make-traverse-block
    :body (lambda (node)
            (declare (type tc:node-body node))
            (mapc #'warn-if-discarded-result (tc:node-body-nodes node))
            node)
    ;; WHEN and UNLESS also discard the value of their last body form.
    :when (lambda (node)
            (warn-if-discarded-result (tc:node-body-last-node (tc:node-when-body node)))
            node)
    :unless (lambda (node)
              (warn-if-discarded-result (tc:node-body-last-node (tc:node-unless-body node)))
              node))))
