(in-package #:coalton-impl/ast)

(defun expr-variables (value)
  "Compute a list of variables referenced in the VALUE expression.

NOTE: Just because a variable shows up in the list does *NOT* mean the
variable is free in the expression."
  (declare (type node value)
           (values list))
  (let ((fv nil))
    (labels ((analyze (expr)
               (etypecase expr
                 (node-literal
                  nil)

                 (node-variable
                  (let ((name (node-variable-name expr)))
                    (push name fv)))

                 (node-abstraction
                   (analyze (node-abstraction-subexpr expr)))

                 (node-let
                  (let* ((bindings (node-let-bindings expr))
                         (subexpr (node-let-subexpr expr))
                         (vals (mapcar #'cdr bindings)))
                    (mapc #'analyze (cons subexpr vals))))

                 (node-lisp
                  nil)

                 (node-application
                  (let ((rator (node-application-rator expr))
                        (rands (node-application-rands expr)))
                    (mapc #'analyze (cons rator rands))))

                 (node-match
                  (let ((value (node-match-expr expr))
                        (branches (node-match-branches expr)))
                    (analyze value)
                    (dolist (branch branches)
                      (analyze (match-branch-subexpr branch)))))

                 (node-seq
                  (dolist (subnode (node-seq-subnodes expr))
                    (analyze subnode)))

                 (node-the
                  (analyze (node-the-subnode expr))))))

      (analyze value)
      fv)))

(defun bindings-to-dag (bindings)
  (let ((vars (mapcar #'car bindings)))
    (loop :for (var . val) :in bindings
          :collect (cons var (copy-list (intersection vars (expr-variables val)))))))
