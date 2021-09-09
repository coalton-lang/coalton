(in-package #:coalton-impl/ast)

(defun free-variables (value)
  "Compute a list of free variables in the VALUE expression.

NOTE: Just because a variable shows up in the list does *NOT* mean all occurrences of that variable are free in the expression."
  (declare (type node value)
           (values list))
  (let ((fv nil))
    (labels ((analyze (expr bv)
               (etypecase expr
                 (node-literal
                  nil)

                 (node-variable
                  (let ((name (node-variable-name expr)))
                    (unless (member name bv)
                      (push name fv))))

                 (node-abstraction
                  (let ((vars (node-abstraction-vars expr)))
                    (analyze (node-abstraction-subexpr expr) (union bv vars))))

                 (node-let
                  (let* ((bindings (node-let-bindings expr))
                         (subexpr (node-let-subexpr expr))
                         (vars (mapcar #'car bindings))
                         (vals (mapcar #'cdr bindings))
                         (bv* (union bv vars)))
                    (mapc (lambda (expr) (analyze expr bv*)) (cons subexpr vals))))

                 (node-lisp
                  nil)

                 (node-application
                  (let ((rator (node-application-rator expr))
                        (rands (node-application-rands expr)))
                    (mapc (lambda (expr) (analyze expr bv)) (cons rator rands))))

                 (node-match
                  (let ((value (node-match-expr expr))
                        (branches (node-match-branches expr)))
                    (analyze value bv)
                    (dolist (branch branches)
                      (analyze (match-branch-subexpr branch)
                               (union bv (pattern-variables
                                          (match-branch-pattern branch)))))))
                 (node-seq
                  (dolist (subnode (node-seq-subnodes expr))
                    (analyze subnode bv))))))
      (analyze value nil)
      fv)))

(defun bindings-to-dag (bindings)
  (let ((vars (mapcar #'car bindings)))
    (loop :for (var . val) :in bindings
          :collect (cons var (copy-list (intersection vars (free-variables val)))))))
