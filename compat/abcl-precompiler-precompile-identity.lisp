;;; abcl (1.9.2) needs this patch of precompile-identity.
;;;
;;; Reported in
;;; https://github.com/slburson/fset/issues/105#issuecomment-3970021869
;;;
;;; The patch is already in abcl's repo, hopefully it'll make it in
;;; the 1.9.2+ release.

(cl:in-package :precompiler)

(declaim (ftype (function (t t) t) precompile-function-call))
(defun precompile-function-call (form)
  (let ((op (car form)))
    (when (and (consp op) (eq (%car op) 'LAMBDA))
      (return-from precompile-function-call
        (or (precompile1 (precompile-function-position-lambda op (cdr form)))
            (cons (precompile-lambda op)
                  (mapcar #'precompile1 (cdr form))))))
    (when (or (not *in-jvm-compile*) (notinline-p op))
      (return-from precompile-function-call (precompile-cons form)))
    (when (source-transform op)
      (let ((new-form (expand-source-transform form)))
        (when (neq new-form form)
          (return-from precompile-function-call (precompile1 new-form)))))
    (when *enable-inline-expansion*
      (let ((expansion (inline-expansion op)))
        (when expansion
          (let ((explain *explain*))
            (when (and explain (memq :calls explain))
              (format t ";   inlining call to ~S~%" op)))
          (return-from precompile-function-call (precompile1 (expand-inline form expansion))))))
    (cons op (mapcar #'precompile1 (cdr form)))))

(format *error-output* "                *** Patched abcl's precompiler:precompile-identity ***~%")
