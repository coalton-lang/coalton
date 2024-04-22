;;; slime-coalton.el --- coalton-mode lisp integration -*- lexical-binding: t; -*-
;;
;; Slime extension via `define-slime-contrib' for interaction with a
;; Coalton instance running in a Slime-managed Lisp subprocess.

(require 'slime)

(defun coalton-available-p ()
  (and (not (null slime-net-processes))
       (fboundp 'xxx)))

(cl-defmacro slime-coalton--show ((name) &body body)
  (declare (indent 1))
  `(with-current-buffer (get-buffer-create ,name)
     (erase-buffer)
     ,@body
     (display-buffer (current-buffer))
     (current-buffer)))

(defun slime-coalton--buffer-name (type)
  (format "*coalton-%s*" (symbol-name type)))

(defun slime-coalton--popup-buffer (type)
  (let ((name (slime-coalton--buffer-name type)))
    (slime-coalton--show (name)
      (current-buffer))))

(defun slime-coalton--popup (type value)
  (pop-to-buffer (slime-coalton--popup-buffer type))
  (erase-buffer)
  (insert value)
  (goto-char (point-min)))

(defun slime-coalton--eval (sexp cont)
  (declare (indent 1))
  (slime-rex (cont)
      (sexp "swank")
    ((:ok result)
     (when cont
       (funcall cont result)))
    ((:abort condition)
     (message "Evaluation aborted on %s." condition))))

(defun slime-coalton--ast ()
  "Display the AST of the definition at point."
  (interactive)
  (let ((form (coalton-definition-at-point))
        (package (coalton-package)))
    (slime-coalton--eval `(swank:swank-coalton-ast `,form `,package)
      (lambda (result)
        (slime-coalton--popup 'ast result)))))


;;; Initialization

(defun slime-coalton-init ()
  (message "slime-coalton.el: slime-coalton-init"))

(define-slime-contrib slime-coalton
  "Support Coalton language"
  (:authors "Jesse Bouwman <jlbouwman@hrl.com>")
  (:swank-dependencies swank-coalton))

(defun coalton ()
  (interactive)
  (message "slime-coalton.el: coalton"))

(provide 'slime-coalton)
