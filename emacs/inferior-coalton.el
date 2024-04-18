;;; inferior-coalton.el --- coalton-mode lisp integration -*- lexical-binding: t; -*-
;;
;; Slime extension via `define-slime-contrib' for interactive with a
;; Coalton instance running in a Slime/Swank-managed Lisp subprocess.

(require 'slime)

(defun slime-coalton-mode-hook ()
  (slime-mode 1))

(defun slime-coalton-indentation-update (symbol indent packages)
  ;; Does the symbol have an indentation value that we set?
  (when (equal (get symbol 'coalton-indent-function)
	       (get symbol 'slime-coalton-indent))
    (put symbol 'slime-coalton-indent indent)
    (put symbol 'coalton-indent-function indent)))


;;; Initialization

(defun slime-coalton-init ()
  (add-hook 'coalton-mode-hook 'slime-coalton-mode-hook)
  (add-to-list 'slime-lisp-modes 'coalton-mode))

(defun slime-coalton-unload ()
  (remove-hook 'coalton-mode-hook 'slime-coalton-mode-hook)
  (setq slime-lisp-modes (remove 'coalton-mode slime-lisp-modes)))

(define-slime-contrib slime-coalton
  "Support Coalton language"
  (:authors "Jesse Bouwman <jlbouwman@hrl.com>")
  (:slime-dependencies slime-coalton)
  (:swank-dependencies swank-coalton)
  (:on-load
   (slime-coalton-init)))

(provide 'coalton)
