;;; coalton-mode.el --- Major mode for working with Coalton -*- lexical-binding: t; -*-

(require 'treesit)

(defvar coalton-mode-map
  (make-sparse-keymap))

(defvar coalton-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry '(0 . 127) "_" table)

    (modify-syntax-entry '(?0 . ?9) "w" table)
    (modify-syntax-entry '(?a . ?z) "w" table)
    (modify-syntax-entry '(?A . ?Z) "w" table)

    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\f " " table)

    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)

    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    table))

(defvar coalton--debug t
  "Enable debugging.")

(defun coalton--load-grammar ()
  "Install grammar."
  (let ((grammars '((coalton "https://github.com/jbouwman/tree-sitter-coalton.git" "main"))))
    (dolist (grammar grammars)
      (let ((language (car grammar)))
        (unless (treesit-language-available-p language nil)
          (let ((treesit-language-source-alist grammars))
            (treesit-install-language-grammar language)))))))

(defun coalton--font-lock-settings ()
  "Get settings for `treesit-font-lock-settings'."
   (treesit-font-lock-rules
    :feature 'number
    :language 'coalton
    '((number) @font-lock-number-face)

    :feature 'symbol
    :language 'coalton
    '((symbol) @font-lock-type-face)))

(defun coalton-mode-variables ()
  "Init buffer-local vars."
  (setq-local treesit-font-lock-settings
              (coalton--font-lock-settings)))

;;;###autoload
(define-derived-mode coalton-mode prog-mode "Coalton"
  "Major mode for working with Coalton.

\\{coalton-mode-map}"
  :syntax-table coalton-mode-syntax-table
  (coalton--load-grammar)
  (when (treesit-ready-p 'coalton)
    (treesit-parser-create 'coalton)
    (coalton-mode-variables)
    (when coalton--debug
      (setq-local treesit--indent-verbose t)
      (setq-local treesit--font-lock-verbose t)
      (treesit-inspect-mode))
    (treesit-major-mode-setup)))

(add-to-list 'auto-mode-alist '("\\.coalton\\'" . coalton-mode))
