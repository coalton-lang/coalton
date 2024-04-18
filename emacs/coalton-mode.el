;;; coalton-mode.el --- Major mode for working with Coalton -*- lexical-binding: t; -*-
;;
;; This file contains functions for in-Emacs structural operations on
;; Coalton code, including syntax highlighting, indentation and
;; navigation, and command integration with the in-CL operations
;; defined in `inferior-coalton.el'.

(require 'treesit)

(defvar coalton-ts-repo
  "https://github.com/jbouwman/tree-sitter-coalton.git")

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


;; Fontification

(defun coalton--font-lock-settings ()
  "Return settings for `treesit-font-lock-settings'."
  (treesit-font-lock-rules
   :feature 'symbol
   :language 'coalton
   '((symbol) @font-lock-builtin-face) ; what's the right choice?

   :feature 'number
   :language 'coalton
   '((number) @font-lock-number-face)

   :feature 'comment
   :language 'coalton
   '((comment) @font-lock-comment-face)))


;; Indentation

(defun coalton--indent-rules ()
  "Return rules for `treesit-simple-indent-rules'."
  `((coalton
     ((parent-is "list") parent 2))))


;; Indexing and navigation

(defun coalton--node-type-p (type node)
  "Does NODE have tree-sitter TYPE?"
  (string-equal type (treesit-node-type node)))
  
(defun coalton--list-p (node)
  "Is NODE a list?"
  (coalton--node-type-p "list" node))

(defun coalton--symbol-p (node)
  "Is NODE a symbol?"
  (coalton--node-type-p "symbol" node))

(defun coalton--symbol-name (node)
  "If NODE is a symbol, return its name."
  (when (coalton--symbol-p node)
    (treesit-node-text node t)))

(defun coalton--symbol-name-p (name node)
  "Is NODE a symbol named NAME?"
  (and (coalton--symbol-p node)
       (string-equal name (coalton--symbol-name node))))

(defun coalton--definition-type (node)
  "If NODE is a definition, return the definition's type."
  (when (coalton--list-p node)
    (let ((node (treesit-node-child node 0 t)))
      (when (coalton--symbol-p node)
        (coalton--symbol-name node)))))

(defun coalton--definition-p (type node)
  "Is NODE a definition of type TYPE?"
  (string-equal type (coalton--definition-type node)))

(defun coalton--definition-name (node)
  "If NODE is a definition, return its name."
  (when (coalton--list-p node)
    (let ((node (treesit-node-child node 1 t)))
      (cond ((coalton--list-p node)
             (let ((node (treesit-node-child node 0 t)))
               (when (coalton--symbol-p node)
                 (coalton--symbol-name node))))
            ((coalton--symbol-p node)
             (coalton--symbol-name node))))))


;; Imenu

(defun coalton--type-definition-p (node)
  "Does NODE represent a type definition?"
  (coalton--definition-p "define-type" node))

(defun coalton--instance-definition-p (node)
  "Does NODE represent an instanclue definition?"
  (coalton--definition-p "define-instance" node))

(defun coalton--function-definition-p (node)
  "Does NODE represent a function definition?"
  (coalton--definition-p "define" node))

(defvar coalton--imenu-settings
  '(("Type" "list"
     coalton--type-definition-p
     coalton--definition-name)
    ("Instance" "list"
     coalton--instance-definition-p
     coalton--definition-name)
    ("Function" "list"
     coalton--function-definition-p
     coalton--definition-name))
  "The value for `treesit-simple-imenu-settings'.")


;; Initialization

(defun coalton--load-grammar ()
  "Install grammar."
  (let ((grammars `((coalton ,coalton-ts-repo "main"))))
    (dolist (grammar grammars)
      (unless (treesit-language-available-p (car grammar) nil)
        (let ((treesit-language-source-alist grammars))
          (treesit-install-language-grammar (car grammar)))))))

(defun coalton-mode-variables ()
  "Initialize buffer-local vars."
  (setq-local treesit-simple-imenu-settings
              coalton--imenu-settings)
  (setq-local treesit-font-lock-settings
              (coalton--font-lock-settings))
  (setq-local treesit-font-lock-feature-list
              ;; Amount of decoration, from least to most, cumulative,
              ;; controlled by `treesit-font-lock-level'.
              '((comment)               ; 1
                ()                      ; 2
                (number symbol)         ; 3
                ()))                    ; 4
  (setq-local treesit-simple-indent-rules
              (coalton--indent-rules)))

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

(provide 'coalton-mode)
