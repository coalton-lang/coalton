;;; coalton-mode.el --- Major mode for working with Coalton -*- lexical-binding: t; -*-
;;
;; URL: http://github.com/coalton-lang/coaltom
;; Keywords: languages coalton lisp
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;;
;; This file contains functions for in-Emacs structural operations on
;; Coalton code, including syntax highlighting, indentation and
;; navigation, and command integration with the in-CL operations
;; defined in `slime-coalton.el'.

(require 'treesit)
(require 'lisp-mnt)

(defconst coalton-mode-version
  (eval-when-compile
    (lm-version (or load-file-name buffer-file-name))))

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

(defvar coalton--debug nil
  "Enable debugging.")


;; Fontification

(defconst coalton--builtin-symbol
  (eval-and-compile
    (concat "^"
            (regexp-opt
             '("declare"
               "define"
               "define-instance"
               "define-type"
               "do"
               "let"
               "match"
               "package"))
            "$")))

(defun coalton--font-lock-settings ()
  "Return settings for `treesit-font-lock-settings'."
  (treesit-font-lock-rules
   :feature 'builtin
   :language 'coalton
   `(((list :anchor (symbol (symbol_name) @font-lock-keyword-face))
      (:match ,coalton--builtin-symbol @font-lock-keyword-face)))
   
   :feature 'number
   :language 'coalton
   '((number) @font-lock-number-face)
   
   :feature 'paren
   :language 'coalton
   '((["(" ")"]) @font-lock-bracket-face)
    
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


;; Easy menu

(defvar coalton-easy-menu
  (let ((C '(coalton-available-p)))
    `("Coalton"
      ("Debug"
       [ "Show AST"         slime-coalton--ast ,C ])
      ("Compile"
       [ "Compile File"     slime-coalton--compile-file ,C ]))))

(easy-menu-define menubar-coalton coalton-mode-map "Coalton" coalton-easy-menu)


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
  (let ((treesit-language-source-alist
         `((coalton ,coalton-ts-repo "main"))))
    (unless (treesit-language-available-p 'coalton nil)
      (when (yes-or-no-p "treesitter-coalton is not installed. Clone, build and install it?")
        (treesit-install-language-grammar 'coalton)))))

(defun coalton-mode-variables ()
  "Initialize buffer-local vars."
  (setq-local comment-start "; ")
  (setq-local treesit-simple-imenu-settings
              coalton--imenu-settings)
  (setq-local treesit-font-lock-settings
              (coalton--font-lock-settings))
  (setq-local treesit-font-lock-feature-list
              ;; Amount of decoration, from least to most, cumulative,
              ;; controlled by `treesit-font-lock-level'.
              '((comment)               ; 1
                ()                      ; 2
                (number builtin)        ; 3
                (paren)))               ; 4
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

(defvar coalton--query-package
  (treesit-query-compile
   'coalton
   '(((program (list
                :anchor (symbol name: (symbol_name) @package)
                :anchor (symbol name: (symbol_name) @package-name)))
      (:equal @package "package")))))

(defun coalton-package ()
  (let ((nodes (treesit-query-capture 'coalton coalton--query-package)))
    (treesit-node-text (cdr (assoc 'package-name nodes)) t)))

(defun coalton--find-parent (node pred)
  "Find first parent of NODE matching PRED."
  (cond ((null node)
         nil)
        ((funcall pred node)
         node)
        (t
         (coalton--find-parent (treesit-node-parent node) pred))))

(defun coalton--toplevel-form-p (node)
  "Is NODE a toplevel program element?"
  (and (coalton--list-p node)
       (string-equal "program" (treesit-node-type
                                (treesit-node-parent node)))))

(defun coalton-toplevel-form ()
  "Return the text of the toplevel form at point."
  (when-let ((node (coalton--find-parent (treesit-node-at (point))
                                         #'coalton--toplevel-form-p)))
    (treesit-node-text node t)))

(provide 'coalton-mode)
