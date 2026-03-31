;;;; server-main.lisp -- Runtime server entry point.
;;;;
;;;; When mine is invoked with --runtime-server, this module starts
;;;; the server loop that handles evaluation requests from the TUI.

(in-package #:mine/runtime/server-main)

;;; Soft reset -- clean editor state from inherited image

(defun %soft-reset ()
  "Delete editor-only packages and clear ASDF state for a clean subprocess."
  ;; Delete editor packages (in reverse dependency order)
  (dolist (pkg-name '("MINE/APP/MINE" "MINE/APP/SETUP" "MINE/APP/COMMANDS"
                       "MINE/CONFIG/KEYS" "MINE/CONFIG/THEME"
                       "MINE/CONFIG/PARSER" "MINE/CONFIG/TYPES"
                       "MINE/PROJECT/SESSION" "MINE/PROJECT/TREE-MODEL"
                       "MINE/PROJECT/ASDF-WRITER" "MINE/PROJECT/ASDF-PARSER"
                       "MINE/PROJECT/ASDF-MODEL"
                       "MINE/PANE/MINIBUFFER" "MINE/PANE/DEBUGGER"
                       "MINE/PANE/STATUS" "MINE/PANE/OUTPUT"
                       "MINE/PANE/REPL" "MINE/PANE/EDITOR"
                       "MINE/PANE/TREE" "MINE/PANE/MENUBAR"
                       "MINE/WIDGET/INPUT" "MINE/WIDGET/TEXT"
                       "MINE/WIDGET/RENDER" "MINE/WIDGET/FOCUS"
                       "MINE/WIDGET/LAYOUT" "MINE/WIDGET/TYPES"
                       "MINE/SYNTAX/PAREDIT" "MINE/SYNTAX/INDENT"
                       "MINE/SYNTAX/HIGHLIGHT" "MINE/SYNTAX/LEXER"
                       "MINE/SYNTAX/TOKEN"
                       "MINE/EDIT/CLIPBOARD" "MINE/EDIT/OPERATIONS"
                       "MINE/EDIT/UNDO" "MINE/EDIT/CURSOR"
                       "MINE/BUFFER/MANAGER" "MINE/BUFFER/BUFFER"
                       "MINE/BUFFER/GAP"
                       "MINE/EVENT/LOOP" "MINE/EVENT/QUEUE"
                       "MINE/EVENT/TYPES"
                       "MINE/TERM/TERMINAL" "MINE/TERM/SCREEN"
                       "MINE/TERM/INPUT" "MINE/TERM/ESCAPE"
                       "MINE/TERM/COLOR"
                       "MINE/BINDINGS/SIGNAL" "MINE/BINDINGS/TERMINAL"
                       "MINE/BINDINGS/TERMBOX"))
    (ignore-errors (delete-package (find-package pkg-name))))
  ;; Clear ASDF registration for the editor system
  (ignore-errors (asdf:clear-system "mine")))

;;; Entry point

(defun main (&optional port)
  "Start the runtime server on PORT (default: 0 = auto-assign).
PORT can be an integer or a string that will be parsed as an integer.
Blocks until the server finishes (via :quit message or connection loss)."
  (%soft-reset)
  (let ((port-number (etypecase port
                       (null 0)
                       (integer port)
                       (string (or (parse-integer port :junk-allowed t) 0)))))
    (format *error-output* ";; mine runtime server starting (port=~D)~%"
            port-number)
    (force-output *error-output*)
    (handler-case
        (mine/protocol/server:start-server port-number)
      (error (c)
        (format *error-output* ";; mine runtime server fatal error: ~A~%" c)
        (force-output *error-output*)
        (sb-ext:exit :code 1)))))
