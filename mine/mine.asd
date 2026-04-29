;;; mine.asd -- A TUI-based IDE for Coalton and Common Lisp.

;;; Runtime server (no terminal dependency)
(asdf:defsystem "mine/runtime"
  :description "Runtime server for the mine IDE."
  :depends-on ("coalton" "coalton/xmath")
  :defsystem-depends-on ("coalton-asdf")
  :pathname "src/"
  :serial t
  :components
  ((:file "package")

   ;; CL bindings -- runtime-only (no terminal/signal)
   (:module "bindings"
    :serial t
    :components ((:file "socket")
                 (:file "process")
                 (:file "process-unix" :if-feature :unix)
                 (:file "process-win32" :if-feature :win32)
                 (:file "thread")))

   ;; Protocol
   (:module "protocol"
    :serial t
    :components ((:ct-file "wire")
                 (:ct-file "messages")
                 (:ct-file "client")
                 (:file "diagnostics")
                 (:file "server")
                 (:ct-file "lifecycle")))

   ;; Runtime server (CL side)
   (:module "runtime"
    :serial t
    :components ((:file "eval")
                 (:file "introspect")
                 (:file "asdf")
                 (:file "server-main")))))

;;; Full TUI editor (depends on mine/runtime)
(asdf:defsystem "mine"
  :description "A TUI-based IDE for Coalton and Common Lisp."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :defsystem-depends-on ("coalton-asdf")
  :depends-on ("mine/runtime")
  :in-order-to ((asdf:test-op (asdf:test-op "mine-tests")))
  :pathname "src/"
  :serial t
  :components
  ((:file "version")

   ;; CL bindings -- editor-only (platform, terminal, signal)
   (:module "bindings"
    :serial t
    :components ((:file "platform-package")
                 (:file "platform-unix" :if-feature :unix)
                 (:file "platform-win32" :if-feature :win32)
                 (:file "terminal")
                 (:file "signal")))

   ;; Shared text layout helpers
   (:module "text"
    :serial t
    :components ((:ct-file "width")))

   ;; Terminal abstraction
   (:module "term"
    :serial t
    :components ((:ct-file "color")
                 (:ct-file "escape")
                 (:ct-file "input")
                 (:ct-file "screen")
                 (:ct-file "terminal")))

   ;; Event system
   (:module "event"
    :serial t
    :components ((:ct-file "types")
                 (:ct-file "queue")))

   ;; Utilities and bindings
   (:module "utils"
    :serial t
    :components ((:ct-file "file-system")))

   ;; Undo tree (no buffer dependency; loaded early so Buffer can embed it)
   (:module "edit-undo"
    :pathname "edit"
    :components ((:ct-file "undo")))

   ;; Text buffer
   (:module "buffer"
    :serial t
    :components ((:ct-file "gap")
                 (:ct-file "buffer")
                 (:ct-file "manager")))

   ;; Syntax core (token, lexer, highlight, indent -- no edit dependency)
   (:module "syntax-core"
    :pathname "syntax"
    :serial t
    :components ((:ct-file "token")
                 (:ct-file "lexer")
                 (:ct-file "highlight")
                 (:ct-file "indent")))

   ;; Editing operations (depends on syntax/indent)
   (:module "edit"
    :serial t
    :components ((:ct-file "cursor")
                 (:ct-file "operations")))

   ;; Paredit (depends on edit/cursor, edit/undo)
   (:module "syntax-paredit"
    :pathname "syntax"
    :serial t
    :components ((:ct-file "paredit")))

   ;; Widget framework
   (:module "widget"
    :serial t
    :components ((:ct-file "types")
                 (:ct-file "focus")
                 (:ct-file "render")
                 (:ct-file "text")
                 (:ct-file "input")
                 (:ct-file "spinner")))

   ;; Pane implementations (depends on syntax/lexer, syntax/highlight)
   (:module "pane"
    :serial t
    :components ((:ct-file "menubar")
                 (:ct-file "tree")
                 (:ct-file "editor")
                 (:ct-file "repl")
                 (:ct-file "status")
                 (:ct-file "debugger")
                 (:ct-file "minibuffer")))

   ;; Project management
   (:module "project"
    :serial t
    :components ((:ct-file "asdf-model")
                 (:ct-file "asdf-parser")
                 (:ct-file "asdf-writer")))

   ;; Configuration
   (:module "config"
    :serial t
    :components ((:ct-file "types")
                 (:ct-file "parser")
                 (:ct-file "keys")))

   ;; Application entry point
   (:module "app"
    :serial t
    :components ((:ct-file "setup")
                 (:ct-file "help")
                 (:ct-file "tutorial")
                 (:ct-file "clipboard")
                 (:ct-file "completion")
                 (:ct-file "dialogs")
                 (:ct-file "find")
                 (:ct-file "build")
                 (:ct-file "symbols")
                 (:ct-file "keyboard-debugger")
                 (:file "diagnostics")
                 (:ct-file "mine")
                 (:file "executable")))))
