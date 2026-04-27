(in-package #:mine-tests)

(defun %check-repl-input (rp expected-text expected-cursor)
  (%check (string= expected-text (repl:repl-pane-get-input rp))
          "Expected REPL input ~S, got ~S"
          expected-text
          (repl:repl-pane-get-input rp))
  (%check (= expected-cursor (repl:repl-pane-input-cursor rp))
          "Expected REPL cursor ~D, got ~D"
          expected-cursor
          (repl:repl-pane-input-cursor rp)))

(defun check-repl-structural-editing-pairs-delimiters ()
  (let ((rp (repl:repl-pane-new)))
    (%check (eq wt:Consumed
                (repl:repl-pane-handle-structural-key!
                 rp (input:KeyChar #\() input:ModShift))
            "Expected structural open paren to be consumed")
    (%check-repl-input rp "()" 1)
    (%check (eq wt:Consumed
                (repl:repl-pane-handle-structural-key!
                 rp input:KeyBackspace input:ModNone))
            "Expected structural backspace to be consumed")
    (%check-repl-input rp "" 0)
    (%check (eq wt:Consumed
                (repl:repl-pane-handle-structural-key!
                 rp (input:KeyChar #\") input:ModShift))
            "Expected structural double quote to be consumed")
    (%check-repl-input rp "\"\"" 1)))

(defun check-repl-structural-editing-alt-sexp-motion ()
  (let ((rp (repl:repl-pane-new)))
    (repl:repl-pane-paste-to-input! rp "(foo bar)")
    (%check-repl-input rp "(foo bar)" 9)
    (%check (eq wt:Consumed
                (repl:repl-pane-handle-structural-key!
                 rp (input:KeyChar #\b) input:ModAlt))
            "Expected Alt+b to be consumed")
    (%check-repl-input rp "(foo bar)" 0)
    (%check (eq wt:Consumed
                (repl:repl-pane-handle-structural-key!
                 rp (input:KeyChar #\f) input:ModAlt))
            "Expected Alt+f to be consumed")
    (%check-repl-input rp "(foo bar)" 9)))

(defun check-repl-hint-symbol-extraction ()
  (%check (string= "+"
                   (mine/app/mine::%read-enclosing-symbol-from-string "(+ 1 " 5))
          "Expected REPL hint extraction to find +")
  (%check (string= "map"
                   (mine/app/mine::%read-enclosing-symbol-from-string "(map" 4))
          "Expected REPL hint extraction to handle an incomplete form")
  (%check (null (mine/app/mine::%read-enclosing-symbol-from-string "plain-symbol" 12))
          "Expected no enclosing function outside a form"))
