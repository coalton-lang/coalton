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

(defun check-repl-structural-close-paren-in-string-inserts ()
  (let ((rp (repl:repl-pane-new)))
    (repl:repl-pane-paste-to-input! rp "(print \"abc\")")
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (%check-repl-input rp "(print \"abc\")" 11)
    (%check (eq wt:Consumed
                (repl:repl-pane-handle-structural-key!
                 rp (input:KeyChar #\)) input:ModShift))
            "Expected structural close paren in a string to be consumed")
    (%check-repl-input rp "(print \"abc)\")" 12)))

(defun check-repl-structural-close-paren-collapses-empty-form ()
  (let ((rp (repl:repl-pane-new)))
    (repl:repl-pane-paste-to-input! rp (format nil "(~%~%)"))
    (dotimes (i 3)
      (declare (ignorable i))
      (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone))
    (%check-repl-input rp (format nil "(~%~%)") 1)
    (%check (eq wt:Consumed
                (repl:repl-pane-handle-structural-key!
                 rp (input:KeyChar #\)) input:ModShift))
            "Expected structural close paren in whitespace-only form to be consumed")
    (%check-repl-input rp "()" 2))
  (let ((rp (repl:repl-pane-new)))
    (repl:repl-pane-paste-to-input! rp (format nil "(~%~%a)"))
    (dotimes (i 4)
      (declare (ignorable i))
      (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone))
    (%check-repl-input rp (format nil "(~%~%a)") 1)
    (%check (eq wt:Consumed
                (repl:repl-pane-handle-structural-key!
                 rp (input:KeyChar #\)) input:ModShift))
            "Expected structural close paren before non-whitespace body to be consumed")
    (%check-repl-input rp (format nil "(~%~%a)") 5)))

(defun check-repl-structural-delimiters-in-string-insert-literals ()
  (flet ((check-one (key modifier expected-text)
           (let ((rp (repl:repl-pane-new)))
             (repl:repl-pane-paste-to-input! rp "(print \"abc\")")
             (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
             (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
             (%check-repl-input rp "(print \"abc\")" 11)
             (%check (eq wt:Consumed
                         (repl:repl-pane-handle-structural-key!
                          rp key modifier))
                     "Expected structural delimiter in a string to be consumed")
             (%check-repl-input rp expected-text 12))))
    (check-one (input:KeyChar #\() input:ModShift "(print \"abc(\")")
    (check-one (input:KeyChar #\[) input:ModNone "(print \"abc[\")")
    (check-one (input:KeyChar #\]) input:ModNone "(print \"abc]\")")))

(defun check-repl-structural-delimiters-in-line-comment-insert-literals ()
  (flet ((check-one (key modifier expected-text)
           (let ((rp (repl:repl-pane-new)))
             (repl:repl-pane-paste-to-input! rp "(foo) ; comment")
             (%check-repl-input rp "(foo) ; comment" 15)
             (%check (eq wt:Consumed
                         (repl:repl-pane-handle-structural-key!
                          rp key modifier))
                     "Expected structural delimiter in a line comment to be consumed")
             (%check-repl-input rp expected-text (length expected-text)))))
    (check-one (input:KeyChar #\() input:ModShift "(foo) ; comment(")
    (check-one (input:KeyChar #\)) input:ModShift "(foo) ; comment)")
    (check-one (input:KeyChar #\[) input:ModNone "(foo) ; comment[")
    (check-one (input:KeyChar #\]) input:ModNone "(foo) ; comment]")
    (check-one (input:KeyChar #\") input:ModShift "(foo) ; comment\"")))

(defun check-repl-structural-doublequote-in-string ()
  (let ((rp (repl:repl-pane-new)))
    (repl:repl-pane-paste-to-input! rp "(print \"abc\")")
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (%check-repl-input rp "(print \"abc\")" 11)
    (%check (eq wt:Consumed
                (repl:repl-pane-handle-structural-key!
                 rp (input:KeyChar #\") input:ModShift))
            "Expected structural double quote at string end to be consumed")
    (%check-repl-input rp "(print \"abc\")" 12))
  (let ((rp (repl:repl-pane-new)))
    (repl:repl-pane-paste-to-input! rp "(print \"abc\")")
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (%check-repl-input rp "(print \"abc\")" 10)
    (%check (eq wt:Consumed
                (repl:repl-pane-handle-structural-key!
                 rp (input:KeyChar #\") input:ModShift))
            "Expected structural double quote in string body to be consumed")
    (%check-repl-input rp "(print \"ab\\\"c\")" 12)))

(defun check-repl-structural-escaped-quote-deletes-as-unit ()
  (let ((rp (repl:repl-pane-new)))
    (repl:repl-pane-paste-to-input! rp "(print \"ab\\\"c\")")
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (%check-repl-input rp "(print \"ab\\\"c\")" 12)
    (%check (eq wt:Consumed
                (repl:repl-pane-handle-structural-key!
                 rp input:KeyBackspace input:ModNone))
            "Expected backspace after escaped quote to be consumed")
    (%check-repl-input rp "(print \"abc\")" 10))
  (let ((rp (repl:repl-pane-new)))
    (repl:repl-pane-paste-to-input! rp "(print \"ab\\\"c\")")
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (%check-repl-input rp "(print \"ab\\\"c\")" 11)
    (%check (eq wt:Consumed
                (repl:repl-pane-handle-structural-key!
                 rp input:KeyBackspace input:ModNone))
            "Expected backspace before escaped quote to be consumed")
    (%check-repl-input rp "(print \"abc\")" 10))
  (let ((rp (repl:repl-pane-new)))
    (repl:repl-pane-paste-to-input! rp "(print \"ab\\\"c\")")
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (%check-repl-input rp "(print \"ab\\\"c\")" 10)
    (%check (eq wt:Consumed
                (repl:repl-pane-handle-structural-key!
                 rp input:KeyDelete input:ModNone))
            "Expected delete at escaped quote to be consumed")
    (%check-repl-input rp "(print \"abc\")" 10))
  (let ((rp (repl:repl-pane-new)))
    (repl:repl-pane-paste-to-input! rp "(print \"ab\\\"c\")")
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (repl:repl-pane-handle-key! rp input:KeyLeft input:ModNone)
    (%check-repl-input rp "(print \"ab\\\"c\")" 11)
    (%check (eq wt:Consumed
                (repl:repl-pane-handle-structural-key!
                 rp input:KeyDelete input:ModNone))
            "Expected delete before escaped quote to be consumed")
    (%check-repl-input rp "(print \"abc\")" 10)))

(defun check-paredit-matching-ignores-delimiters-in-strings ()
  (let ((gb (gap:gap-from-string "(print \"abc)\")")))
    (%check (eq coalton:none (paredit:scan-matching-open-paren gb 11))
            "Expected close paren inside string to have no structural match")
    (%check (eql 0 (paredit:scan-matching-open-paren gb 13))
            "Expected code close paren to still have a structural match"))
  (let ((gb (gap:gap-from-string "[vector \"x]\"]")))
    (%check (eq coalton:none (paredit:scan-matching-open-bracket gb 10))
            "Expected close bracket inside string to have no structural match")
    (%check (eql 0 (paredit:scan-matching-open-bracket gb 12))
            "Expected code close bracket to still have a structural match"))
  (let ((gb (gap:gap-from-string "(foo) ; )")))
    (%check (eq coalton:none (paredit:scan-matching-open-paren gb 8))
            "Expected close paren inside line comment to have no structural match")
    (%check (eql 0 (paredit:scan-matching-open-paren gb 4))
            "Expected code close paren before line comment to still have a structural match"))
  (let ((gb (gap:gap-from-string "[foo] ; ]")))
    (%check (eq coalton:none (paredit:scan-matching-open-bracket gb 8))
            "Expected close bracket inside line comment to have no structural match")
    (%check (eql 0 (paredit:scan-matching-open-bracket gb 4))
            "Expected code close bracket before line comment to still have a structural match")))

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

(defun check-editor-completion-prefix-extraction ()
  (let* ((text "(alpha beta)")
         (gb (gap:gap-from-string text)))
    (%check (string= "beta"
                     (mine/app/completion:extract-symbol-prefix gb 11))
            "Expected editor completion prefix beta")
    (%check (string= ""
                     (mine/app/completion:extract-symbol-prefix gb 0))
            "Expected no editor completion prefix at buffer start")))
