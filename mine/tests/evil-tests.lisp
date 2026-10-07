(in-package #:mine-tests)

;;; Harness

(defun %evil-buffer (text &key (name "evil-test.ct"))
  "A fresh buffer that holds TEXT. NAME selects the buffer mode."
  (let ((buffer (buf:buffer-new-file (buf:BufferId 0) name)))
    (gap:gap-insert-string! (buf:buffer-gap buffer) 0 text)
    buffer))

(defun %evil-parse-keys (keys)
  "Turn KEYS into a list of (key modifier char). <Esc> <CR> <BS> <Del> <lt>
are special tokens; every other char is a plain KeyChar."
  (let ((result '()) (i 0) (n (length keys)))
    (loop while (< i n) do
      (let ((c (char keys i)))
        (if (char= c #\<)
            (let* ((close (position #\> keys :start i))
                   (name (subseq keys (1+ i) close)))
              (push (cond ((string-equal name "Esc") (list input:KeyEscape input:ModNone nil))
                          ((string-equal name "CR") (list input:KeyEnter input:ModNone #\Newline))
                          ((string-equal name "BS") (list input:KeyBackspace input:ModNone :backspace))
                          ((string-equal name "Del") (list input:KeyDelete input:ModNone nil))
                          ((string-equal name "lt") (list (input:KeyChar #\<) input:ModNone #\<))
                          (t (error "Unknown key token ~S" name)))
                    result)
              (setf i (1+ close)))
            (progn
              (push (list (input:KeyChar c) input:ModNone c) result)
              (incf i)))))
    (nreverse result)))

(defun %evil-feed (es buffer cs events structural)
  "Feed EVENTS to the evil layer. Keys that pass through in insert mode are
typed into the buffer, as the editor would. Return the result labels."
  (let ((labels '()))
    (dolist (ev events)
      (destructuring-bind (key mod ch) ev
        (let* ((result (evil:evil-handle-key! es buffer cs key mod
                                              (if structural coalton:True coalton:False)))
               (label (evil:evil-result-label result)))
          (push label labels)
          (when (and (string= label "pass") ch)
            (cond ((eq ch :backspace)
                   (ops:delete-backward! buffer (buf:buffer-undo buffer) cs))
                  (t
                   (ops:insert-char! buffer (buf:buffer-undo buffer) cs ch)))))))
    (nreverse labels)))

(defun %evil-run (text keys &key structural (name "evil-test.ct") (pos 0))
  "Run KEYS on a buffer with TEXT. Return text, cursor, evil state, buffer,
cursor state and the list of result labels."
  (let* ((buffer (%evil-buffer text :name name))
         (gb (buf:buffer-gap buffer))
         (cs (cursor:cursor-new))
         (es (evil:evil-new coalton:True)))
    (cursor:cursor-move-to-buffer-position! gb cs pos)
    (let ((labels (%evil-feed es buffer cs (%evil-parse-keys keys) structural)))
      (values (gap:gap-to-string gb) (cursor:cursor-position cs) es buffer cs labels))))

(defun %check-evil (text keys expected-text expected-pos &key structural (pos 0) (name "evil-test.ct"))
  (multiple-value-bind (out cur) (%evil-run text keys :structural structural :pos pos :name name)
    (%check (string= expected-text out)
            "Keys ~S on ~S: expected text ~S, got ~S" keys text expected-text out)
    (when expected-pos
      (%check (= expected-pos cur)
              "Keys ~S on ~S: expected cursor ~D, got ~D" keys text expected-pos cur))))

(defun %lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(defun %undo-once! (buffer cs)
  "Apply one undo step. Return T when a step existed."
  (let ((entry (app::%coalton-optional-value-or-nil
                (undo:undo-undo! (buf:buffer-undo buffer)))))
    (when entry
      (app::apply-undo-ops
       (buf:buffer-gap buffer)
       (funcall (find-symbol "UNDOENTRY/UNDOENTRY-_0" "MINE/EDIT/UNDO") entry))
      (cursor:cursor-move-to-position!
       cs
       (funcall (find-symbol "UNDOENTRY/UNDOENTRY-_1" "MINE/EDIT/UNDO") entry))
      t)))

(defun %blocked-p (labels)
  (some (lambda (l) (and (>= (length l) 8) (string= "message:" (subseq l 0 8)))) labels))

;;; Motions

(defun check-evil-motions-hjkl-and-clamp ()
  (let ((text (%lines "abc" "def")))
    (%check-evil text "lll" text 2)
    (%check-evil text "lllj" text 6)
    (%check-evil text "$" text 2)
    (%check-evil text "$j" text 6)
    (%check-evil text "jh" text 4)
    (%check-evil text "jk" text 0)
    (%check-evil text "ll0" text 0)
    (%check-evil text "<CR>" text 4)
    (%check-evil text "l<BS>" text 0)))

(defun check-evil-word-motions ()
  (let ((text "foo bar-baz (qux)"))
    (%check-evil text "w" text 4)
    (%check-evil text "ww" text 13)
    (%check-evil text "www" text 16)
    (%check-evil text "e" text 2)
    (%check-evil text "ee" text 10)
    (%check-evil text "wwb" text 4)
    (%check-evil text "$b" text 13)))

(defun check-evil-line-motions ()
  (let ((text (%lines "  one" "two" " three")))
    (%check-evil text "G" text 11)
    (%check-evil text "Ggg" text 2)
    (%check-evil text "2G" text 6)
    (%check-evil text "^" text 2)
    (%check-evil text "$" text 4)
    (%check-evil text "3G0" text 10)))

(defun check-evil-counts ()
  (%check-evil "abcdef" "3l" "abcdef" 3)
  (%check-evil "a b c d" "2w" "a b c d" 4)
  (%check-evil (%lines "a" "b" "c") "2j" (%lines "a" "b" "c") 4)
  (%check-evil "abcdef" "10l" "abcdef" 5))

;;; Insert mode

(defun check-evil-insert-entry ()
  (multiple-value-bind (out cur es) (%evil-run "abc" "i")
    (declare (ignore out))
    (%check (= 0 cur) "i keeps the cursor")
    (%check (eq evil:EvilInsert (evil:evil-mode es)) "i enters insert mode"))
  (multiple-value-bind (out cur) (%evil-run "abc" "a")
    (declare (ignore out))
    (%check (= 1 cur) "a moves one right, got ~D" cur))
  (multiple-value-bind (out cur) (%evil-run "abc" "A")
    (declare (ignore out))
    (%check (= 3 cur) "A moves to the line end, got ~D" cur))
  (multiple-value-bind (out cur) (%evil-run "  ab" "I")
    (declare (ignore out))
    (%check (= 2 cur) "I moves to the first non-blank, got ~D" cur))
  (%check-evil "abc" "ix<Esc>" "xabc" 0)
  (%check-evil "abc" "A!<Esc>" "abc!" 3)
  (%check-evil "abc" "llaZ<Esc>" "abcZ" 3)
  (multiple-value-bind (out cur es) (%evil-run "abc" "ix<Esc>")
    (declare (ignore out cur))
    (%check (eq evil:EvilNormal (evil:evil-mode es)) "Esc returns to normal mode")))

;;; Single-key edits

(defun check-evil-x-and-big-x ()
  (%check-evil "abc" "x" "bc" 0)
  (%check-evil "abc" "lx" "ac" 1)
  (%check-evil "abc" "$x" "ab" 1)
  (%check-evil "abcdef" "3x" "def" 0)
  (%check-evil "abc" "5x" "" 0)
  (%check-evil "abc" "X" "abc" 0)
  (%check-evil "abc" "lX" "bc" 0)
  (%check-evil "abc" "$X" "ac" 1)
  (%check-evil "abc" "<Del>" "bc" 0)
  (%check-evil (%lines "" "x") "x" (%lines "" "x") 0))

(defun check-evil-delete-operators ()
  (let ((text (%lines "foo bar baz" "qux")))
    (%check-evil text "dw" (%lines "bar baz" "qux") 0)
    (%check-evil text "wdw" (%lines "foo baz" "qux") 4)
    (%check-evil text "wwdw" (%lines "foo bar " "qux") 7)
    (%check-evil text "wd$" (%lines "foo " "qux") 3)
    (%check-evil text "wD" (%lines "foo " "qux") 3)
    (%check-evil text "dd" "qux" 0)
    (%check-evil text "jdd" "foo bar baz" 0)
    (%check-evil text "d0" text 0)
    (%check-evil text "wd0" (%lines "bar baz" "qux") 0)
    (%check-evil text "wde" (%lines "foo  baz" "qux") 4)
    (%check-evil text "wwdb" (%lines "foo baz" "qux") 4))
  (%check-evil (%lines "a" "b" "c") "2dd" "c" 0)
  (%check-evil (%lines "a" "b" "c") "dj" "c" 0)
  (%check-evil (%lines "a" "b" "c") "Gdj" (%lines "a" "b" "c") 4)
  (%check-evil (%lines "a" "b" "c") "dk" (%lines "a" "b" "c") 0)
  (%check-evil (%lines "a" "b" "c") "jdk" "c" 0)
  (%check-evil (%lines "a" "b" "c") "dG" "" 0)
  (%check-evil (%lines "a" "b" "c") "Gdgg" "" 0)
  (%check-evil (%lines "a" "b" "c") "d2G" "c" 0))

(defun check-evil-change-operators ()
  (multiple-value-bind (out cur es) (%evil-run "foo bar" "cw")
    (%check (string= " bar" out) "cw deletes to the word end, got ~S" out)
    (%check (= 0 cur) "cw keeps the cursor at the start")
    (%check (eq evil:EvilInsert (evil:evil-mode es)) "cw enters insert mode"))
  (%check-evil "foo bar" "cwxy<Esc>" "xy bar" 1)
  (%check-evil (%lines "  foo" "bar") "ccz<Esc>" (%lines "z" "bar") 0)
  (%check-evil "foobar" "llC!<Esc>" "fo!" 2)
  (%check-evil "abc" "sZ<Esc>" "Zbc" 0)
  (%check-evil (%lines "abc" "def") "Sq<Esc>" (%lines "q" "def") 0)
  (%check-evil (%lines "a" "b" "c") "cjq<Esc>" (%lines "q" "c") 0))

(defun check-evil-yank-and-put ()
  (let ((text (%lines "one" "two")))
    (%check-evil text "yyp" (%lines "one" "one" "two") 4)
    (%check-evil text "yyP" (%lines "one" "one" "two") 0)
    (%check-evil text "jyyp" (%lines "one" "two" "two") 8)
    (%check-evil text "Yp" (%lines "one" "one" "two") 4))
  (%check-evil "one two" "ywp" "oone ne two" 4)
  (%check-evil "one two" "ywP" "one one two" 3)
  (%check-evil "one two" "yw$p" "one twoone " 10)
  (%check-evil (%lines "a" "b") "ddp" (%lines "b" "a") 2)
  (%check-evil "ab" "yl3p" "aaaab" 3)
  (%check-evil "ab" "p" "ab" 0))

(defun check-evil-join ()
  (%check-evil (%lines "foo" "  bar" "baz") "J" (%lines "foo bar" "baz") 3)
  (%check-evil (%lines "foo" "  bar" "baz") "3J" "foo bar baz" 7)
  (%check-evil (%lines "(a" ")") "J" "(a)" 2)
  (%check-evil (%lines "" "foo") "J" "foo" 0)
  (%check-evil (%lines "foo " "bar") "J" "foo bar" 4)
  (%check-evil "foo" "J" "foo" 0))

(defun check-evil-replace ()
  (%check-evil "abc" "rx" "xbc" 0)
  (%check-evil "abc" "l2rz" "azz" 2)
  (%check-evil "ab" "3rq" "ab" 0)
  (%check-evil "abc" "r<Esc>x" "bc" 0))

;;; Visual mode

(defun check-evil-visual-charwise ()
  (multiple-value-bind (out cur es) (%evil-run "abcdef" "vlld")
    (%check (string= "def" out) "vlld deletes three chars, got ~S" out)
    (%check (= 0 cur) "vlld leaves the cursor at the start")
    (%check (eq evil:EvilNormal (evil:evil-mode es)) "d leaves visual mode"))
  (%check-evil "abcdef" "vlyp" "aabbcdef" 2)
  (multiple-value-bind (out cur es buffer cs) (%evil-run "abcdef" "vl<Esc>")
    (declare (ignore out buffer))
    (%check (= 1 cur) "Esc keeps the cursor")
    (%check (eq evil:EvilNormal (evil:evil-mode es)) "Esc leaves visual mode")
    (%check (null (app::%coalton-optional-value-or-nil (cursor:cursor-selection-anchor cs)))
            "Esc clears the selection"))
  (%check-evil (%lines "abc" "def") "lvjd" "af" 1)
  (%check-evil "abcdef" "lvlcX<Esc>" "aXdef" 1)
  (multiple-value-bind (out cur es) (%evil-run "abc" "vv")
    (declare (ignore out cur))
    (%check (eq evil:EvilNormal (evil:evil-mode es)) "v twice leaves visual mode")))

(defun check-evil-visual-line ()
  (%check-evil (%lines "a" "b" "c") "Vjd" "c" 0)
  (%check-evil (%lines "a" "b" "c") "GVd" (%lines "a" "b") 2)
  (%check-evil (%lines "a" "b" "c") "jVkd" "c" 0)
  (%check-evil (%lines "a" "b" "c") "Vyjp" (%lines "a" "b" "a" "c") 4)
  (multiple-value-bind (out cur es) (%evil-run (%lines "a" "b") "V")
    (declare (ignore out cur))
    (%check (eq evil:EvilVisualLine (evil:evil-mode es)) "V enters visual line mode"))
  (multiple-value-bind (out cur es) (%evil-run (%lines "a" "b") "V<Esc>")
    (declare (ignore out cur))
    (%check (eq evil:EvilNormal (evil:evil-mode es)) "Esc leaves visual line mode")))

;;; Commands for the application

(defun check-evil-commands ()
  (flet ((last-label (keys)
           (multiple-value-bind (out cur es buffer cs labels) (%evil-run "abc" keys)
             (declare (ignore out cur es buffer cs))
             (car (last labels)))))
    (%check (string= "open-below" (last-label "o")) "o asks to open a line below")
    (%check (string= "open-above" (last-label "O")) "O asks to open a line above")
    (%check (string= "undo" (last-label "u")) "u asks for undo")
    (%check (string= "ex-prompt" (last-label ":")) ": asks for the ex prompt")
    (%check (string= "search" (last-label "/")) "/ asks for search")
    (%check (string= "search-next" (last-label "n")) "n asks for the next match")
    (%check (string= "search-prev" (last-label "N")) "N asks for the previous match"))
  (multiple-value-bind (out cur es) (%evil-run "abc" "o")
    (declare (ignore out cur))
    (%check (eq evil:EvilInsert (evil:evil-mode es)) "o enters insert mode")))

(defun check-evil-parse-ex ()
  (flet ((label (text)
           (let ((cmd (app::%coalton-optional-value-or-nil (evil:evil-parse-ex text))))
             (and cmd (evil:evil-result-label (evil:EvilCommand cmd))))))
    (%check (string= "save" (label "w")) ":w saves")
    (%check (string= "quit" (label "q")) ":q quits")
    (%check (string= "quit" (label "q!")) ":q! quits")
    (%check (string= "save-quit" (label "wq")) ":wq saves and quits")
    (%check (string= "save-quit" (label "x")) ":x saves and quits")
    (%check (string= "goto-line:12" (label " 12 ")) ":12 goes to a line")
    (%check (null (label "bogus")) "unknown commands parse to None")
    (%check (null (label "")) "an empty command parses to None")))

(defun check-evil-pass-through ()
  (let* ((buffer (%evil-buffer "abc"))
         (cs (cursor:cursor-new))
         (es (evil:evil-new coalton:True)))
    (flet ((label (key mod)
             (evil:evil-result-label
              (evil:evil-handle-key! es buffer cs key mod coalton:False))))
      (%check (string= "pass" (label (input:KeyCtrl #\s) input:ModNone)) "Ctrl keys pass through")
      (%check (string= "pass" (label (input:KeyChar #\c) input:ModAlt)) "Alt keys pass through")
      (%check (string= "pass" (label input:KeyUp input:ModNone)) "arrows pass through")
      (%check (string= "pass" (label input:KeyTab input:ModNone)) "Tab passes through")
      (%check (string= "edited" (label (input:KeyChar #\D) input:ModShift)) "shifted keys count")
      (%check (string= "handled" (label (input:KeyChar #\i) input:ModNone)) "i is handled")
      (%check (string= "pass" (label (input:KeyChar #\x) input:ModNone)) "typing in insert mode passes through")
      (%check (string= "pass" (label input:KeyEnter input:ModNone)) "Enter in insert mode passes through")
      (%check (string= "handled" (label input:KeyEscape input:ModNone)) "Esc leaves insert mode"))))

(defun check-evil-disabled-state-is-inert ()
  (let ((es (evil:evil-new coalton:False)))
    (%check (not (evil:evil-enabled? es)) "evil-new honours the flag")
    (evil:evil-set-enabled! es coalton:True)
    (%check (evil:evil-enabled? es) "evil-set-enabled! turns it on")
    (%check (string= "NORMAL" (evil:evil-mode-label es)) "the initial mode is normal")))

;;; Undo

(defun check-evil-insert-session-is-one-undo ()
  (multiple-value-bind (out cur es buffer cs) (%evil-run "" "ihello<Esc>")
    (declare (ignore cur es))
    (%check (string= "hello" out) "typing inserts text, got ~S" out)
    (%check (%undo-once! buffer cs) "one undo step exists")
    (%check (string= "" (gap:gap-to-string (buf:buffer-gap buffer)))
            "one undo reverts the whole insert session")
    (%check (not (%undo-once! buffer cs)) "no second undo step exists"))
  (multiple-value-bind (out cur es buffer cs) (%evil-run "foo bar" "cwxyz<Esc>")
    (declare (ignore cur es))
    (%check (string= "xyz bar" out) "cw plus typing, got ~S" out)
    (%check (%undo-once! buffer cs) "one undo step exists for cw")
    (%check (string= "foo bar" (gap:gap-to-string (buf:buffer-gap buffer)))
            "one undo reverts the change and the typed text")
    (%check (not (%undo-once! buffer cs)) "no second undo step exists for cw")))

(defun check-evil-insert-group-closes-for-other-buffer ()
  (let* ((buffer (%evil-buffer "abc"))
         (other (%evil-buffer "zzz" :name "other.ct"))
         (cs (cursor:cursor-new))
         (es (evil:evil-new coalton:True)))
    (%evil-feed es buffer cs (%evil-parse-keys "ix") nil)
    (%check (app::%coalton-optional-value-or-nil (evil:evil-insert-group-buffer es))
            "an insert session records its buffer")
    ;; The group belongs to BUFFER; closing with OTHER must not touch OTHER
    ;; but must clear the record.
    (evil:evil-close-insert-group! es (coalton:Some other) 0)
    (%check (null (app::%coalton-optional-value-or-nil (evil:evil-insert-group-buffer es)))
            "closing clears the record")
    ;; Closing with the right buffer ends the group so later edits are separate.
    (%evil-feed es buffer cs (%evil-parse-keys "<Esc>iy<Esc>") nil)
    (%check (string= "yxabc" (gap:gap-to-string (buf:buffer-gap buffer)))
            "edits after the orphaned group still apply, got ~S"
            (gap:gap-to-string (buf:buffer-gap buffer)))))

;;; Structural editing

(defun check-evil-structural-blocks-unbalanced-deletes ()
  (multiple-value-bind (out cur es buffer cs labels)
      (%evil-run (%lines "(defun foo" "  (bar))") "dd" :structural t)
    (declare (ignore cur es buffer cs))
    (%check (string= (%lines "(defun foo" "  (bar))") out) "dd on an unbalanced line is blocked")
    (%check (%blocked-p labels) "a blocked delete reports a message"))
  (%check-evil (%lines "(foo bar)" "baz") "dd" "baz" 0 :structural t)
  (%check-evil "foo bar)" "wdw" "foo bar)" 4 :structural t)
  (%check-evil "foo bar)" "dw" "bar)" 0 :structural t)
  (%check-evil "(a)" "x" "(a)" 0 :structural t)
  (%check-evil "()" "x" "" 0 :structural t)
  (%check-evil "(a)" "lx" "()" 1 :structural t)
  (%check-evil "(print \"a(b\")" "8ld$" "(print \"a(b\")" 8 :structural t)
  (%check-evil "(print \"a(b\")" "8ldl" "(print \"(b\")" 8 :structural t)
  (%check-evil "a" "r(" "a" 0 :structural t)
  (%check-evil "a" "rb" "b" 0 :structural t)
  (%check-evil "(a)" "r[" "(a)" 0 :structural t)
  (%check-evil (%lines "; c" "(a" " b)") "J" (%lines "; c" "(a" " b)") 0 :structural t)
  (%check-evil (%lines "(a" " b)") "J" "(a b)" 2 :structural t)
  (%check-evil (%lines "; c" "; d") "J" "; c ; d" 3 :structural t)
  (%check-evil (%lines "(a)" "; c (") "jdd" "(a)" 0 :structural t)
  (%check-evil (%lines "(a)" "; c (") "jD" (%lines "(a)" "") 4 :structural t)
  (%check-evil "(a (b) c)" "wwdw" "(a (b) c)" 4 :structural t)
  (%check-evil "(a b c)" "wdw" "(b c)" 1 :structural t)
  (%check-evil (%lines "(a" "b)") "Vjd" "" 0 :structural t)
  (%check-evil (%lines "(a" "b)") "Vd" (%lines "(a" "b)") 2 :structural t)
  (%check-evil "; c (" "x" "; c (" 0 :structural t)
  (%check-evil "; c (" "ra" "; c (" 0 :structural t)
  (%check-evil "#\\( x" "x" "#\\( x" 0 :structural t)
  (%check-evil "#\\( x" "lx" "#\\( x" 1 :structural t)
  (%check-evil "#\\( x" "3x" " x" 0 :structural t)
  (%check-evil (%lines "(a)" "(b)") "Vjd" "" 0 :structural t)
  (%check-evil "(a)" "p" "(a)" 0 :structural t))

;;; Properties

(defparameter *evil-property-iterations* 300)

(defun %random-elt (seq)
  (elt seq (random (length seq))))

(defun %random-keys (alphabet max-len)
  (let ((n (1+ (random max-len))))
    (with-output-to-string (s)
      (dotimes (i n)
        (write-string (%random-elt alphabet) s)))))

(defparameter *evil-normal-alphabet*
  '("h" "j" "k" "l" "w" "b" "e" "0" "^" "$" "gg" "G" "x" "X" "d" "D" "y" "Y" "J"
    "ra" "rb" "r(" "v" "V" "1" "2" "3" "<Esc>" "<CR>" "<BS>" "<Del>" "dd" "yy"))

(defparameter *evil-full-alphabet*
  (append *evil-normal-alphabet*
          '("i" "a" "I" "A" "c" "s" "S" "C" "p" "P" "u" "o" "O" "cc"
            "(" ")" "\"" "q" " " "<CR>" "<BS>" "<Esc>")))

(defun check-evil-property-no-crash ()
  "Random key sequences never signal and never leave the cursor out of range."
  (let ((*random-state* (sb-ext:seed-random-state 42))
        (text (%lines "(defun foo (x)" "  \"doc\"" "  (+ x 1))" "" "(bar [1 2])")))
    (dotimes (i *evil-property-iterations*)
      (let ((keys (%random-keys *evil-full-alphabet* 12)))
        (multiple-value-bind (out cur es) (%evil-run text keys)
          (%check (<= cur (length out))
                  "Keys ~S: cursor ~D is past the end of ~S" keys cur out)
          (%check (member (evil:evil-mode es)
                          (list evil:EvilNormal evil:EvilInsert evil:EvilVisual evil:EvilVisualLine))
                  "Keys ~S: invalid mode" keys))))))

(defun check-evil-property-structural-keeps-balance ()
  "With structural editing on, a balanced buffer stays balanced under any
normal-mode key sequence (puts are excluded: the register may hold any text)."
  (let ((*random-state* (sb-ext:seed-random-state 7))
        (text (%lines "(defun foo (x)" "  ;; comment (with parens" "  (let ((y \"a (string) here\"))"
                      "    (+ x y)))" "(bar [1 2] #\\( )")))
    (dotimes (i *evil-property-iterations*)
      (let* ((keys (%random-keys *evil-normal-alphabet* 10))
             (buffer (%evil-buffer text))
             (gb (buf:buffer-gap buffer))
             (cs (cursor:cursor-new))
             (es (evil:evil-new coalton:True)))
        (%check (paredit:range-balanced? gb 0 (gap:gap-length gb)) "the start text is balanced")
        (dolist (ev (%evil-parse-keys keys))
          (%evil-feed es buffer cs (list ev) t)
          (%check (paredit:range-balanced? gb 0 (gap:gap-length gb))
                  "Keys ~S left an unbalanced buffer: ~S" keys (gap:gap-to-string gb)))))))

(defparameter *evil-single-edits*
  '("x" "X" "lx" "3x" "dd" "dw" "de" "d$" "D" "dj" "J" "3J" "yyp" "yyP" "ywp" "d2w"
    "cwzz<Esc>" "ccq<Esc>" "C!<Esc>" "sZ<Esc>" "S<Esc>" "ra" "l2rz" "Vjd" "vlld" "vjy"
    "ihello<Esc>" "Aend<Esc>" "I--<Esc>" "lvlcX<Esc>" "wdw" "$x" "Gdd" "ggdG"))

(defun check-evil-property-single-undo-step ()
  "Every complete edit command is exactly one undo step: one undo restores
the original text and a second undo finds nothing."
  (let ((*random-state* (sb-ext:seed-random-state 99))
        (text (%lines "foo bar baz" "  (qux 1 2)" "end")))
    (dotimes (i *evil-property-iterations*)
      (let ((keys (%random-elt *evil-single-edits*)))
        (multiple-value-bind (out cur es buffer cs) (%evil-run text keys)
          (declare (ignore cur es))
          (let ((undone (%undo-once! buffer cs))
                (restored (gap:gap-to-string (buf:buffer-gap buffer))))
            (%check (string= text restored)
                    "Keys ~S: after one undo expected ~S, got ~S (edit gave ~S)"
                    keys text restored out)
            (when undone
              (%check (not (%undo-once! buffer cs))
                      "Keys ~S: a second undo step exists" keys))))))))
