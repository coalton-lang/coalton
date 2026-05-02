(in-package #:mine-tests)

(defun %optional-ufix (value)
  (if value
      (coalton:Some value)
      coalton:None))

(defun %coalton-boolean (value)
  (if value coalton:True coalton:False))

(defun %indent-rule (head body-start lambda-index
                     &key if local-bindings defmethod)
  (indent:IndentRuleEntry
   head
   (indent:RuleKnown (%optional-ufix body-start)
                     (%optional-ufix lambda-index)
                     (%coalton-boolean if)
                     (%coalton-boolean local-bindings)
                     (%coalton-boolean defmethod))))

(defun %default-indent-rules ()
  (list (%indent-rule "cl:multiple-value-bind" 3 1)
        (%indent-rule "multiple-value-bind" 3 1)
        (%indent-rule "defun" 3 2)
        (%indent-rule "flet" 2 nil :local-bindings t)
        (%indent-rule "when" 2 nil)
        (%indent-rule "hunchentoot:define-easy-handler" 3 nil)))

(defun %indent-of-line (text line &optional (mode 1) (rules (%default-indent-rules)))
  (indent:compute-indent-with-rules (gap:gap-from-string text) mode line rules))

(defun %column-of (text needle &optional (start 0))
  (let ((pos (search needle text :start2 start)))
    (%check pos "Expected to find ~S in ~S" needle text)
    (let ((line-start (or (position #\Newline text :end pos :from-end t)
                          -1)))
      (- pos line-start 1))))

(defun %check-indent (text line expected &optional (description "indent"))
  (let ((actual (%indent-of-line text line)))
    (%check (= expected actual)
            "Expected ~A for line ~D to be ~D, got ~D in:~%~A"
            description line expected actual text)))

(defun check-indent-hunchentoot-style-handler-body ()
  (let ((text "(hunchentoot:define-easy-handler (say-yo :uri \"/yo\") (name)
(setf (hunchentoot:content-type*) \"text/plain\")
(format nil \"Yo, ~A\" name))"))
    (%check-indent text 1 2 "handler body indentation")
    (%check-indent text 2 2 "handler body continuation indentation")))

(defun check-indent-multiple-value-bind-special-form ()
  (let ((text "(multiple-value-bind (value presentp)
    (lookup key table)
  (when presentp
(format t \"~A\" value)))"))
    (%check-indent text 1 4 "multiple-value-bind value form indentation")
    (%check-indent text 2 2 "multiple-value-bind body indentation")
    (%check-indent text 3 4 "nested body indentation")))

(defun check-indent-lambda-list-keyword-alignment ()
  (let ((text "(defun collect-values (first second &rest rest
&key limit transform)
(declare (ignore first second rest limit transform)))"))
    (%check-indent text 1 (%column-of text "&rest")
                   "lambda-list keyword alignment")
    (%check-indent text 2 2 "defun body indentation")))

(defun check-indent-lambda-list-keyword-parameter-alignment ()
  (let ((text "(defun configure (stream &key width height
title foreground)
(list stream width height title foreground))"))
    (%check-indent text 1 (%column-of text "width")
                   "lambda-list keyword parameter alignment")
    (%check-indent text 2 2 "defun body indentation")))

(defun check-indent-keyword-call-alignment ()
  (let ((text "(make-instance 'pane :width 80
:height 24
:title \"Mine\")"))
    (%check-indent text 1 (%column-of text ":width")
                   "keyword argument alignment")
    (%check-indent text 2 (%column-of text ":width")
                   "later keyword argument alignment")))

(defun check-indent-flet-local-function-body ()
  (let ((text "(flet ((render-item (item &key width
height)
(draw item width height)))
  (render-item thing))"))
    (%check-indent text 1 (%column-of text "width")
                   "local function lambda-list keyword parameter alignment")
    (%check-indent text 2 9 "local function body indentation")
    (%check-indent text 3 2 "flet body indentation")))

(defun check-indent-cl-prefixed-multiple-value-bind ()
  (let ((text "(cl:multiple-value-bind (value presentp)
    (lookup key table)
  (when presentp
(format t \"~A\" value)))"))
    (%check-indent text 1 4 "cl-prefixed multiple-value-bind value form")
    (%check-indent text 2 2 "cl-prefixed multiple-value-bind body")
    (%check-indent text 3 4 "cl-prefixed multiple-value-bind nested body")))

(defun check-indent-non-cl-prefixed-multiple-value-bind-is-generic ()
  (let ((text "(weird-package:multiple-value-bind (value presentp)
(lookup key table)
body)"))
    (%check-indent text 1 (%column-of text "(value")
                   "non-cl-prefixed multiple-value-bind first continuation")
    (%check-indent text 2 (%column-of text "(value")
                   "non-cl-prefixed multiple-value-bind later continuation")))

(defun check-indent-plain-text-mode-never-indents ()
  (let ((text "(not-lisp
should-stay-flush-left)"))
    (%check (= 0 (%indent-of-line text 1 2))
            "Expected plain text mode to return zero indentation")))

(defun check-indent-runtime-rules-resolve-shadowed-cl-symbols ()
  (let* ((pkg-name (format nil "MINE-INDENT-TEST-~A" (string-upcase (symbol-name (gensym)))))
         (pkg (make-package pkg-name :use nil)))
    (unwind-protect
         (progn
           (intern "MULTIPLE-VALUE-BIND" pkg)
           (let ((rules (server::%indent-rule-specs-for-heads
                         '("multiple-value-bind" "cl:multiple-value-bind")
                         pkg-name)))
             (%check (equal (first rules)
                            '("multiple-value-bind" nil nil nil nil nil))
                     "Expected shadowed bare multiple-value-bind to be generic, got ~S"
                     (first rules))
             (%check (equal (second rules)
                            '("cl:multiple-value-bind" 3 1 nil nil nil))
                     "Expected cl:multiple-value-bind to resolve to CL rule, got ~S"
                     (second rules)))
           (let ((lowercase-package-rules
                   (server::%indent-rule-specs-for-heads
                    '("multiple-value-bind")
                    "cl-user")))
             (%check (equal (first lowercase-package-rules)
                            '("multiple-value-bind" 3 1 nil nil nil))
                     "Expected lowercase package names to resolve through the runtime image, got ~S"
                     (first lowercase-package-rules)))
           (let ((unknown-package-rules
                   (server::%indent-rule-specs-for-heads
                    '("multiple-value-bind")
                    "MINE-INDENT-TEST-NO-SUCH-PACKAGE")))
             (%check (equal (first unknown-package-rules)
                            '("multiple-value-bind" nil nil nil nil nil))
                     "Expected an unknown package to resolve no indentation rule, got ~S"
                     (first unknown-package-rules))))
      (delete-package pkg))))

(defun check-indent-runtime-rules-use-cl-user-for-lisp-default ()
  (let ((rules (server::%indent-rule-specs-for-heads
                '("defun" "define")
                "CL-USER")))
    (%check (equal (first rules)
                   '("defun" 3 2 nil nil nil))
            "Expected Lisp buffers without in-package to resolve defun in CL-USER, got ~S"
            (first rules))
    (%check (equal (second rules)
                   '("define" nil nil nil nil nil))
            "Expected CL-USER define to remain generic, got ~S"
            (second rules))))

(defun check-indent-newline-before-close-paren-uses-blank-context ()
  (let* ((text (format nil "(f a b~%)"))
         (gb (gap:gap-from-string text))
         (pos (1+ (position #\Newline text))))
    (%check (= 0 (indent:compute-indent-with-rules
                  gb 1 1 (list (%indent-rule "f" 3 nil))))
            "Expected ordinary reindent of a close-paren line to stay at column zero")
    (%check (= 2 (indent:compute-blank-indent-at-position-with-rules
                  gb 1 pos (list (%indent-rule "f" 3 nil))))
            "Expected Enter before close paren to use body indentation")
    (%check (= 3 (indent:compute-blank-indent-at-position-with-rules
                  gb 1 pos nil))
            "Expected Enter before close paren to use generic continuation indentation without a runtime rule")))

(defun %indent-buffer-with-text (text)
  (let ((buffer (buf:buffer-new-file (buf:BufferId 0) "indent-test.lisp")))
    (gap:gap-insert-string! (buf:buffer-gap buffer) 0 text)
    buffer))

(defun %apply-line-indent (text cursor-pos line target-indent)
  (let* ((buffer (%indent-buffer-with-text text))
         (cs (cursor:cursor-new)))
    (cursor:cursor-move-to-position! cs cursor-pos)
    (app::%set-line-indentation! buffer (buf:buffer-undo buffer) cs line target-indent)
    (values (gap:gap-to-string (buf:buffer-gap buffer))
            (cursor:cursor-position cs))))

(defun check-indent-line-tab-hop-to-source ()
  (multiple-value-bind (text pos)
      (%apply-line-indent (format nil "(foo~%  bar)") 5 1 2)
    (%check (string= text (format nil "(foo~%  bar)"))
            "Expected already-correct indentation to leave text unchanged, got ~S"
            text)
    (%check (= pos 7)
            "Expected Tab at line start to hop to source column, got cursor position ~D"
            pos)))

(defun check-indent-line-preserves-source-position ()
  (multiple-value-bind (text pos)
      (%apply-line-indent (format nil "(foo~%bar)") 6 1 2)
    (%check (string= text (format nil "(foo~%  bar)"))
            "Expected indentation to insert two spaces, got ~S"
            text)
    (%check (= pos 8)
            "Expected reindentation to preserve cursor's source-relative position, got ~D"
            pos)))

(defun check-editor-paste-clamps-stale-cursor-to-buffer-end ()
  (let* ((buffer (buf:buffer-new-file (buf:BufferId 0) "paste-test.ct"))
         (cs (cursor:cursor-new))
         (gb (buf:buffer-gap buffer)))
    (gap:gap-insert-string! (buf:buffer-gap buffer) 0 "abc")
    (cursor:cursor-move-to-position! cs 250)
    (ops:insert-string! buffer (buf:buffer-undo buffer) cs "XYZ")
    (%check (string= "abcXYZ" (gap:gap-to-string gb))
            "Expected paste with a stale cursor to append at EOF, got ~S"
            (gap:gap-to-string gb))
    (%check (= 6 (cursor:cursor-position cs))
            "Expected cursor after clamped paste to be 6, got ~D"
            (cursor:cursor-position cs))
    (let ((entry (app::%coalton-optional-value-or-nil
                  (undo:undo-undo! (buf:buffer-undo buffer)))))
      (%check entry "Expected paste to record an undo entry")
      (app::apply-undo-ops
       gb
       (funcall (find-symbol "UNDOENTRY/UNDOENTRY-_0" "MINE/EDIT/UNDO")
                entry))
      (cursor:cursor-move-to-position!
       cs
       (funcall (find-symbol "UNDOENTRY/UNDOENTRY-_1" "MINE/EDIT/UNDO")
                entry))
      (%check (string= "abc" (gap:gap-to-string gb))
              "Expected undo after clamped paste to restore text, got ~S"
              (gap:gap-to-string gb))
      (%check (= 3 (cursor:cursor-position cs))
              "Expected undo after clamped paste to restore cursor to EOF, got ~D"
              (cursor:cursor-position cs)))))
