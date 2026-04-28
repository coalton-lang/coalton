(in-package #:mine-tests)

(defun %write-utf8-file (pathname text)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string text stream)))

(defun %read-utf8-file (pathname)
  (with-open-file (stream pathname :direction :input :external-format :utf-8)
    (let ((text (make-string (file-length stream))))
      (read-sequence text stream)
      text)))

(defun %muffle-warning-if-possible (condition)
  (let ((restart (find-restart 'muffle-warning condition)))
    (when restart
      (invoke-restart restart))))

(defun %collect-compile-file-diagnostics (pathname)
  (let ((diagnostics nil)
        (group 0)
        (fasl nil))
    (labels ((record (condition &key muffle)
               (let ((items (diag:condition-diagnostics condition
                                                        :request 1
                                                        :group group)))
                 (when items
                   (incf group)
                   (setf diagnostics (nconc diagnostics items))))
               (when muffle
                 (%muffle-warning-if-possible condition))))
      (unwind-protect
           (handler-case
               (handler-bind ((sb-ext:compiler-note
                                (lambda (condition)
                                  (record condition)))
                              (sb-c:compiler-error
                                (lambda (condition)
                                  (record condition)))
                              (warning
                                (lambda (condition)
                                  (record condition :muffle t)))
                              (error
                                (lambda (condition)
                                  (record condition))))
                 (setf fasl (compile-file pathname)))
             (sb-c:compiler-error ()
               nil)
             (error ()
               nil))
        (when fasl
          (ignore-errors (delete-file fasl)))))
    diagnostics))

(defun %diagnostic-snippet (diagnostic text)
  (subseq text
          (getf diagnostic ':start)
          (getf diagnostic ':end)))

(defun %find-diagnostic (diagnostics predicate)
  (find-if predicate diagnostics))

(defun %collect-server-messages (thunk)
  (uiop:with-temporary-file (:pathname pathname :keep t)
    (unwind-protect
         (progn
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
             (funcall thunk stream))
           (with-open-file (stream pathname
                                   :direction :input
                                   :element-type '(unsigned-byte 8))
             (loop :for message = (server::read-message stream)
                   :while message
                   :collect message)))
      (ignore-errors (delete-file pathname)))))

(defun %read-server-messages-from-file (pathname)
  (with-open-file (stream pathname
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (loop :for message = (server::read-message stream)
          :while message
          :collect message)))

(defun %message-diagnostic-plist (message)
  (when (and (consp message)
             (eq (first message) ':notify)
             (consp (second message))
             (eq (first (second message)) ':diagnostic))
    (rest (second message))))

(defun %return-message-p (message)
  (and (consp message)
       (eq (first message) ':return)))

(defun %return-payload-text (message tag)
  (let ((payload (third message)))
    (when (and (consp payload)
               (eq (first payload) tag)
               (stringp (second payload)))
      (second payload))))

(defun %debug-notification-p (message)
  (and (consp message)
       (eq (first message) ':notify)
       (consp (second message))
       (eq (first (second message)) ':debug-activate)))

(defun %quick-result-plist (text)
  (let ((*read-eval* nil))
    (multiple-value-bind (payload pos)
        (read-from-string text)
      (%check (and (consp payload)
                   (eq (first payload) ':quick-result)
                   (= pos (length text)))
              "Expected Quick Result payload, got ~S"
              text)
      (rest payload))))

(defun %canonical-namestring (pathspec)
  (let* ((pathname (pathname pathspec))
         (probe (ignore-errors (probe-file pathname))))
    (namestring (or probe pathname))))

(defun %check (condition control &rest arguments)
  (unless condition
    (apply #'error control arguments)))

(defun check-diagnostics-locate-style-warning-spans ()
  (uiop:with-temporary-file (:pathname pathname :type "lisp" :keep t)
    (unwind-protect
         (let* ((text (format nil "(in-package #:cl-user)~%(defun style-warning-target () (unknown-fn 1))~%"))
                (unused (%write-utf8-file pathname text))
                (diagnostics (%collect-compile-file-diagnostics pathname))
                (diagnostic (%find-diagnostic
                             diagnostics
                             (lambda (item)
                               (eq (getf item ':severity) ':style-warning)))))
           (declare (ignore unused))
           (%check diagnostic "Expected a style-warning diagnostic. Diagnostics: ~S" diagnostics)
           (%check (string= (namestring pathname) (getf diagnostic ':file))
                   "Expected diagnostic file ~S, got ~S"
                   (namestring pathname)
                   (getf diagnostic ':file))
           (%check (search "unknown-fn"
                           (%diagnostic-snippet diagnostic text)
                           :test #'char-equal)
                   "Expected the style-warning span to cover UNKNOWN-FN: ~S"
                   diagnostic))
      (ignore-errors (delete-file pathname)))))

(defun check-diagnostics-use-character-offsets-for-unicode-files ()
  (uiop:with-temporary-file (:pathname pathname :type "lisp" :keep t)
    (unwind-protect
         (let* ((text (format nil ";; λ prefix~%(in-package #:cl-user)~%(defun unicode-target () (unknown-fn 1))~%"))
                (unused (%write-utf8-file pathname text))
                (diagnostics (%collect-compile-file-diagnostics pathname))
                (diagnostic (%find-diagnostic
                             diagnostics
                             (lambda (item)
                               (eq (getf item ':severity) ':style-warning)))))
           (declare (ignore unused))
           (%check diagnostic "Expected a style-warning diagnostic")
           (%check (search "unknown-fn"
                           (%diagnostic-snippet diagnostic text)
                           :test #'char-equal)
                   "Expected character offsets, not byte offsets: ~S"
                   diagnostic))
      (ignore-errors (delete-file pathname)))))

(defun check-coalton-source-conditions-expand-to-grouped-diagnostics ()
  (let* ((file (source:make-source-file #P"/tmp/source-warning.ct"))
         (primary (source:make-location file '(2 . 5)))
         (secondary (source:make-location file '(9 . 12)))
         (captured nil))
    (handler-bind ((source:source-warning
                     (lambda (condition)
                       (setf captured condition)
                       (%muffle-warning-if-possible condition))))
      (source:warn "warning summary"
                   (source:note primary "primary label")
                   (source:secondary-note secondary "secondary label")
                   (source:help secondary #'identity "help label")))
    (let ((diagnostics (diag:condition-diagnostics captured :request 5 :group 17)))
      (%check (= 3 (length diagnostics)) "Expected 3 diagnostics, got ~S" diagnostics)
      (%check (every (lambda (item) (eq (getf item ':severity) ':warning)) diagnostics)
              "Expected warning severities: ~S"
              diagnostics)
      (%check (equal '(:primary :secondary :help)
                     (mapcar (lambda (item) (getf item ':label-kind)) diagnostics))
              "Unexpected label kinds: ~S"
              diagnostics)
      (%check (every (lambda (item) (= 17 (getf item ':group))) diagnostics)
              "Expected shared group id 17: ~S"
              diagnostics)
      (%check (every (lambda (item)
                       (string= "/tmp/source-warning.ct" (getf item ':file)))
                     diagnostics)
              "Unexpected files in Coalton diagnostics: ~S"
              diagnostics))))

(defun check-wrapped-coalton-source-conditions-use-note-spans ()
  (let* ((file (source:make-source-file #P"/tmp/source-error.ct"))
         (primary (source:make-location file '(24 . 29)))
         (secondary (source:make-location file '(8 . 15)))
         (wrapped nil))
    (handler-case
        (source:error "type mismatch"
                      (source:note primary "expected String")
                      (source:secondary-note secondary "declared String"))
      (source:source-error (condition)
        (setf wrapped (make-condition 'sb-c:compiler-error :condition condition))))
    (let ((diagnostics (diag:condition-diagnostics wrapped :request 9 :group 23)))
      (%check (= 2 (length diagnostics))
              "Expected wrapped Coalton diagnostics to preserve note spans: ~S"
              diagnostics)
      (%check (equal '(24 8)
                     (mapcar (lambda (item) (getf item ':start)) diagnostics))
              "Expected wrapped Coalton starts, got ~S"
              diagnostics)
      (%check (equal '(29 15)
                     (mapcar (lambda (item) (getf item ':end)) diagnostics))
              "Expected wrapped Coalton ends, got ~S"
              diagnostics)
      (%check (equal '(:primary :secondary)
                     (mapcar (lambda (item) (getf item ':label-kind)) diagnostics))
              "Expected wrapped Coalton label kinds, got ~S"
              diagnostics)
      (%check (every (lambda (item)
                       (eq (getf item ':severity) ':error))
                     diagnostics)
              "Expected wrapped Coalton severities to stay :error: ~S"
              diagnostics))))

(defun check-generic-coalton-toplevel-notes-stay-textual ()
  (uiop:with-temporary-file (:pathname pathname :type "ct" :keep t)
    (unwind-protect
         (let* ((text (format nil "(in-package #:cl-user)~%~%(coalton-toplevel~%  (declare f (U64 -> U64))~%  (define (f x)~%    x))~%"))
                (unused (%write-utf8-file pathname text))
                (start (search "(coalton-toplevel" text))
                (demoted (diag::%low-confidence-coalton-compiler-span-p
                          (namestring pathname)
                          start
                          ':note))
                (not-demoted (diag::%low-confidence-coalton-compiler-span-p
                              (namestring pathname)
                              start
                              ':error)))
           (declare (ignore unused))
           (%check demoted
                   "Expected generic note span on a coalton-toplevel to be treated as low-confidence")
           (%check (not not-demoted)
                   "Did not expect error severity on a coalton-toplevel to be demoted"))
      (ignore-errors (delete-file pathname)))))

(defun check-source-diagnostic-hook-sees-source-error-subclasses ()
  (let* ((file (source:make-source-file #P"/tmp/source-hook.ct"))
         (primary (source:make-location file '(41 . 42)))
         (captured nil))
    (handler-case
        (let ((source:*source-diagnostic-hook*
                (lambda (condition)
                  (setf captured condition))))
          (coalton-impl/typechecker/base:tc-error
           "unknown instance"
           (source:note primary "Unknown instance Num String")))
      (source:source-error ()
        nil))
    (%check captured "Expected the source diagnostic hook to capture a source-error subclass")
    (%check (typep captured 'coalton-impl/typechecker/base:tc-error)
            "Expected the hook to receive the original tc-error, got ~S"
            captured)
    (let ((diagnostics (diag:condition-diagnostics captured :request 13 :group 6)))
      (%check (= 1 (length diagnostics))
              "Expected one diagnostic from the hooked source-error subclass, got ~S"
              diagnostics)
      (%check (= 41 (getf (first diagnostics) ':start))
              "Expected the hooked source-error subclass to preserve the note start: ~S"
              diagnostics)
      (%check (= 42 (getf (first diagnostics) ':end))
              "Expected the hooked source-error subclass to preserve the note end: ~S"
              diagnostics))))

(defun check-reader-errors-produce-point-diagnostics ()
  (uiop:with-temporary-file (:pathname pathname :type "lisp" :keep t)
    (unwind-protect
         (let* ((text (format nil "(defun broken-reader ()~%  (list 1 2~%"))
                (unused (%write-utf8-file pathname text))
                (diagnostics (%collect-compile-file-diagnostics pathname))
                (diagnostic (%find-diagnostic
                             diagnostics
                             (lambda (item)
                               (eq (getf item ':severity) ':error)))))
           (declare (ignore unused))
           (%check diagnostic "Expected a reader error diagnostic")
           (%check (string= (namestring pathname) (getf diagnostic ':file))
                   "Expected reader error file ~S, got ~S"
                   (namestring pathname)
                   (getf diagnostic ':file))
           (%check (= (getf diagnostic ':start) (getf diagnostic ':end))
                   "Expected point diagnostic, got ~S"
                   diagnostic)
           (%check (<= 0 (getf diagnostic ':start) (length text))
                   "Expected reader error start within file, got ~S"
                   diagnostic))
      (ignore-errors (delete-file pathname)))))

(defun check-quick-result-lisp-expression-shows-result ()
  (let* ((messages (%collect-server-messages
                    (lambda (stream)
                      (server::handle-quick-result
                       17 "(+ 1 2)" "CL-USER" stream))))
         (return-message (find-if #'%return-message-p messages))
         (text (%return-payload-text return-message ':ok))
         (payload (%quick-result-plist text)))
    (%check return-message "Expected a Quick Result return message: ~S" messages)
    (%check (string= (getf payload ':output) "")
            "Expected no printed output, got ~S from ~S"
            (getf payload ':output)
            messages)
    (%check (equal (getf payload ':values) '("3"))
            "Expected Quick Result value 3, got ~S from ~S"
            (getf payload ':values)
            messages)
    (%check (notany #'%debug-notification-p messages)
            "Quick Result must not activate the debugger: ~S"
            messages)))

(defun check-quick-result-lisp-format-separates-output-and-result ()
  (let* ((messages (%collect-server-messages
                    (lambda (stream)
                      (server::handle-quick-result
                       23 "(format t \"hello\")" "CL-USER" stream))))
         (return-message (find-if #'%return-message-p messages))
         (text (%return-payload-text return-message ':ok))
         (payload (%quick-result-plist text)))
    (%check return-message "Expected a Quick Result return message: ~S" messages)
    (%check (string= (getf payload ':output) "hello")
            "Expected captured output, got ~S from ~S"
            (getf payload ':output)
            messages)
    (%check (equal (getf payload ':values) '("NIL"))
            "Expected NIL result value, got ~S from ~S"
            (getf payload ':values)
            messages)
    (%check (notany #'%debug-notification-p messages)
            "Quick Result must not activate the debugger: ~S"
            messages)))

(defun check-quick-result-lisp-no-values-is-distinct ()
  (let* ((messages (%collect-server-messages
                    (lambda (stream)
                      (server::handle-quick-result
                       31 "(values)" "CL-USER" stream))))
         (return-message (find-if #'%return-message-p messages))
         (text (%return-payload-text return-message ':ok))
         (payload (%quick-result-plist text)))
    (%check return-message "Expected a Quick Result return message: ~S" messages)
    (%check (string= (getf payload ':output) "")
            "Expected no printed output, got ~S from ~S"
            (getf payload ':output)
            messages)
    (%check (equal (getf payload ':values) nil)
            "Expected no result values, got ~S from ~S"
            (getf payload ':values)
            messages)
    (%check (notany #'%debug-notification-p messages)
            "Quick Result must not activate the debugger: ~S"
            messages)))

(defun check-quick-result-lisp-error-is-short ()
  (let* ((messages (%collect-server-messages
                    (lambda (stream)
                      (server::handle-quick-result
                       29 "(error \"boom\")" "CL-USER" stream))))
         (return-message (find-if #'%return-message-p messages))
         (text (%return-payload-text return-message ':error)))
    (%check return-message "Expected a Quick Result error return message: ~S" messages)
    (%check (and text (search "boom" text :test #'char-equal))
            "Expected short error containing boom, got ~S from ~S"
            text
            messages)
    (%check (notany #'%debug-notification-p messages)
            "Quick Result must not activate the debugger: ~S"
            messages)))

(defun check-quick-result-interrupt-request-cancels-eval-thread ()
  (uiop:with-temporary-file (:pathname quick-path :keep t)
    (uiop:with-temporary-file (:pathname interrupt-path :keep t)
      (let ((quick-stream nil)
            (interrupt-stream nil)
            (worker nil))
        (unwind-protect
             (progn
               (setf quick-stream
                     (open quick-path
                           :direction :output
                           :if-exists :supersede
                           :element-type '(unsigned-byte 8)))
               (setf interrupt-stream
                     (open interrupt-path
                           :direction :output
                           :if-exists :supersede
                           :element-type '(unsigned-byte 8)))
               (setf worker
                     (sb-thread:make-thread
                      (lambda ()
                        (server::dispatch-message
                         '(:quick-result 900 "(loop)" "CL-USER")
                         quick-stream)
                        (force-output quick-stream))
                      :name "mine-test-quick-result-interrupt"))
               (loop :repeat 50
                     :until (server::%active-request-thread 900)
                     :do (sleep 0.02))
               (%check (server::%active-request-thread 900)
                       "Expected Quick Result request to be registered as active")
               (server::dispatch-message '(:interrupt-request 901 900)
                                         interrupt-stream)
               (force-output interrupt-stream)
               (let ((done nil))
                 (loop :repeat 50
                       :do (if (sb-thread:thread-alive-p worker)
                               (sleep 0.05)
                               (progn
                                 (setf done t)
                                 (return))))
                 (%check done
                         "Expected request-scoped interrupt to stop Quick Result evaluation"))
               (close quick-stream)
               (setf quick-stream nil)
               (close interrupt-stream)
               (setf interrupt-stream nil)
               (let ((interrupt-messages
                       (%read-server-messages-from-file interrupt-path))
                     (quick-messages
                       (%read-server-messages-from-file quick-path)))
                 (%check (member '(:return 901 (:ok t))
                                 interrupt-messages
                                 :test #'equal)
                         "Expected interrupt request to succeed, got ~S"
                         interrupt-messages)
                 (%check (member '(:return 900 (:error "Interrupted."))
                                 quick-messages
                                 :test #'equal)
                         "Expected Quick Result to return Interrupted, got ~S"
                         quick-messages)))
          (when (and worker (sb-thread:thread-alive-p worker))
            (sb-thread:terminate-thread worker))
          (when quick-stream
            (ignore-errors (close quick-stream)))
          (when interrupt-stream
            (ignore-errors (close interrupt-stream)))
          (ignore-errors (delete-file quick-path))
          (ignore-errors (delete-file interrupt-path)))))))

(defun %tuple-slot (tuple slot-name)
  (slot-value tuple (find-symbol slot-name "COALTON/CLASSES")))

(defun check-quick-result-selection-range ()
  (let ((cs (cursor:cursor-new)))
    (cursor:cursor-move-to-position! cs 3)
    (cursor:cursor-start-selection! cs)
    (cursor:cursor-move-to-position! cs 8)
    (let ((range (mine/app/mine::%active-selection-range cs)))
      (%check (and (not (eq range coalton:none))
                   (= (%tuple-slot range "_0") 3)
                   (= (%tuple-slot range "_1") 8))
              "Expected forward selection range 3..8, got ~S"
              range))
    (cursor:cursor-clear-selection! cs)
    (cursor:cursor-move-to-position! cs 8)
    (cursor:cursor-start-selection! cs)
    (cursor:cursor-move-to-position! cs 3)
    (let ((range (mine/app/mine::%active-selection-range cs)))
      (%check (and (not (eq range coalton:none))
                   (= (%tuple-slot range "_0") 3)
                   (= (%tuple-slot range "_1") 8))
              "Expected backward selection range 3..8, got ~S"
              range))
    (cursor:cursor-clear-selection! cs)
    (cursor:cursor-move-to-position! cs 3)
    (cursor:cursor-start-selection! cs)
    (%check (eq (mine/app/mine::%active-selection-range cs) coalton:none)
            "Expected zero-width selection to be ignored")))

(defun check-quick-result-target-uses-smallest-enclosing-form ()
  (let* ((text "(progn (+ 1 (* 2 3)) (list 4))")
         (gb (gap:gap-from-string text))
         (pos (search "* 2" text))
         (range (mine/app/mine::%find-quick-result-target gb pos)))
    (%check (and (not (eq range coalton:none))
                 (string= (gap:gap-substring gb
                                             (%tuple-slot range "_0")
                                             (%tuple-slot range "_1"))
                          "(* 2 3)"))
            "Expected Quick Result to target the smallest enclosing form, got ~S"
            range))
  (let* ((text "(+ 1 2)")
         (gb (gap:gap-from-string text))
         (range (mine/app/mine::%find-quick-result-target gb 0)))
    (%check (and (not (eq range coalton:none))
                 (= (%tuple-slot range "_0") 0)
                 (= (%tuple-slot range "_1") (length text)))
            "Expected Quick Result at an opening paren to target that form, got ~S"
            range)))

(defun check-quick-result-popup-ellipsizes-clipped-lines ()
  (let ((ellipsis (string (code-char 8230))))
    (%check (string= (mine/app/mine::%quick-result-ellipsize "abc" 3) "abc")
            "Expected fitted Quick Result text to stay unchanged")
    (%check (string= (mine/app/mine::%quick-result-ellipsize "abcdef" 4)
                     (concatenate 'string "abc" ellipsis))
            "Expected clipped Quick Result text to use a single ellipsis")
    (%check (string= (mine/app/mine::%quick-result-ellipsize "abcdef" 1)
                     ellipsis)
            "Expected one-cell Quick Result clipping to show only the ellipsis")))

(defun check-quick-result-popup-layout-prioritizes-results ()
  (let* ((layout (mine/app/mine::%quick-result-layout
                  (list :output (format nil "one~%two~%three")
                        :values '("VALUE"))
                  4)))
    (%check (string= (getf layout ':title) "Output")
            "Expected Output frame title, got ~S" layout)
    (%check (equal (getf layout ':output-lines)
                   (list "one" (format nil "~C 2 lines omitted" (code-char 8230))))
            "Expected output to truncate first, got ~S" layout)
    (%check (getf layout ':output-omitted)
            "Expected output omitted marker, got ~S" layout)
    (%check (getf layout ':separator)
            "Expected Result separator, got ~S" layout)
    (%check (equal (getf layout ':result-lines) '("VALUE"))
            "Expected result line to remain visible, got ~S" layout)
    (%check (not (getf layout ':result-omitted))
            "Expected no result omitted marker, got ~S" layout))
  (let ((layout (mine/app/mine::%quick-result-layout
                 '(:output "" :values ("VALUE"))
                 3)))
    (%check (string= (getf layout ':title) "Result")
            "Expected Result frame title without output, got ~S" layout)
    (%check (not (getf layout ':separator))
            "Expected no separator without output, got ~S" layout)
    (%check (equal (getf layout ':result-lines) '("VALUE"))
            "Expected value result line, got ~S" layout))
  (let ((layout (mine/app/mine::%quick-result-layout
                 '(:output "" :values nil)
                 3)))
    (%check (getf layout ':no-values)
            "Expected no-values marker, got ~S" layout)
    (%check (equal (getf layout ':result-lines) '("No values"))
            "Expected No values display line, got ~S" layout))
  (let ((layout (mine/app/mine::%quick-result-layout
                 '(:output "" :values ("first" "second" "third"))
                 2)))
    (%check (equal (getf layout ':result-lines)
                   (list "first" (format nil "~C 2 lines omitted" (code-char 8230))))
            "Expected result overflow to use an omitted-line marker, got ~S" layout)
    (%check (getf layout ':result-omitted)
            "Expected result omitted marker, got ~S" layout))
  (let ((layout (mine/app/mine::%quick-result-layout
                 '(:title "Quick Result" :pending :busy)
                 3
                 "*")))
    (%check (string= (getf layout ':title) "Quick Result")
            "Expected Quick Result pending title, got ~S" layout)
    (%check (getf layout ':pending)
            "Expected pending Quick Result layout, got ~S" layout)
    (%check (equal (getf layout ':result-lines)
                   '("Busy *" "Esc/Ctrl+g cancels"))
            "Expected pending Quick Result busy rows, got ~S" layout))
  (let ((layout (mine/app/mine::%quick-result-layout
                 '(:title "Quick Result" :pending :interrupting)
                 3
                 "*")))
    (%check (equal (getf layout ':result-lines)
                   '("Interrupting *" "Waiting for runtime"))
            "Expected pending Quick Result interrupt rows, got ~S" layout)))

(defun check-quick-result-popup-uses-terminal-height ()
  (%check (> (mine/app/mine::%quick-result-max-body-lines 30) 8)
          "Expected Quick Result popup to use more than the old fixed row cap")
  (%check (= (mine/app/mine::%quick-result-max-body-lines 3) 1)
          "Expected very short terminals to keep at least one body row"))

(defun check-coalton-none-is-not-current-buffer-at-cl-boundary ()
  (let* ((bm (mine/buffer/manager:bufmgr-new))
         (current (mine/buffer/manager:bufmgr-current bm)))
    (%check (eq current coalton:none)
            "Expected empty buffer manager to return Coalton None, got ~S"
            current)
    (%check (null (mine/app/mine::%coalton-optional-value-or-nil current))
            "Expected Coalton None to unwrap to CL NIL at app boundary")
    (%check (null (mine/app/diagnostics::coalton-optional-value-or-nil current))
            "Expected Coalton None to unwrap to CL NIL at diagnostics boundary")))

(defun check-beam-system-emits-diagnostics-before-return ()
  (let* ((system-name (format nil "mine-beam-test-~A"
                              (string-downcase (symbol-name (gensym)))))
         (dir (merge-pathnames (format nil "~A/" system-name)
                               (uiop:temporary-directory)))
         (source-path (merge-pathnames "main.lisp" dir))
         (asd-path (merge-pathnames (format nil "~A.asd" system-name) dir)))
    (unwind-protect
         (progn
           (ensure-directories-exist source-path)
           (%write-utf8-file
            source-path
            (format nil "(in-package #:cl-user)~%(defun beam-system-target () (unknown-fn 1))~%"))
           (%write-utf8-file
            asd-path
            (format nil "(asdf:defsystem ~S :serial t :components ((:file ~S)))~%"
                    system-name
                    "main"))
           (let* ((messages (%collect-server-messages
                             (lambda (stream)
                               (server::handle-beam-system 7 system-name (namestring asd-path) stream))))
                  (return-message (find-if (lambda (message)
                                             (and (consp message)
                                                 (eq (first message) ':return)))
                                           messages))
                  (diagnostic-message (find-if (lambda (message)
                                                 (let ((plist (%message-diagnostic-plist message)))
                                                   (and plist
                                                        (eq (getf plist ':severity) ':style-warning))))
                                               messages))
                  (diagnostic (and diagnostic-message
                                   (%message-diagnostic-plist diagnostic-message))))
             (%check diagnostic "Expected a Beam System diagnostic notification: ~S" messages)
             (%check return-message "Expected a Beam System return message: ~S" messages)
             (%check (eq ':ok (first (third return-message)))
                     "Expected Beam System success return: ~S"
                     return-message)
             (%check (string= (%canonical-namestring source-path)
                              (%canonical-namestring (getf diagnostic ':file)))
                     "Expected diagnostic file ~S, got ~S"
                     (%canonical-namestring source-path)
                     (getf diagnostic ':file))
             (%check (< (position diagnostic-message messages :test #'equal)
                        (position return-message messages :test #'equal))
                     "Expected diagnostic before return: ~S"
                     messages)))
      (ignore-errors (asdf:clear-system system-name))
      (ignore-errors (uiop:delete-directory-tree dir :validate t)))))

(defun check-beam-system-preserves-coalton-error-spans ()
  (let* ((system-name (format nil "mine-beam-coalton-~A"
                              (string-downcase (symbol-name (gensym)))))
         (package-name (string-upcase system-name))
         (dir (merge-pathnames (format nil "~A/" system-name)
                               (uiop:temporary-directory)))
         (source-path (merge-pathnames "main.ct" dir))
         (asd-path (merge-pathnames (format nil "~A.asd" system-name) dir)))
    (unwind-protect
         (progn
           (ensure-directories-exist source-path)
           (%write-utf8-file
            source-path
            (format nil "(defpackage #:~A~%  (:use #:coalton #:coalton-prelude))~%(in-package #:~A)~%~%(coalton-toplevel~%  (declare f (String -> String))~%  (define (f x)~%    (+ 1 x)))~%"
                    package-name
                    package-name))
           (%write-utf8-file
            asd-path
            (format nil "(asdf:defsystem ~S~%  :depends-on (~S)~%  :defsystem-depends-on (~S)~%  :components ((:ct-file ~S)))~%"
                    system-name
                    "coalton"
                    "coalton-asdf"
                    "main"))
           (let* ((messages (%collect-server-messages
                             (lambda (stream)
                               (server::handle-beam-system 11 system-name (namestring asd-path) stream))))
                  (return-message (find-if (lambda (message)
                                             (and (consp message)
                                                  (eq (first message) ':return)))
                                           messages))
                  (diagnostic-message (find-if (lambda (message)
                                                 (let ((plist (%message-diagnostic-plist message)))
                                                   (and plist
                                                        (eq (getf plist ':severity) ':error))))
                                               messages))
                  (diagnostic (and diagnostic-message
                                   (%message-diagnostic-plist diagnostic-message))))
             (%check diagnostic "Expected a Beam System Coalton diagnostic notification: ~S" messages)
             (%check return-message "Expected a Beam System return message: ~S" messages)
             (%check (eq ':error (first (third return-message)))
                     "Expected Beam System error return: ~S"
                     return-message)
             (%check (string= (%canonical-namestring source-path)
                              (%canonical-namestring (getf diagnostic ':file)))
                     "Expected Coalton diagnostic file ~S, got ~S"
                     (%canonical-namestring source-path)
                     (getf diagnostic ':file))
             (%check (< (getf diagnostic ':start) (getf diagnostic ':end))
                     "Expected a non-empty Coalton diagnostic span, got ~S"
                     diagnostic)
             (%check (string= (getf diagnostic ':summary)
                              (second (third return-message)))
                     "Expected return text to mirror the diagnostic summary: ~S / ~S"
                     diagnostic
                     return-message)))
      (ignore-errors (asdf:clear-system system-name))
      (ignore-errors (uiop:delete-directory-tree dir :validate t)))))
