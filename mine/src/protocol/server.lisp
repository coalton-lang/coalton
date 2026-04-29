;;;; server.lisp -- Protocol server (CL side).
;;;;
;;;; Handles incoming connections from the TUI client and dispatches
;;;; requests to the runtime modules.  Uses the wire format:
;;;;   6-byte uppercase hex length header + UTF-8 S-expression payload.

(in-package #:mine/protocol/server)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

;;; Uninteresting warnings
;;;
;;; SBCL signals style-warnings (REDEFINITION-WITH-DEFUN, etc.) that are
;;; noisy but harmless during normal ASDF compile-file + load cycles.
;;; SLIME/SWANK silences these globally via swank-asdf; ASDF's own
;;; compile-file* / load* muffle them when *uninteresting-conditions* is
;;; set.  We muffle the same set at the compiler level (declaim) and
;;; skip them in our handler-bind so they never reach the REPL.

(declaim (sb-ext:muffle-conditions sb-kernel:redefinition-with-defun
                                   sb-kernel:redefinition-with-defgeneric
                                   sb-kernel:redefinition-with-defmethod
                                   sb-kernel::redefinition-with-defmacro
                                   sb-kernel:uninteresting-redefinition
                                   sb-ext:implicit-generic-function-warning
                                   sb-int:package-at-variance))

(defun %uninteresting-warning-p (w)
  "Return T if W is a warning that ASDF/SWANK would normally muffle."
  (typep w '(or sb-kernel:redefinition-warning
                sb-ext:implicit-generic-function-warning
                sb-int:package-at-variance)))

;;; Wire format

(defun read-message (stream)
  "Read one message from STREAM using the 6-byte hex header protocol.
Returns the parsed S-expression, or NIL on EOF/error."
  (handler-case
      (let ((header (make-array 6 :element-type '(unsigned-byte 8))))
        ;; Read the 6-byte length header
        (let ((n (read-sequence header stream)))
          (when (< n 6)
            (return-from read-message nil)))
        (let* ((header-string (sb-ext:octets-to-string
                               header :external-format ':utf-8))
               (length (parse-integer header-string :radix 16
                                                    :junk-allowed t)))
          (when (or (null length) (<= length 0))
            (return-from read-message nil))
          ;; Read the payload
          (let ((payload (make-array length
                                     :element-type '(unsigned-byte 8))))
            (let ((n (read-sequence payload stream)))
              (when (< n length)
                (return-from read-message nil)))
            (let ((text (sb-ext:octets-to-string
                         payload :external-format ':utf-8)))
              ;; Parse the S-expression
              (handler-case
                  (read-from-string text)
                (error (c)
                  (declare (ignore c))
                  nil))))))
    (end-of-file () nil)
    (error () nil)))

(defun write-message (stream sexp)
  "Write SEXP to STREAM using the 6-byte hex header protocol."
  (handler-case
      (let* ((text (let ((*print-case* ':downcase))
                     (prin1-to-string sexp)))
             (octets (sb-ext:string-to-octets text :external-format ':utf-8))
             (length (length octets))
             (header (sb-ext:string-to-octets
                      (format nil "~6,'0X" length)
                      :external-format ':utf-8)))
        (write-sequence header stream)
        (write-sequence octets stream)
        (force-output stream)
        t)
    (error () nil)))

(defun %reject-unexpected-message-during-wait (stream msg context)
  "Return an error for MSG when a nested protocol wait cannot handle it."
  (let ((id (and (consp msg) (integerp (second msg)) (second msg))))
    (when id
      (write-message stream
                     `(:return ,id
                       (:error ,(format nil "Runtime is waiting for ~A." context)))))))

(defvar *active-request-threads* (make-hash-table :test #'eql)
  "Map request IDs to the runtime thread currently evaluating them.")

(defvar *active-request-threads-lock*
  (mine/bindings/thread:make-mutex "mine-active-requests"))

(defun %remember-active-request (id)
  "Remember that ID is running on the current thread."
  (mine/bindings/thread:with-mutex (*active-request-threads-lock*)
    (setf (gethash id *active-request-threads*) sb-thread:*current-thread*)))

(defun %forget-active-request (id)
  "Forget the runtime thread for ID."
  (mine/bindings/thread:with-mutex (*active-request-threads-lock*)
    (remhash id *active-request-threads*)))

(defmacro %with-active-request ((id) &body body)
  "Run BODY while ID can be interrupted by :interrupt-request."
  `(unwind-protect
        (progn
          (%remember-active-request ,id)
          ,@body)
     (%forget-active-request ,id)))

(defun %active-request-thread (id)
  "Return the thread handling ID, or NIL."
  (mine/bindings/thread:with-mutex (*active-request-threads-lock*)
    (gethash id *active-request-threads*)))

(defun %interrupt-active-request (id)
  "Interrupt the thread handling ID without signaling the whole runtime process."
  (let ((thread (%active-request-thread id)))
    (when (and thread
               (not (eq thread sb-thread:*current-thread*))
               (sb-thread:thread-alive-p thread))
      (sb-thread:interrupt-thread
       thread
       (lambda ()
         (error 'sb-sys:interactive-interrupt)))
      t)))

;;; TUI input stream (Gray stream for interactive IO)

(defclass tui-input-stream (sb-gray:fundamental-character-input-stream)
  ((wire-stream    :initarg :wire-stream    :accessor tis-wire-stream)
   (msg-id         :initarg :msg-id         :accessor tis-msg-id)
   (stdout-capture :initarg :stdout-capture :accessor tis-stdout-capture)
   (buffer         :initform ""             :accessor tis-buffer)
   (buffer-pos     :initform 0             :accessor tis-buffer-pos))
  (:documentation
   "A Gray stream that routes reads through the wire protocol to the TUI.
When CL code calls READ-LINE or READ on this stream, it sends an :io-request
message to the TUI and blocks until the user provides input."))

(defun %request-input-from-tui (tis prompt)
  "Send :io-request to TUI and block until :io-response or :io-abort.
Flushes any pending captured output first so the user sees it.
Returns the input text string, or NIL for EOF/abort."
  (let ((wire (tis-wire-stream tis))
        (id   (tis-msg-id tis))
        (capture (tis-stdout-capture tis)))
    ;; Flush pending output (e.g., y-or-n-p prompt text) before requesting input
    (when capture
      (let ((text (get-output-stream-string capture)))
        (when (plusp (length text))
          (dolist (line (split-string-by-newline text))
            (when (plusp (length line))
              (write-message wire `(:notify (:output ,line))))))))
    (write-message wire `(:io-request ,id ,(or prompt "")))
    (loop
      (let ((msg (read-message wire)))
        (cond
          ((null msg) (return nil))
          ((and (consp msg) (eq (first msg) :io-response))
           (return (third msg)))
          ((and (consp msg) (eq (first msg) :io-abort))
           (return nil))
          (t
           (%reject-unexpected-message-during-wait wire msg "input")))))))

(defmethod sb-gray:stream-read-line ((stream tui-input-stream))
  "Read a line from the TUI. Returns (values string eof-p)."
  (let ((text (%request-input-from-tui stream "")))
    (if text
        (values text nil)
        (values "" t))))

(defmethod sb-gray:stream-read-char ((stream tui-input-stream))
  "Read a single character, requesting a new line from TUI when buffer is exhausted.
Non-empty responses are buffered as-is (no trailing newline) so that
single-char readers like y-or-n-p don't see a phantom newline.
Empty responses (user pressed Enter with no text) return #\\Newline,
which serves as a token delimiter for READ."
  (let ((buf (tis-buffer stream))
        (pos (tis-buffer-pos stream)))
    (if (< pos (length buf))
        ;; Dispense from buffer
        (prog1 (char buf pos)
          (setf (tis-buffer-pos stream) (1+ pos)))
        ;; Buffer exhausted -- request a new line
        (let ((text (%request-input-from-tui stream "")))
          (cond
            ((null text) :eof)
            ((zerop (length text))
             ;; Empty response = Enter key = newline delimiter
             (setf (tis-buffer stream) "")
             (setf (tis-buffer-pos stream) 0)
             #\Newline)
            (t
             (setf (tis-buffer stream) text)
             (setf (tis-buffer-pos stream) 1)
             (char text 0)))))))

(defmethod sb-gray:stream-unread-char ((stream tui-input-stream) char)
  "Push back a character by decrementing the buffer position."
  (declare (ignore char))
  (when (> (tis-buffer-pos stream) 0)
    (decf (tis-buffer-pos stream)))
  nil)

(defmethod sb-gray:stream-listen ((stream tui-input-stream))
  "Return T if there are buffered characters available."
  (< (tis-buffer-pos stream) (length (tis-buffer stream))))

(defmethod sb-gray:stream-clear-input ((stream tui-input-stream))
  "Discard any buffered input."
  (setf (tis-buffer stream) "")
  (setf (tis-buffer-pos stream) 0)
  nil)

;;; Message dispatch

(defun dispatch-message (msg stream)
  "Dispatch a protocol message MSG and send the response on STREAM.
Returns :quit if the server should shut down, T otherwise."
  (unless (consp msg)
    (return-from dispatch-message t))
  (let ((tag (first msg)))
    (case tag
      (:ping
       (let ((id (second msg)))
         (write-message stream `(:return ,id (:ok :pong)))))

      (:heap-info
       (let* ((id (second msg))
              (used (sb-kernel:dynamic-usage))
              (total (sb-ext:dynamic-space-size)))
         (write-message stream `(:return ,id (:ok ,(cons used total))))))

      (:quit
       (let ((id (second msg)))
         (write-message stream `(:return ,id (:ok :goodbye)))
         (return-from dispatch-message ':quit)))

      (:eval
       (destructuring-bind (id form-string package-name) (rest msg)
         (handle-eval id form-string package-name stream)))

      (:quick-result
       (destructuring-bind (id form-string package-name) (rest msg)
         (%with-active-request (id)
           (handle-quick-result id form-string package-name stream))))

      (:interrupt-request
       (destructuring-bind (id target-id) (rest msg)
         (if (%interrupt-active-request target-id)
             (write-message stream `(:return ,id (:ok t)))
             (write-message stream `(:return ,id (:error "No active request"))))))

      (:compile-string
       (destructuring-bind (id string document-key package-name position client-prefix-length)
           (rest msg)
         (handle-compile-string id string document-key package-name position
                                client-prefix-length stream)))

      (:compile-file
       (destructuring-bind (id filename load-p) (rest msg)
         (handle-compile-file id filename load-p stream)))

      (:beam-system
       (destructuring-bind (id system-name asd-path) (rest msg)
         (handle-beam-system id system-name asd-path stream)))

      (:type-of
       (destructuring-bind (id symbol-name package-name) (rest msg)
         (handle-type-of id symbol-name package-name stream)))

      (:find-definition
       (destructuring-bind (id symbol-name package-name) (rest msg)
         (handle-find-definition id symbol-name package-name stream)))

      (:complete
       (destructuring-bind (id prefix package-name) (rest msg)
         (handle-complete id prefix package-name stream)))

      (:arglist
       (destructuring-bind (id function-name package-name) (rest msg)
         (handle-arglist id function-name package-name stream)))

      (:indent-rules
       (destructuring-bind (id heads package-name) (rest msg)
         (handle-indent-rules id heads package-name stream)))

      (:coalton-type-of
       (destructuring-bind (id symbol-name package-name) (rest msg)
         (handle-coalton-type-of id symbol-name package-name stream)))

      (otherwise
       (let ((id (if (cdr msg) (second msg) 0)))
         (write-message stream
                        `(:return ,id
                          (:error ,(format nil "Unknown message tag: ~S"
                                          tag))))))))
  t)

;;; Handler functions

(defun %find-symbol-flexibly (symbol-name package-name)
  "Find symbol, trying package-name as direct package or as a local nickname.
Returns the symbol or NIL."
  (let* ((upcase-pkg (string-upcase package-name))
         (upcase-sym (string-upcase symbol-name))
         (pkg (find-package upcase-pkg)))
    ;; Direct lookup
    (when pkg
      (let ((sym (find-symbol upcase-sym pkg)))
        (when sym (return-from %find-symbol-flexibly sym))))
    ;; Try as a local nickname across loaded packages
    (dolist (p (list-all-packages))
      (let ((nicknames (ignore-errors (sb-ext:package-local-nicknames p))))
        (dolist (entry nicknames)
          (when (string-equal upcase-pkg (car entry))
            (let* ((real-pkg (cdr entry))
                   (sym (find-symbol upcase-sym real-pkg)))
              (when sym
                (return-from %find-symbol-flexibly sym)))))))
    nil))

(defun %find-indent-package (package-name)
  "Find PACKAGE-NAME for indentation resolution.
Try the exact runtime package name first, then the normal uppercase spelling.
Never fall back to *PACKAGE*; an unknown package means unknown indentation."
  (and (stringp package-name)
       (or (find-package package-name)
           (find-package (string-upcase package-name)))))

(defun %read-symbol-token-in-package (token package-name)
  "Read TOKEN as a symbol in PACKAGE-NAME.
The CL reader is the authority here: it handles imports, shadows, package
prefixes, and SBCL package-local nicknames relative to *PACKAGE*."
  (handler-case
      (let ((pkg (%find-indent-package package-name)))
        (when pkg
          (let ((*package* pkg)
                (*read-eval* nil))
            (multiple-value-bind (object position)
                (read-from-string token nil nil)
              (and (symbolp object)
                   (= position (length token))
                   object)))))
    (error () nil)))

(defun %symbol-named-p (sym package-name symbol-name)
  (let ((pkg (find-package package-name)))
    (and pkg
         (multiple-value-bind (found status)
             (find-symbol symbol-name pkg)
           (and status (eq sym found))))))

(defun %symbol-name-member-p (sym package-name symbol-names)
  (let ((target-package (find-package package-name))
        (symbol-package (symbol-package sym)))
    (and target-package
         symbol-package
         (eq symbol-package target-package)
         (member (symbol-name sym) symbol-names :test #'string=))))

(defun %known-indent-rule-for-symbol (sym)
  "Return (BODY-START LAMBDA-INDEX IF-P LOCAL-BINDINGS-P DEFMETHOD-P)
for symbols with built-in indentation semantics, or NIL."
  (flet ((rule (body-start lambda-index
                &key if local-bindings defmethod)
           (list body-start lambda-index
                 (and if t) (and local-bindings t) (and defmethod t))))
    (cond
      ((%symbol-named-p sym "COMMON-LISP" "IF")
       (rule nil nil :if t))
      ((%symbol-name-member-p sym "COMMON-LISP"
                              '("PROGN" "LOCALLY" "TAGBODY" "LOOP" "COND"))
       (rule 1 nil))
      ((%symbol-name-member-p sym "COMMON-LISP"
                              '("LAMBDA"))
       (rule 2 1))
      ((%symbol-name-member-p sym "COMMON-LISP"
                              '("LET" "LET*" "WHEN" "UNLESS"
                                "EVAL-WHEN" "WITH-OPEN-FILE" "HANDLER-BIND"
                                "RESTART-BIND" "SYMBOL-MACROLET" "BLOCK"
                                "CATCH" "UNWIND-PROTECT" "HANDLER-CASE"
                                "RESTART-CASE" "DOLIST" "DOTIMES"
                                "DEFSTRUCT" "DEFVAR" "DEFPARAMETER"
                                "DEFCONSTANT"))
       (rule 2 nil))
      ((%symbol-name-member-p sym "COMMON-LISP"
                              '("FLET" "LABELS" "MACROLET"))
       (rule 2 nil :local-bindings t))
      ((%symbol-name-member-p sym "COMMON-LISP"
                              '("DEFUN" "DEFMACRO" "DEFINE-COMPILER-MACRO"
                                "DEFTYPE"))
       (rule 3 2))
      ((%symbol-name-member-p sym "COMMON-LISP"
                              '("DESTRUCTURING-BIND" "MULTIPLE-VALUE-BIND"))
       (rule 3 1))
      ((%symbol-named-p sym "COMMON-LISP" "DEFMETHOD")
       (rule 3 nil :defmethod t))
      ((%symbol-name-member-p sym "COMMON-LISP"
                              '("DEFGENERIC" "WITH-SLOTS" "WITH-ACCESSORS"
                                "DEFINE-CONDITION" "DEFCLASS"))
       (rule 3 nil))
      ((%symbol-name-member-p sym "COALTON"
                              '("COALTON-TOPLEVEL" "DEFINE" "DEFINE-TYPE"
                                "DEFINE-STRUCT" "DEFINE-CLASS"
                                "DEFINE-INSTANCE" "REPR" "MATCH"))
       (rule 1 nil))
      ((%symbol-named-p sym "COALTON" "FN")
       (rule 2 1))
      (t nil))))

(defun %lambda-list-body-start (lambda-list)
  "Derive a conservative body start from a macro lambda list with &BODY.
Returns the form element index where body indentation begins, or NIL."
  (let ((arg-count 0)
        (skip-next nil))
    (dolist (item lambda-list)
      (let ((name (and (symbolp item) (symbol-name item))))
        (cond
          ((and name (string= name "&BODY"))
           (return-from %lambda-list-body-start (1+ arg-count)))
          (skip-next
           (setf skip-next nil))
          ((and name (member name '("&WHOLE" "&ENVIRONMENT") :test #'string=))
           (setf skip-next t))
          ((and name (>= (length name) 1) (char= (char name 0) #\&))
           nil)
          (t
           (incf arg-count)))))
    nil))

(defun %image-indent-rule-for-symbol (sym)
  "Return indentation rule metadata from the live image for SYM.
Known CL/Coalton symbol identities are exact. For other macros, derive body
indentation only when the runtime can expose a macro lambda list containing
&BODY."
  (or (%known-indent-rule-for-symbol sym)
      (let ((property (get sym 'common-lisp-indent-function)))
        (when (and (integerp property) (>= property 0))
          (list (1+ property) nil nil nil nil)))
      (when (macro-function sym)
        (let* ((lambda-list
                 (ignore-errors
                   (sb-introspect:function-lambda-list (macro-function sym))))
               (body-start (and (listp lambda-list)
                                (%lambda-list-body-start lambda-list))))
          (when body-start
            (list body-start nil nil nil nil))))))

(defun %indent-rule-spec-for-head (head package-name)
  "Return a protocol-friendly indentation rule spec for HEAD."
  (let* ((sym (%read-symbol-token-in-package head package-name))
         (rule (and sym (%image-indent-rule-for-symbol sym))))
    (if rule
        (destructuring-bind (body-start lambda-index if-p local-bindings-p defmethod-p)
            rule
          (list head body-start lambda-index
                (and if-p t)
                (and local-bindings-p t)
                (and defmethod-p t)))
        (list head nil nil nil nil nil))))

(defun %indent-rule-specs-for-heads (heads package-name)
  "Return indentation rule specs for HEADS resolved in PACKAGE-NAME."
  (let ((seen (make-hash-table :test 'equal))
        (result nil))
    (dolist (head heads (nreverse result))
      (when (and (stringp head) (not (gethash head seen)))
        (setf (gethash head seen) t)
        (push (%indent-rule-spec-for-head head package-name) result)))))

(defun handle-indent-rules (id heads package-name stream)
  "Handle an :indent-rules request."
  (handler-case
      (write-message stream
                     `(:return ,id
                       (:ok ,(%indent-rule-specs-for-heads
                              (if (listp heads) heads nil)
                              package-name))))
    (error (c)
      (write-message stream
                     `(:return ,id
                       (:error ,(format nil "~A" c)))))))

(defun split-string-by-newline (string)
  "Split STRING into a list of substrings at newline boundaries."
  (loop :for start = 0 :then (1+ end)
        :for end = (position #\Newline string :start start)
        :collect (subseq string start (or end (length string)))
        :while end))

(defun %enter-debugger (id condition stream)
  "Enter interactive debug mode. Send debug info to TUI, wait for restart choice."
  (let* ((condition-text (handler-case (format nil "~A" condition)
                           (error () "[unprintable condition]")))
         (restarts (compute-restarts condition))
         (restart-list
           (handler-case
               (loop :for r :in restarts
                     :for i :from 0
                     :collect (list i
                                    (format nil "~A" (restart-name r))
                                    (handler-case (format nil "~A" r)
                                      (error () "[unprintable]"))))
             (error () nil)))
         (frames
           (handler-case
               (let ((bt (with-output-to-string (s)
                           (sb-debug:print-backtrace :stream s :count 20))))
                 (loop :for line :in (split-string-by-newline bt)
                       :for i :from 0
                       :when (plusp (length line))
                       :collect (list i line)))
             (error () (list (list 0 "[backtrace unavailable]"))))))
    ;; Send debug event
    (handler-case
        (write-message stream `(:debug ,id ,condition-text ,restart-list ,frames))
      (error (e)
        ;; If we can't send the debug message, send an error and bail
        (handler-case
            (write-message stream `(:return ,id (:error ,(format nil "Debugger failed: ~A / Original: ~A" e condition-text))))
          (error () nil))
        (throw '%debugger-abort nil)))
    ;; Wait for user choice
    (loop
      (let ((msg (read-message stream)))
        (when (null msg) (return))  ;; connection lost
        (when (consp msg)
          (case (first msg)
            (:debug-restart
              (let ((restart-idx (third msg)))
                (when (and (integerp restart-idx)
                           (< restart-idx (length restarts)))
                  (invoke-restart (nth restart-idx restarts)))))
            (:debug-abort
              (let ((abort-restart (find-restart 'abort condition)))
                (cond
                  (abort-restart
                   (invoke-restart abort-restart))
                  (t
                   (write-message stream
                     `(:return ,id (:error ,(format nil "Aborted: ~A" condition-text))))
                   (throw '%debugger-abort nil)))))
            (otherwise
             (%reject-unexpected-message-during-wait stream msg "the debugger"))))))))

(defun %flush-stderr-capture (stderr-capture stream)
  "Send any captured stderr output as :notify messages, then reset the stream."
  (handler-case
      (let ((text (get-output-stream-string stderr-capture)))
        (when (plusp (length text))
          (dolist (line (split-string-by-newline text))
            (when (plusp (length line))
              (write-message stream `(:notify (:output ,line)))))))
    (error () nil)))

(defun %muffle-warning-if-possible (warning)
  "Muffle WARNING when a MUFFLE-WARNING restart is available."
  (declare (ignore warning))
  (let ((restart (find-restart 'muffle-warning)))
    (when restart
      (invoke-restart restart))))

(defun %emit-diagnostics (stream diagnostics)
  "Emit normalized diagnostic plists as protocol notifications."
  (dolist (diagnostic diagnostics)
    (write-message stream `(:notify (:diagnostic ,@diagnostic)))))

(defun %make-diagnostic-state ()
  "Return mutable diagnostic state: #(items next-group seen-source-conditions authoritative-error-summaries)."
  (vector nil 0 (make-hash-table :test #'eq) nil))

(defun %diagnostic-items (state)
  (svref state 0))

(defun %diagnostic-seen-source-conditions (state)
  (svref state 2))

(defun %diagnostic-authoritative-error-summaries (state)
  (svref state 3))

(defun %diagnostic-count (state)
  (length (%diagnostic-items state)))

(defun %coalton-source-condition-p (condition)
  (or (typep condition 'coalton-impl/source:source-error)
      (typep condition 'coalton-impl/source:source-warning)))

(defun %coalton-source-condition (condition)
  (cond
    ((%coalton-source-condition-p condition)
     condition)
    (t nil)))

(defun %seen-source-condition-p (state source-condition)
  (and source-condition
       (gethash source-condition (%diagnostic-seen-source-conditions state))))

(defun %remember-source-condition! (state source-condition)
  (when source-condition
    (setf (gethash source-condition (%diagnostic-seen-source-conditions state)) t)))

(defun %authoritative-error-summary-match-p (state summary)
  (and (stringp summary)
       (plusp (length summary))
       (loop :for authoritative :in (%diagnostic-authoritative-error-summaries state)
             :thereis (or (search authoritative summary :test #'char-equal)
                          (search summary authoritative :test #'char-equal)))))

(defun %record-authoritative-error-summaries! (state items)
  (dolist (item items)
    (let ((summary (getf item ':summary)))
      (when (and (eq (getf item ':severity) ':error)
                 (stringp summary)
                 (plusp (length summary)))
        (pushnew summary
                 (svref state 3)
                 :test #'string=)))))

(defun %shadowed-by-authoritative-source-error-p (state condition items)
  (and (%diagnostic-authoritative-error-summaries state)
       (not (%coalton-source-condition condition))
       (every (lambda (item)
                (and (eq (getf item ':severity) ':error)
                     (%authoritative-error-summary-match-p state
                                                          (getf item ':summary))))
              items)))

(defun %collect-diagnostics (condition request-id state
                               &key file-override (offset-base 0) (synthetic-prefix 0))
  "Normalize CONDITION and append any diagnostics into STATE."
  (handler-case
      (let ((source-condition (%coalton-source-condition condition)))
        (unless (%seen-source-condition-p state source-condition)
          (let* ((group (svref state 1))
                 (items (mine/protocol/diagnostics:condition-diagnostics
                         condition
                         :request request-id
                         :group group
                         :file-override file-override
                         :offset-base offset-base
                         :synthetic-prefix synthetic-prefix)))
            (when (and items
                       (not (%shadowed-by-authoritative-source-error-p
                             state condition items)))
              (when source-condition
                (%remember-source-condition! state source-condition)
                (%record-authoritative-error-summaries! state items))
              (incf (svref state 1))
              (setf (svref state 0)
                    (nconc (svref state 0) items))))))
    (error () nil)))

(defun %flush-diagnostics (stream state)
  "Emit and clear all queued diagnostics."
  (let ((diagnostics (%diagnostic-items state)))
    (when diagnostics
      (%emit-diagnostics stream diagnostics)
      (setf (svref state 0) nil))))

(defun %diagnostic-error-summary (state)
  "Return the first error-level diagnostic summary from STATE, or NIL."
  (loop :for diagnostic :in (%diagnostic-items state)
        :for severity = (getf diagnostic ':severity)
        :for summary = (getf diagnostic ':summary)
        :when (and (eq severity ':error)
                   (stringp summary)
                   (plusp (length summary)))
          :return summary))

(defun %source-diagnostic-hook (request-id state &key file-override (offset-base 0) (synthetic-prefix 0))
  "Return a Coalton source diagnostic hook that feeds directly into STATE."
  (lambda (condition)
    (%collect-diagnostics condition request-id state
                          :file-override file-override
                          :offset-base offset-base
                          :synthetic-prefix synthetic-prefix)))

(defun handle-eval (id form-string package-name stream)
  "Handle an :eval request with interactive debugger support."
  (let ((stderr-capture (make-string-output-stream)))
    (catch '%debugger-abort
      (handler-bind
          ((serious-condition
             (lambda (condition)
               (unless (or (typep condition 'reader-error)
                           (typep condition 'end-of-file)
                           (typep condition 'package-error))
                 ;; Flush captured stderr so the user sees compilation details
                 (%flush-stderr-capture stderr-capture stream)
                 (%enter-debugger id condition stream))))
           (warning
             (lambda (w)
               (unless (%uninteresting-warning-p w)
                 (handler-case
                     (let* ((class-name (string-downcase
                                         (class-name (class-of w))))
                            (text (format nil "~A" w)))
                       (write-message stream
                         `(:notify (:output ,(format nil ";; ~A: ~A"
                                                     class-name
                                                     (string-trim '(#\Newline #\Space) text))))))
                   (error () nil))))))
        (let ((*error-output* stderr-capture))
          (restart-case
              (handler-case
                  (multiple-value-bind (result output)
                      (mine/runtime/eval:debug-eval form-string package-name
                                                    stream id
                                                    (let ((pkg (find-package (string-upcase package-name))))
                                                      (and pkg (%coalton-package-p pkg))))
                    (when (and output (plusp (length output)))
                      (dolist (line (split-string-by-newline output))
                        (write-message stream `(:notify (:output ,line)))))
                    (write-message stream `(:return ,id (:ok ,(or result "NIL")))))
                (reader-error (c)
                  (write-message stream `(:return ,id (:error ,(format nil "Read error: ~A" c)))))
                (end-of-file (c)
                  (declare (ignore c))
                  (write-message stream `(:return ,id (:error "Read error: unexpected end of input"))))
                (package-error (c)
                  (write-message stream `(:return ,id (:error ,(format nil "Package error: ~A" c))))))
            (abort ()
              :report "Abort evaluation and return to REPL"
              (write-message stream `(:return ,id (:error "Aborted."))))
            (continue ()
              :report "Continue, returning NIL"
              (write-message stream `(:return ,id (:ok "NIL"))))))))))

(defun handle-quick-result (id form-string package-name stream)
  "Handle a :quick-result request without debugger or diagnostic side effects."
  (handler-case
      (multiple-value-bind (display error-text)
          (mine/runtime/eval:quick-result
           form-string package-name
           (let ((pkg (find-package (string-upcase package-name))))
             (and pkg (%coalton-package-p pkg))))
        (if error-text
            (write-message stream `(:return ,id (:error ,error-text)))
            (write-message stream `(:return ,id (:ok ,display)))))
    (sb-sys:interactive-interrupt (c)
      (declare (ignore c))
      (write-message stream `(:return ,id (:error "Interrupted."))))
    (error (c)
      (write-message stream `(:return ,id (:error ,(format nil "~A" c)))))))

(defun handle-compile-string (id form-string document-key package-name position client-prefix-length stream)
  "Handle a :compile-string request with interactive debugger support.
Uses compile-file + load for correct eval-when toplevel semantics."
  (let ((stderr-capture (make-string-output-stream))
        (diag-state (%make-diagnostic-state))
        (file-override (and (stringp document-key) (plusp (length document-key)) document-key))
        (synthetic-prefix (+ (length (mine/runtime/eval:compile-string-source-prefix package-name))
                             (or client-prefix-length 0))))
    (catch '%debugger-abort
      (handler-bind
          ((serious-condition
             (lambda (condition)
               (unless (or (typep condition 'reader-error)
                           (typep condition 'end-of-file)
                           (typep condition 'package-error))
                 (%collect-diagnostics condition id diag-state
                                       :file-override file-override
                                       :offset-base position
                                       :synthetic-prefix synthetic-prefix)
                 (%flush-stderr-capture stderr-capture stream)
                 (%flush-diagnostics stream diag-state)
                 (%enter-debugger id condition stream))))
           (sb-ext:compiler-note
             (lambda (n)
               (%collect-diagnostics n id diag-state
                                     :file-override file-override
                                     :offset-base position
                                     :synthetic-prefix synthetic-prefix)))
           (warning
             (lambda (w)
               (unless (%uninteresting-warning-p w)
                 (%collect-diagnostics w id diag-state
                                       :file-override file-override
                                       :offset-base position
                                       :synthetic-prefix synthetic-prefix)
                 (%muffle-warning-if-possible w)))))
        (let ((*error-output* stderr-capture)
              (coalton-impl/source:*source-diagnostic-hook*
                (%source-diagnostic-hook id diag-state
                                         :file-override file-override
                                         :offset-base position
                                         :synthetic-prefix synthetic-prefix)))
          (restart-case
              (handler-case
                  (multiple-value-bind (result output)
                      (mine/runtime/eval:debug-compile-string
                       form-string package-name stream id
                       (let ((pkg (find-package (string-upcase package-name))))
                         (and pkg (%coalton-package-p pkg))))
                    (when (and output (plusp (length output)))
                      (dolist (line (split-string-by-newline output))
                        (write-message stream `(:notify (:output ,line)))))
                    (%flush-diagnostics stream diag-state)
                    (write-message stream `(:return ,id (:ok ,(or result "T")))))
                (reader-error (c)
                  (%collect-diagnostics c id diag-state
                                        :file-override file-override
                                        :offset-base position
                                        :synthetic-prefix synthetic-prefix)
                  (%flush-stderr-capture stderr-capture stream)
                  (%flush-diagnostics stream diag-state)
                  (write-message stream `(:return ,id (:error ,(format nil "Read error: ~A" c)))))
                (end-of-file (c)
                  (declare (ignore c))
                  (%flush-stderr-capture stderr-capture stream)
                  (%flush-diagnostics stream diag-state)
                  (write-message stream `(:return ,id (:error "Read error: unexpected end of input"))))
                (package-error (c)
                  (%collect-diagnostics c id diag-state
                                        :file-override file-override
                                        :offset-base position
                                        :synthetic-prefix synthetic-prefix)
                  (%flush-stderr-capture stderr-capture stream)
                  (%flush-diagnostics stream diag-state)
                  (write-message stream `(:return ,id (:error ,(format nil "Package error: ~A" c))))))
            (abort ()
              :report "Abort compilation and return to REPL"
              (%flush-stderr-capture stderr-capture stream)
              (%flush-diagnostics stream diag-state)
              (write-message stream `(:return ,id (:error "Aborted."))))
            (continue ()
              :report "Continue, returning NIL"
              (%flush-stderr-capture stderr-capture stream)
              (%flush-diagnostics stream diag-state)
              (write-message stream `(:return ,id (:ok "T"))))))))))

(defun %ct-file-p (filename)
  "Return T if FILENAME has a .ct extension."
  (let ((len (length filename)))
    (and (> len 3)
         (string-equal ".ct" filename :start2 (- len 3)))))

(defun handle-compile-file (id filename load-p stream)
  "Handle a :compile-file request. Captures structured diagnostics.
For .ct files, uses LOAD instead of COMPILE-FILE since Coalton's compiler
runs during macroexpansion and needs the full runtime environment.
Binds IO streams so interactive reads (y-or-n-p, read, etc.) work via the TUI."
  (let* ((diag-state (%make-diagnostic-state))
         (stdout-capture (make-string-output-stream))
         (tis (make-instance 'tui-input-stream
                :wire-stream stream :msg-id id
                :stdout-capture stdout-capture)))
    (handler-case
        (handler-bind
            ((warning
               (lambda (w)
                 (unless (%uninteresting-warning-p w)
                   (%collect-diagnostics w id diag-state)
                   (%muffle-warning-if-possible w))))
             (sb-ext:compiler-note
               (lambda (n)
                 (%collect-diagnostics n id diag-state)))
             (sb-c:compiler-error
               (lambda (c)
                 (%collect-diagnostics c id diag-state)))
             (error
               (lambda (c)
                 (%collect-diagnostics c id diag-state))))
          (let* ((*standard-output* stdout-capture)
                 (*standard-input* tis)
                 (*query-io* (make-two-way-stream tis *standard-output*))
                 (*terminal-io* (make-two-way-stream tis *standard-output*))
                 (coalton-impl/source:*source-diagnostic-hook*
                   (%source-diagnostic-hook id diag-state)))
            (let ((path (pathname filename)))
              (cond
                ((%ct-file-p filename)
                 ;; .ct files: bind the Coalton readtable (as coalton-asdf does)
                 ;; and load source directly
                 (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton)))
                   (load path)))
                (t
                 ;; .lisp files: compile then optionally load the FASL
                 (let ((output-file (compile-file path)))
                   (when (and load-p output-file)
                     (load output-file)))))))
          ;; Send any captured output
          (let ((output (get-output-stream-string stdout-capture)))
            (when (plusp (length output))
              (dolist (line (split-string-by-newline output))
                (write-message stream `(:notify (:output ,line))))))
          (let ((diagnostic-count (%diagnostic-count diag-state)))
            (%flush-diagnostics stream diag-state)
            (write-message stream
                           `(:return ,id
                             (:ok ,(format nil "Compiled ~A (~D diagnostic~:P)"
                                           filename diagnostic-count))))))
      (sb-c:compiler-error (c)
        (let ((output (get-output-stream-string stdout-capture)))
          (when (plusp (length output))
            (dolist (line (split-string-by-newline output))
              (write-message stream `(:notify (:output ,line))))))
        (%flush-diagnostics stream diag-state)
        (write-message stream
                       `(:return ,id
                         (:error ,(format nil "~A" c)))))
      (error (c)
        ;; Send any captured output even on error
        (let ((output (get-output-stream-string stdout-capture)))
          (when (plusp (length output))
            (dolist (line (split-string-by-newline output))
              (write-message stream `(:notify (:output ,line))))))
        (%flush-diagnostics stream diag-state)
        (write-message stream
                       `(:return ,id
                         (:error ,(format nil "~A" c))))))))

(defun handle-beam-system (id system-name asd-path stream)
  "Handle a :beam-system request with structured diagnostics and debugger support."
  (let ((stderr-capture (make-string-output-stream))
        (diag-state (%make-diagnostic-state)))
    (catch '%debugger-abort
      (handler-bind
          ((serious-condition
             (lambda (condition)
               (unless (or (typep condition 'reader-error)
                           (typep condition 'end-of-file)
                           (typep condition 'package-error))
                 (%collect-diagnostics condition id diag-state)
                 (%flush-stderr-capture stderr-capture stream)
                 (%flush-diagnostics stream diag-state)
                 (%enter-debugger id condition stream))))
           (sb-c:compiler-error
             (lambda (c)
               (%collect-diagnostics c id diag-state)))
           (sb-ext:compiler-note
             (lambda (n)
               (%collect-diagnostics n id diag-state)))
           (warning
             (lambda (w)
               (unless (%uninteresting-warning-p w)
                 (%collect-diagnostics w id diag-state)
                 (%muffle-warning-if-possible w)))))
        (let ((*standard-output* stderr-capture)
              (*error-output* stderr-capture)
              (*trace-output* stderr-capture)
              (coalton-impl/source:*source-diagnostic-hook*
                (%source-diagnostic-hook id diag-state)))
          (restart-case
              (handler-case
                  (multiple-value-bind (ok err)
                      (mine/runtime/asdf:beam-system
                       system-name
                       (and (stringp asd-path) (plusp (length asd-path)) asd-path))
                    (let ((output (get-output-stream-string stderr-capture))
                          (diagnostic-error (%diagnostic-error-summary diag-state)))
                      (when (plusp (length output))
                        (dolist (line (split-string-by-newline output))
                          (write-message stream `(:notify (:output ,line)))))
                      (%flush-diagnostics stream diag-state)
                      (cond
                        (ok
                         (write-message stream
                                        `(:return ,id (:ok ,(format nil "Loaded system ~A" system-name)))))
                        (t
                         (write-message stream
                                        `(:return ,id
                                          (:error ,(or diagnostic-error
                                                       err
                                                       (format nil "Failed to load system ~A" system-name)))))))))
                (reader-error (c)
                  (%collect-diagnostics c id diag-state)
                  (%flush-stderr-capture stderr-capture stream)
                  (%flush-diagnostics stream diag-state)
                  (write-message stream `(:return ,id (:error ,(format nil "Read error: ~A" c)))))
                (end-of-file (c)
                  (declare (ignore c))
                  (%flush-stderr-capture stderr-capture stream)
                  (%flush-diagnostics stream diag-state)
                  (write-message stream `(:return ,id (:error "Read error: unexpected end of input"))))
                (package-error (c)
                  (%collect-diagnostics c id diag-state)
                  (%flush-stderr-capture stderr-capture stream)
                  (%flush-diagnostics stream diag-state)
                  (write-message stream `(:return ,id (:error ,(format nil "Package error: ~A" c))))))
            (abort ()
              :report "Abort system load and return to REPL"
              (%flush-stderr-capture stderr-capture stream)
              (%flush-diagnostics stream diag-state)
              (write-message stream `(:return ,id (:error "Aborted."))))
            (continue ()
              :report "Continue, returning NIL"
              (%flush-stderr-capture stderr-capture stream)
              (%flush-diagnostics stream diag-state)
              (write-message stream `(:return ,id (:ok "T"))))))))))

(defun handle-type-of (id symbol-name package-name stream)
  "Handle a :type-of request."
  (let ((info (mine/runtime/introspect:symbol-info
               symbol-name package-name)))
    (cond
      (info
       (write-message stream
                      `(:return ,id (:ok ,info))))
      (t
       (write-message stream
                      `(:return ,id
                        (:error ,(format nil "Symbol ~A not found in ~A"
                                        symbol-name package-name))))))))

(defun %try-coalton-source-location (sym)
  "Look up Coalton source location for SYM.
Returns (((:file . path) (:offset . n))) or NIL."
  (handler-case
      (let* ((env coalton-impl/entry:*global-environment*)
             (entry (coalton-impl/typechecker/environment:lookup-name env sym)))
        (when entry
          (let* ((loc (slot-value entry 'coalton-impl/typechecker/environment::location))
                 (source (coalton-impl/source:location-source loc))
                 (span (coalton-impl/source:location-span loc)))
            (when (and (typep source 'coalton-impl/source::source-file)
                       (consp span))
              (let ((filepath (slot-value source 'coalton-impl/source::name))
                    (char-offset (car span)))
                (when (and filepath (numberp char-offset))
                  (list (list (cons ':file filepath)
                              (cons ':form-path nil)
                              (cons ':offset char-offset)))))))))
    (error () nil)))

(defun handle-find-definition (id symbol-name package-name stream)
  "Handle a :find-definition request. Tries Coalton source first, then sb-introspect."
  (handler-case
      (let* ((sym (%find-symbol-flexibly symbol-name package-name)))
        (cond
          ((null sym)
           (write-message stream
                          `(:return ,id
                            (:error ,(format nil "Symbol ~A not found in ~A"
                                            symbol-name package-name)))))
          (t
           ;; Try Coalton source first (more accurate for .ct files)
           (let ((coalton-sources (%try-coalton-source-location sym)))
             (cond
               (coalton-sources
                (write-message stream
                               `(:return ,id (:ok ,coalton-sources))))
               (t
                ;; Fall back to sb-introspect
                (let ((all-sources nil))
                  (dolist (type '(:function :variable :type :method-combination
                                  :class :compiler-macro :structure))
                    (handler-case
                        (let ((s (sb-introspect:find-definition-sources-by-name
                                  sym type)))
                          (setf all-sources (nconc all-sources s)))
                      (error () nil)))
                  (if all-sources
                      (write-message stream
                                     `(:return ,id
                                       (:ok ,(%format-sources all-sources))))
                      (write-message stream
                                     `(:return ,id
                                       (:error "No definition found")))))))))))
    (error (c)
      (write-message stream
                     `(:return ,id
                       (:error ,(format nil "~A" c)))))))

(defun %format-sources (sources)
  "Format definition sources into alist form for the client."
  (mapcar (lambda (source)
            (let ((pathname (sb-introspect:definition-source-pathname source))
                  (form-path (sb-introspect:definition-source-form-path source))
                  (char-offset
                    (sb-introspect:definition-source-character-offset source)))
              `((:file . ,(when pathname (namestring pathname)))
                (:form-path . ,form-path)
                (:offset . ,char-offset))))
          sources))

(defun %coalton-env ()
  "Return the Coalton global environment."
  coalton-impl/entry:*global-environment*)

(defun %symbol-kind-tag (sym)
  "Return a short kind tag for SYM."
  (let ((env (%coalton-env)))
    (cond
      ((coalton-impl/typechecker/environment:lookup-type env sym :no-error t)        "type")
      ((coalton-impl/typechecker/environment:lookup-class env sym :no-error t)       "class")
      ((coalton-impl/typechecker/environment:lookup-constructor env sym :no-error t)  "ctor")
      ((coalton-impl/typechecker/environment:lookup-value-type env sym :no-error t)   "fn")
      ((and (fboundp sym) (macro-function sym))             "mac")
      ((and (fboundp sym) (special-operator-p sym))         "sf")
      ((and (fboundp sym) (typep (fdefinition sym) 'generic-function)) "gen")
      ((fboundp sym)                                        "fn")
      ((find-class sym nil)                                 "type")
      ((constantp sym)                                      "const")
      ((boundp sym)                                         "var")
      (t                                                    "sym"))))

(defun %symbol-type-sig (sym)
  "Return a Coalton type string for SYM, or CL arglist, or empty string."
  (let* ((env (%coalton-env))
         (ty (coalton-impl/typechecker/environment:lookup-value-type env sym :no-error t)))
    (cond
      (ty (coalton-impl/typechecker/type-string:type-to-string ty env))
      ((fboundp sym) (or (ignore-errors (mine/runtime/introspect::%get-arglist sym)) ""))
      (t ""))))

(defun %coalton-known-p (sym)
  "True if SYM is known to the Coalton type environment."
  (let ((env (%coalton-env)))
    (or (coalton-impl/typechecker/environment:lookup-value-type env sym :no-error t)
        (coalton-impl/typechecker/environment:lookup-type env sym :no-error t)
        (coalton-impl/typechecker/environment:lookup-class env sym :no-error t))))

(defun %coalton-package-p (pkg)
  "True if PKG is a Coalton package.
Matches \"COALTON\" itself, any \"COALTON/...\" stdlib package, or packages that use COALTON."
  (let ((name (package-name pkg)))
    (or (string-equal "COALTON" name)
        (and (>= (length name) 8)
             (string-equal "COALTON/" name :end2 8))
        (let ((coalton-pkg (find-package "COALTON")))
          (and coalton-pkg
               (member coalton-pkg (package-use-list pkg)))))))

(defun %symbol-defined-p (sym pkg)
  "True if SYM has a useful definition.
Coalton-known symbols always pass.  Coalton macros/special forms (fboundp in a
Coalton home package) also pass.  CL-only symbols pass only in non-Coalton packages."
  (or (%coalton-known-p sym)
      (and (macro-function sym)
           (let ((home (symbol-package sym)))
             (and home (%coalton-package-p home))))
      (and (not (%coalton-package-p pkg))
           (or (fboundp sym)
               (boundp sym)
               (find-class sym nil)
               (constantp sym)))))

(defun %parse-prefix-qualifier (raw-prefix buffer-package-name)
  "Parse RAW-PREFIX for a package qualifier, resolving local nicknames.
Returns (values symbol-prefix lookup-package name-qualifier) where:
  - SYMBOL-PREFIX is the part after the colon (upcase)
  - LOOKUP-PACKAGE is the resolved package object
  - NAME-QUALIFIER is the original qualifier string to prepend to results
E.g. (\"vec:push\" \"FRACTAL\") -> (values \"PUSH\" #<PKG COALTON/VECTOR> \"vec:\")
     (\"push\" \"FRACTAL\")     -> (values \"PUSH\" #<PKG FRACTAL> \"\")"
  (let* ((colon-pos (position #\: raw-prefix :from-end t))
         ;; Bind *package* to buffer package so find-package resolves local nicknames
         (buf-pkg (find-package (string-upcase buffer-package-name)))
         (*package* (or buf-pkg *package*)))
    (if colon-pos
        (let* ((sym-part (string-upcase (subseq raw-prefix (1+ colon-pos))))
               (qualifier-str (subseq raw-prefix 0 (1+ colon-pos)))
               (pkg-part (string-trim ":" qualifier-str))
               (pkg (find-package (string-upcase pkg-part))))
          (values sym-part (or pkg buf-pkg) qualifier-str))
        (values (string-upcase raw-prefix) buf-pkg ""))))

(defun %matching-package-names (prefix buf-pkg)
  "Return completion entries for package names/nicknames matching PREFIX.
Checks local nicknames of BUF-PKG, then global package names and nicknames.
Each entry is (name-with-colon \"pkg\" \"\")."
  (let* ((upprefix (string-upcase prefix))
         (len (length upprefix))
         (seen (make-hash-table :test 'equal))
         (matches nil))
    ;; Local nicknames first
    (when buf-pkg
      (dolist (pair (sb-ext:package-local-nicknames buf-pkg))
        (let ((nick (string-downcase (car pair))))
          (when (and (>= (length nick) len)
                     (string-equal upprefix nick :end2 len)
                     (not (gethash nick seen)))
            (setf (gethash nick seen) t)
            (push (list (concatenate 'string nick ":")
                        "pkg" "")
                  matches)))))
    ;; Global packages: name and nicknames
    (dolist (pkg (list-all-packages))
      (dolist (name (cons (package-name pkg) (package-nicknames pkg)))
        (let ((lname (string-downcase name)))
          (when (and (>= (length lname) len)
                     (string-equal upprefix lname :end2 len)
                     (not (gethash lname seen)))
            (setf (gethash lname seen) t)
            (push (list (concatenate 'string lname ":")
                        "pkg" "")
                  matches)))))
    matches))

(defun handle-complete (id raw-prefix buffer-package-name stream)
  "Handle a :complete request for symbol completion.
RAW-PREFIX may contain a package qualifier (e.g. \"vec:push\").
BUFFER-PACKAGE-NAME is the package context for resolving local nicknames.
Returns annotated results: each match is (qualified-name kind type-sig).
When no colon in prefix, also completes package names/nicknames."
  (handler-case
      (multiple-value-bind (sym-prefix pkg qualifier)
          (%parse-prefix-qualifier raw-prefix buffer-package-name)
        (let ((matches nil)
              (has-qualifier (plusp (length qualifier))))
          ;; Symbol completions
          (when pkg
            (flet ((%prefix-matches-p (sym)
                     (and (%symbol-defined-p sym pkg)
                          (>= (length (symbol-name sym))
                               (length sym-prefix))
                          (string= sym-prefix (symbol-name sym)
                                   :end2 (length sym-prefix)))))
              (cond
                (has-qualifier
                 ;; Qualified: own-package symbols first, then inherited
                 (let ((own nil) (inherited nil))
                   (do-symbols (sym pkg)
                     (when (%prefix-matches-p sym)
                       (let* ((name (concatenate 'string qualifier
                                      (string-downcase (symbol-name sym))))
                              (entry (list name
                                          (%symbol-kind-tag sym)
                                          (%symbol-type-sig sym))))
                         (cond
                           ((eq (symbol-package sym) pkg)
                            (unless (find name own :key #'first :test #'string=)
                              (push entry own)))
                           (t
                            (unless (find name inherited :key #'first :test #'string=)
                              (push entry inherited)))))))
                   (setf matches (nconc (sort own #'string< :key #'first)
                                        (sort inherited #'string< :key #'first)))))
                (t
                 ;; Unqualified: all accessible symbols, sorted alphabetically
                 (do-symbols (sym pkg)
                   (when (%prefix-matches-p sym)
                     (let ((name (string-downcase (symbol-name sym))))
                       (unless (find name matches :key #'first :test #'string=)
                         (push (list name
                                     (%symbol-kind-tag sym)
                                     (%symbol-type-sig sym))
                               matches)))))
                 (setf matches (sort matches #'string< :key #'first))))))
          ;; Package name completions (only when no qualifier present)
          (unless has-qualifier
            (setf matches
                  (nconc matches (%matching-package-names raw-prefix pkg))))
          (write-message stream
                         `(:return ,id (:ok ,matches)))))
    (error (c)
      (write-message stream
                     `(:return ,id
                       (:error ,(format nil "~A" c)))))))

(defun handle-arglist (id function-name package-name stream)
  "Handle an :arglist request."
  (let ((arglist (mine/runtime/introspect:function-arglist
                  function-name package-name)))
    (write-message stream
                   `(:return ,id (:ok ,arglist)))))

(defun handle-coalton-type-of (id symbol-name package-name stream)
  "Handle a :coalton-type-of request. Returns Coalton type or CL arglist."
  (handler-case
      (let* ((sym (%find-symbol-flexibly symbol-name package-name)))
        (cond
          ((null sym)
           (write-message stream
                          `(:return ,id
                            (:error ,(format nil "Symbol ~A not found"
                                            symbol-name)))))
          (t
           ;; Try Coalton type first
           (let ((coalton-type (ignore-errors
                                 (mine/runtime/introspect::%coalton-type sym))))
             (cond
               (coalton-type
                (write-message stream
                               `(:return ,id (:ok ,coalton-type))))
               ;; Fall back to CL arglist
               ((fboundp sym)
                (let ((arglist (ignore-errors
                                 (mine/runtime/introspect::%get-arglist sym))))
                  (write-message stream
                                 `(:return ,id
                                   (:ok ,(or arglist ""))))))
               (t
                (write-message stream
                               `(:return ,id
                                 (:error "No type info")))))))))
    (error (c)
      (write-message stream
                     `(:return ,id
                       (:error ,(format nil "~A" c)))))))

;;; Server lifecycle

(defvar *server-socket* nil
  "The listening socket for the protocol server.")

(defvar *server-streams* nil
  "Open client connection streams.")

(defvar *server-primary-stream* nil
  "The foreground client connection stream.")

(defvar *server-streams-lock*
  (mine/bindings/thread:make-mutex "mine-server-streams"))

(defvar *server-running* nil
  "Flag controlling the server loop.")

(defun %remember-server-stream (stream)
  (mine/bindings/thread:with-mutex (*server-streams-lock*)
    (unless *server-primary-stream*
      (setf *server-primary-stream* stream))
    (pushnew stream *server-streams* :test #'eq)))

(defun %forget-server-stream (stream)
  (mine/bindings/thread:with-mutex (*server-streams-lock*)
    (when (eq stream *server-primary-stream*)
      (setf *server-primary-stream* nil))
    (setf *server-streams* (remove stream *server-streams* :test #'eq))))

(defun %primary-server-stream-p (stream)
  (mine/bindings/thread:with-mutex (*server-streams-lock*)
    (eq stream *server-primary-stream*)))

(defun %close-server-streams ()
  (let ((streams nil))
    (mine/bindings/thread:with-mutex (*server-streams-lock*)
      (setf streams *server-streams*)
      (setf *server-streams* nil)
      (setf *server-primary-stream* nil))
    (dolist (stream streams)
      (ignore-errors (close stream)))))

(defun %close-server-socket ()
  (when *server-socket*
    (mine/bindings/socket:socket-close *server-socket*)
    (setf *server-socket* nil)))

(defun start-server (port)
  "Start the protocol server on PORT (0 = auto-assign).
Prints the actual port to *standard-output*, then enters the serve loop.
Blocks until the server is shut down."
  (let ((listen-socket (mine/bindings/socket:socket-listen port)))
    (setf *server-socket* listen-socket)
    (setf *server-running* t)
    (let ((actual-port (mine/bindings/socket:socket-local-port listen-socket)))
      ;; Print port so the parent process can read it
      (format t "~D~%" actual-port)
      (force-output *standard-output*)
      (format *error-output* ";; mine runtime server listening on port ~D~%"
              actual-port)
      (force-output *error-output*)
      (unwind-protect
           (serve-loop listen-socket)
        (stop-server)))))

(defun %serve-connection (stream)
  "Process messages from one client connection until it closes or quits."
  (unwind-protect
       (loop :while *server-running*
             :do (let ((msg (read-message stream)))
                   (cond
                     ((null msg)
                      ;; EOF or read error -- this connection disconnected.
                      (when (%primary-server-stream-p stream)
                        (setf *server-running* nil)
                        (%close-server-socket))
                      (return))
                     (t
                      (let ((result (dispatch-message msg stream)))
                        (when (eq result ':quit)
                          (setf *server-running* nil)
                          (%close-server-socket)
                          (return)))))))
    (%forget-server-stream stream)
    (ignore-errors (close stream))))

(defun serve-loop (listen-socket)
  "Accept connections and process each one in its own thread."
  (loop :while *server-running*
        :do (handler-case
                (let ((stream (mine/bindings/socket:socket-accept listen-socket)))
                  (%remember-server-stream stream)
                  (mine/bindings/thread:make-thread
                   "mine-runtime-client"
                   (lambda () (%serve-connection stream))))
              (error ()
                (unless *server-running*
                  (return))))))

(defun stop-server ()
  "Stop the protocol server and close all connections."
  (setf *server-running* nil)
  (%close-server-streams)
  (%close-server-socket))
