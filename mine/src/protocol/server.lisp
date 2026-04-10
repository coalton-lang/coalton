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
           (return nil)))))))

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

      (:compile-string
       (destructuring-bind (id string buffer-name package-name position)
           (rest msg)
         (declare (ignore buffer-name position))
         (handle-compile-string id string package-name stream)))

      (:compile-file
       (destructuring-bind (id filename load-p) (rest msg)
         (handle-compile-file id filename load-p stream)))

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
                   (throw '%debugger-abort nil)))))))))))

(defun %flush-stderr-capture (stderr-capture stream)
  "Send any captured stderr output as :notify messages, then reset the stream."
  (handler-case
      (let ((text (get-output-stream-string stderr-capture)))
        (when (plusp (length text))
          (dolist (line (split-string-by-newline text))
            (when (plusp (length line))
              (write-message stream `(:notify (:output ,line)))))))
    (error () nil)))

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
                                                    stream id)
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

(defun handle-compile-string (id form-string package-name stream)
  "Handle a :compile-string request with interactive debugger support.
Uses compile-file + load for correct eval-when toplevel semantics."
  (let ((stderr-capture (make-string-output-stream)))
    (catch '%debugger-abort
      (handler-bind
          ((serious-condition
             (lambda (condition)
               (unless (or (typep condition 'reader-error)
                           (typep condition 'end-of-file)
                           (typep condition 'package-error))
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
                      (mine/runtime/eval:debug-compile-string
                       form-string package-name stream id)
                    (when (and output (plusp (length output)))
                      (dolist (line (split-string-by-newline output))
                        (write-message stream `(:notify (:output ,line)))))
                    (write-message stream `(:return ,id (:ok ,(or result "T")))))
                (reader-error (c)
                  (write-message stream `(:return ,id (:error ,(format nil "Read error: ~A" c)))))
                (end-of-file (c)
                  (declare (ignore c))
                  (write-message stream `(:return ,id (:error "Read error: unexpected end of input"))))
                (package-error (c)
                  (write-message stream `(:return ,id (:error ,(format nil "Package error: ~A" c))))))
            (abort ()
              :report "Abort compilation and return to REPL"
              (write-message stream `(:return ,id (:error "Aborted."))))
            (continue ()
              :report "Continue, returning NIL"
              (write-message stream `(:return ,id (:ok "T"))))))))))

(defun %ct-file-p (filename)
  "Return T if FILENAME has a .ct extension."
  (let ((len (length filename)))
    (and (> len 3)
         (string-equal ".ct" filename :start2 (- len 3)))))


(defun handle-compile-file (id filename load-p stream)
  "Handle a :compile-file request. Captures compiler notes with source locations.
For .ct files, uses LOAD instead of COMPILE-FILE since Coalton's compiler
runs during macroexpansion and needs the full runtime environment.
Binds IO streams so interactive reads (y-or-n-p, read, etc.) work via the TUI."
  (let* ((notes nil)
         (stdout-capture (make-string-output-stream))
         (tis (make-instance 'tui-input-stream
                :wire-stream stream :msg-id id
                :stdout-capture stdout-capture)))
    (handler-case
        (handler-bind
            ((warning
               (lambda (w)
                 (push (%make-compiler-note w filename) notes)
                 (muffle-warning w)))
             (sb-ext:compiler-note
               (lambda (n)
                 (push (%make-compiler-note n filename) notes))))
          (let* ((*standard-output* stdout-capture)
                 (*standard-input* tis)
                 (*query-io* (make-two-way-stream tis *standard-output*))
                 (*terminal-io* (make-two-way-stream tis *standard-output*)))
            (let ((path (pathname filename)))
              (if (%ct-file-p filename)
                  ;; .ct files: bind the Coalton readtable (as coalton-asdf does)
                  ;; and load source directly
                  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton)))
                    (load path))
                  ;; .lisp files: compile then optionally load the FASL
                  (let ((output-file (compile-file path)))
                    (when (and load-p output-file)
                      (load output-file))))))
          ;; Send any captured output
          (let ((output (get-output-stream-string stdout-capture)))
            (when (plusp (length output))
              (dolist (line (split-string-by-newline output))
                (write-message stream `(:notify (:output ,line))))))
          ;; Send notes as notifications
          (dolist (note (nreverse notes))
            (write-message stream `(:notify (:compiler-note ,@note))))
          (write-message stream
                         `(:return ,id
                           (:ok ,(format nil "Compiled ~A (~D note~:P)"
                                        filename (length notes))))))
      (error (c)
        ;; Send any captured output even on error
        (let ((output (get-output-stream-string stdout-capture)))
          (when (plusp (length output))
            (dolist (line (split-string-by-newline output))
              (write-message stream `(:notify (:output ,line))))))
        ;; Send accumulated notes even on error
        (dolist (note (nreverse notes))
          (write-message stream `(:notify (:compiler-note ,@note))))
        (write-message stream
                       `(:return ,id
                         (:error ,(format nil "~A" c))))))))

(defun %make-compiler-note (condition filename)
  "Extract source location from a compiler condition. Returns a plist."
  (let* ((severity (typecase condition
                     (style-warning :style-warning)
                     (warning :warning)
                     (error :error)
                     (t :note)))
         (message (handler-case (format nil "~A" condition)
                    (error () "[unprintable condition]")))
         (position nil)
         (line nil))
    ;; Try to extract source location from SBCL internals
    (handler-case
        (let ((context (sb-c::find-error-context nil)))
          (when context
            (let ((src (sb-c::compiler-error-context-original-source-path context)))
              (when (and src (consp src) (integerp (first src)))
                (setf position (first src))))))
      (error () nil))
    (list ':severity severity
          ':message message
          ':file filename
          ':position position
          ':line line)))

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

(defun handle-complete (id prefix package-name stream)
  "Handle a :complete request for symbol completion."
  (handler-case
      (let* ((pkg (find-package (string-upcase package-name)))
             (upcase-prefix (string-upcase prefix))
             (matches nil))
        (when pkg
          ;; do-symbols iterates all accessible symbols (own + inherited)
          (do-symbols (sym pkg)
            (when (and (>= (length (symbol-name sym))
                           (length upcase-prefix))
                       (string= upcase-prefix (symbol-name sym)
                                :end2 (length upcase-prefix)))
              (pushnew (string-downcase (symbol-name sym)) matches
                       :test #'string=))))
        (write-message stream
                       `(:return ,id
                         (:ok ,(sort matches #'string<)))))
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

(defvar *server-stream* nil
  "The current client connection stream.")

(defvar *server-running* nil
  "Flag controlling the server loop.")

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

(defun serve-loop (listen-socket)
  "Accept a connection and process messages until quit."
  (let ((stream (mine/bindings/socket:socket-accept listen-socket)))
    (setf *server-stream* stream)
    (unwind-protect
         (loop :while *server-running*
               :do (let ((msg (read-message stream)))
                    (cond
                      ((null msg)
                       ;; EOF or read error -- client disconnected
                       (setf *server-running* nil))
                      (t
                       (let ((result (dispatch-message msg stream)))
                         (when (eq result ':quit)
                           (setf *server-running* nil)))))))
      ;; Clean up
      (ignore-errors (close stream))
      (setf *server-stream* nil))))

(defun stop-server ()
  "Stop the protocol server and close all connections."
  (setf *server-running* nil)
  (when *server-stream*
    (ignore-errors (close *server-stream*))
    (setf *server-stream* nil))
  (when *server-socket*
    (mine/bindings/socket:socket-close *server-socket*)
    (setf *server-socket* nil)))
