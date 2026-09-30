(in-package #:mine-tests)

(defun %runtime-check (value control &rest arguments)
  (unless value
    (error (apply #'format nil control arguments))))

(defun %call-with-runtime-wire-file (function)
  (let ((path (merge-pathnames (format nil "mine-runtime-wire-~A.bin" (gensym))
                              (uiop:temporary-directory))))
    (unwind-protect
         (with-open-file (stream path :direction :io :element-type '(unsigned-byte 8)
                                     :if-exists :supersede :if-does-not-exist :create)
           (funcall function stream))
      (ignore-errors (delete-file path)))))

(defun check-runtime-protocol-io-isolation ()
  (let ((*print-base* 16) (*print-radix* t) (*print-length* 1) (*print-level* 1)
        (*read-base* 16) (*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (%call-with-runtime-wire-file
     (lambda (stream)
       (%runtime-check
        (mine/protocol/server::write-message stream '(:return 12 (:ok "result")))
        "Expected protocol write to succeed")
       (file-position stream 0)
       (%runtime-check
        (equal '(:return 12 (:ok "result"))
               (mine/protocol/server::read-message stream))
        "Protocol round trip inherited user reader/printer controls")))
    (%runtime-check
     (string= "12" (mine/protocol/wire:encode-sexpr (mine/protocol/wire:SInteger 12)))
     "Coalton wire integer encoding inherited printer controls")
    (let* ((*print-base* 10) (*print-radix* nil)
           (payload (mine/runtime/eval::%encode-result-values
                    (list (list 1 2)) (find-package "CL-USER"))))
      (%runtime-check
       (equal (list :values (list "(1 ...)"))
              (mine/protocol/server::decode-protocol-sexp payload))
       "Nested value transport must preserve payload structure and user value printing"))))

(defun check-runtime-protocol-rejects-reader-evaluation ()
  (%runtime-check
   (handler-case
       (progn (mine/protocol/server::decode-protocol-sexp "#.(+ 1 2)") nil)
     (error () t))
   "Protocol reader accepted read-time evaluation")
  (%runtime-check
   (handler-case
       (progn (mine/protocol/server::decode-protocol-sexp "(:ping 1) (:quit 2)") nil)
     (error () t))
   "Protocol reader accepted trailing data"))

(defun %call-with-replaced-runtime-function (name replacement function)
  (let ((original (symbol-function name)))
    (unwind-protect
         (progn (setf (symbol-function name) replacement) (funcall function))
      (setf (symbol-function name) original))))

(defun %call-with-tui-responses (responses function)
  (let ((requests 0))
    (%call-with-replaced-runtime-function
     'mine/protocol/server::%request-input-from-tui
     (lambda (stream prompt)
       (declare (ignore stream prompt))
       (incf requests)
       (pop responses))
     (lambda ()
       (funcall function (make-instance 'mine/protocol/server:tui-input-stream))))
    requests))

(defun check-runtime-input-preserves-lines-and-unread ()
  (%runtime-check
   (= 1 (%call-with-tui-responses
         '("42")
         (lambda (stream) (%runtime-check (= 42 (read stream)) "READ joined submissions"))))
   "READ should consume one submitted numeric line")
  (%runtime-check
   (= 1 (%call-with-tui-responses
         '("abc")
         (lambda (stream)
           (%runtime-check (char= #\a (read-char stream)) "Wrong first character")
           (%runtime-check (string= "bc" (read-line stream)) "READ-LINE discarded buffered text"))))
   "READ-LINE must use remaining buffered characters")
  (%call-with-tui-responses
   '("")
   (lambda (stream)
     (%runtime-check (char= #\Newline (read-char stream)) "Empty line lost its newline")
     (unread-char #\Newline stream)
     (%runtime-check (listen stream) "Unread newline should be available")
     (multiple-value-bind (text eof-p) (read-line stream)
       (%runtime-check (and (string= text "") (not eof-p)) "Unread newline was not restored"))))
  (%runtime-check
   (= 1 (%call-with-tui-responses
         '(nil)
         (lambda (stream)
           (%runtime-check (eq :eof (read-char stream nil :eof)) "Expected EOF")
           (%runtime-check (eq :eof (read-char stream nil :eof)) "Expected persistent EOF"))))
   "EOF should not repeatedly request input"))

(defun %call-with-runtime-messages (function &optional replies)
  (let ((messages nil))
    (%call-with-replaced-runtime-function
     'mine/protocol/server::write-message
     (lambda (stream message)
       (declare (ignore stream))
       (push message messages)
       t)
     (lambda ()
       (%call-with-replaced-runtime-function
        'mine/protocol/server::read-message
        (lambda (stream) (declare (ignore stream)) (pop replies))
        function)))
    (nreverse messages)))

(defun check-runtime-output-flushes-before-debugger-and-abort ()
  (let* ((messages
           (%call-with-runtime-messages
            (lambda ()
              (mine/protocol/server::handle-eval
               41 "(progn (write-string \"before-error\") (error \"boom\"))"
               "CL-USER" :test-wire nil))
            '((:debug-abort 41))))
         (first (first messages)))
    (%runtime-check (equal '(:notify (:output-chunk 41 "before-error")) first)
                    "Expected stdout before debugger, got ~S" first)
    (%runtime-check (eq :debug (first (second messages))) "Expected interactive debugger")
    (%runtime-check (equal '(:return 41 (:error "Aborted.")) (car (last messages)))
                    "Expected an abort reply"))
  (let ((messages
          (%call-with-runtime-messages
           (lambda ()
             (mine/protocol/server::call-with-tui-io
              :test-wire 42
              (lambda ()
                (write-string "a")
                (force-output)
                (write-string "b" *error-output*)
                (write-string "c" *trace-output*)))))))
    (%runtime-check
     (equal '((:notify (:output-chunk 42 "a")) (:notify (:output-chunk 42 "bc"))) messages)
     "Output chunks lost explicit flush boundaries or stream ordering: ~S" messages)))

(defun %runtime-eval-values (text &optional (package "CL-USER") coalton-p)
  (second (mine/protocol/server::decode-protocol-sexp
           (mine/runtime/eval:debug-eval text package nil nil coalton-p))))

(defun check-runtime-multiform-eval-and-history ()
  (let ((+ nil) (++ nil) (+++ nil) (* nil) (** nil) (*** nil)
        (/ nil) (// nil) (/// nil))
    (%runtime-check (equal '("7") (%runtime-eval-values "(+ 1 2) (+ 3 4)"))
                    "REPL ignored trailing forms")
    (%runtime-check (equal '("(+ 3 4)") (%runtime-eval-values "+"))
                    "+ should refer to the previously evaluated form")
    (%runtime-check (equal '("-") (%runtime-eval-values "-"))
                    "- should refer to the form currently being evaluated")
    (%runtime-eval-values "(values 1 2)")
    (%runtime-eval-values "(values)")
    (%runtime-check (equal '("NIL") (%runtime-eval-values "/"))
                    "/ must record zero values")
    (%runtime-check (null (%runtime-eval-values "; only a comment"))
                    "Comment-only input should return no values")))

(defun check-runtime-package-identity-and-success-reporting ()
  (let* ((name (string-downcase (symbol-name (gensym "mine-exact-package-"))))
         (package (make-package name :use '("CL")))
         (missing (symbol-name (gensym "MINE-MISSING-PACKAGE-"))))
    (unwind-protect
         (progn
           (%runtime-check
            (eq package (mine/runtime/eval::find-evaluation-package name))
            "Lowercase package resolved to a different package")
           (let ((messages
                   (%call-with-runtime-messages
                    (lambda ()
                      (mine/protocol/server::handle-eval
                       51 (format nil "(cl:in-package ~S) (+ 1 2)" name)
                       "CL-USER" :test-wire nil)))))
             (%runtime-check (member (list :notify (list :package 51 name)) messages :test #'equal)
                             "Successful qualified IN-PACKAGE was not reported"))
           (let ((messages
                   (%call-with-runtime-messages
                    (lambda ()
                      (mine/protocol/server::handle-eval
                       52 (format nil "(in-package ~S)" missing)
                       "CL-USER" :test-wire nil)))))
             (%runtime-check
              (notany (lambda (message)
                        (and (eq :notify (first message))
                             (eq :package (first (second message))))) messages)
              "Failed package change was reported as successful")
             (%runtime-check (null (find-package missing)) "Missing package was silently created")))
      (delete-package package))))

(defun check-runtime-coalton-multiform-eval ()
  (let* ((name (symbol-name (gensym "MINE-RUNTIME-VALUE-")))
         (input (format nil "(define ~A 10) (+ ~A 2)" name name)))
    (%runtime-check (equal '("12") (%runtime-eval-values input "COALTON-USER" t))
                    "Coalton wrapping must be selected separately for each form")))

(defun check-runtime-debugger-interactive-and-invalid-restarts ()
  (let ((interactive-called nil) (result nil))
    (let ((messages
            (%call-with-runtime-messages
             (lambda ()
               (setf result
                     (restart-case
                         (mine/protocol/server::%enter-debugger
                          61 (make-condition 'simple-error :format-control "test") :test-wire)
                       (supply (value)
                         :report "Supply a value"
                         :interactive (lambda () (setf interactive-called t) (list 42))
                         value))))
             '((:debug-restart 61 999) (:debug-restart 61 0)))))
      (%runtime-check (and interactive-called (eql result 42))
                      "Restart arguments were not collected interactively")
      (%runtime-check
       (find :debug-restart-rejected messages
             :key (lambda (message) (and (eq :notify (first message)) (first (second message)))))
       "Invalid restart should be rejected while keeping the debugger active"))))

(defun check-runtime-beam-errors-reach-debugger ()
  (let ((messages
          (%call-with-runtime-messages
           (lambda ()
             (mine/protocol/server::handle-beam-system
              62 (string-downcase (symbol-name (gensym "mine-absent-system-"))) "" :test-wire))
           '((:debug-abort 62)))))
    (%runtime-check (find :debug messages :key #'first)
                    "System-load error was swallowed before reaching the debugger")
    (%runtime-check (equal '(:return 62 (:error "Aborted.")) (car (last messages)))
                    "System-load debugger should return a completed abort")))

(defun check-runtime-request-scoped-interruption ()
  (let ((thread nil) (worker-error nil))
    (unwind-protect
         (let ((messages
                 (%call-with-runtime-messages
                  (lambda ()
                    (setf thread
                          (sb-thread:make-thread
                           (lambda ()
                             (handler-case
                                 (mine/protocol/server::dispatch-message
                                  '(:eval 71 "(loop (sleep 1))" "CL-USER" nil) :test-wire)
                               (serious-condition (condition) (setf worker-error condition))))
                           :name "mine-interrupt-regression"))
                    (let ((deadline (+ (get-internal-real-time) (* 3 internal-time-units-per-second))))
                      (loop until (mine/protocol/server::%active-request-thread 71)
                            do (%runtime-check (< (get-internal-real-time) deadline)
                                               "Evaluation did not register its active request")
                               (sleep 0.01)))
                    (%runtime-check (mine/protocol/server::%interrupt-active-request 71)
                                    "Request-scoped interrupt was not delivered")
                    (%runtime-check
                     (not (eq :timeout (sb-thread:join-thread thread :timeout 5 :default :timeout)))
                     "Interrupted evaluation did not finish"))
                  '((:debug-abort 71)))))
           (%runtime-check (null worker-error) "Worker failed: ~A" worker-error)
           (%runtime-check (null (mine/protocol/server::%active-request-thread 71))
                           "Completed request remained interruptible")
           (%runtime-check (equal '(:return 71 (:error "Aborted.")) (car (last messages)))
                           "Interrupted REPL evaluation should recover through its debugger"))
      (when (and thread (sb-thread:thread-alive-p thread))
        (sb-thread:terminate-thread thread)))))

(defun check-runtime-registers-every-foreground-request ()
  (dolist (entry '((mine/protocol/server::handle-compile-string
                    (:compile-string 72 "(+ 1 2)" "buffer" "CL-USER" 0 0 nil))
                   (mine/protocol/server::handle-compile-file
                    (:compile-file 72 "file.lisp" t))
                   (mine/protocol/server::handle-beam-system
                    (:beam-system 72 "system" ""))))
    (%call-with-replaced-runtime-function
     (first entry)
     (lambda (&rest args)
       (declare (ignore args))
       (%runtime-check (eq sb-thread:*current-thread*
                           (mine/protocol/server::%active-request-thread 72))
                       "Foreground request was not registered"))
     (lambda () (mine/protocol/server::dispatch-message (second entry) :test-wire)))
    (%runtime-check (null (mine/protocol/server::%active-request-thread 72))
                    "Foreground request registration leaked")))

(defun check-runtime-typed-control-requests-and-send-status ()
  (let* ((rid (mine/protocol/messages:RequestId 81))
         (request (mine/protocol/messages:ReqInterruptRequest rid (mine/protocol/messages:RequestId 71))))
    (%runtime-check
     (equal '(:interrupt-request 81 71)
            (mine/protocol/server::decode-protocol-sexp
             (mine/protocol/wire:encode-sexpr (mine/protocol/messages:request-to-sexpr request))))
     "Typed interrupt request has the wrong wire shape"))
  (%runtime-check
   (equal '(:indent-rules 82 ("let" "when") "CL-USER")
          (mine/protocol/server::decode-protocol-sexp
           (mine/protocol/wire:encode-sexpr
            (mine/protocol/messages:request-to-sexpr
             (mine/protocol/messages:ReqIndentRules
              (mine/protocol/messages:RequestId 82) '("let" "when") "CL-USER")))))
   "Typed indentation request has the wrong wire shape")
  (%call-with-runtime-wire-file
   (lambda (stream)
     (let* ((conn (mine/protocol/client::make-%connection :stream stream :active t))
            (rid (mine/protocol/messages:RequestId 83))
            (request (mine/protocol/messages:ReqEval rid "1" "CL-USER" coalton:False)))
       (%runtime-check (mine/protocol/client:connection-send-checked! conn request)
                       "Expected successful typed send")
       (mine/protocol/client:connection-finish-request! conn (mine/protocol/messages:RequestId 82))
       (%runtime-check (= 83 (mine/protocol/client::%connection-foreground-request-id conn))
                       "Unrelated return cleared the interrupt target")
       (mine/protocol/client:connection-finish-request! conn rid)
       (%runtime-check (null (mine/protocol/client::%connection-foreground-request-id conn))
                       "Matching return did not clear the interrupt target")
       (close stream)
       (%runtime-check (not (mine/protocol/client:connection-send-checked! conn request))
                       "Closed connection reported successful delivery")))))

(coalton:coalton-toplevel
  (coalton:declare %typed-response-valid? (mine/protocol/wire:RawSExpr coalton:-> coalton:Boolean))
  (coalton:define (%typed-response-valid? raw)
    (coalton:match (mine/protocol/wire:raw-to-sexpr raw)
      ((coalton:Some expr)
       (coalton:match (mine/protocol/messages:decode-response expr)
         ((coalton:Some coalton:_) coalton:True)
         ((coalton:None) coalton:False)))
      ((coalton:None) coalton:False)))

  (coalton:declare %typed-wire-roundtrip (mine/protocol/wire:RawSExpr coalton:-> mine/protocol/wire:RawSExpr))
  (coalton:define (%typed-wire-roundtrip raw)
    (coalton:match (mine/protocol/wire:raw-to-sexpr raw)
      ((coalton:Some expr) (mine/protocol/wire:sexpr-to-raw expr))
      ((coalton:None) (mine/protocol/wire:sexpr-to-raw mine/protocol/wire:SNil))))

  (coalton:declare %typed-response-payload (mine/protocol/wire:RawSExpr coalton:-> mine/protocol/wire:RawSExpr))
  (coalton:define (%typed-response-payload raw)
    (coalton:match (mine/protocol/wire:raw-to-sexpr raw)
      ((coalton:Some expr)
       (coalton:match (mine/protocol/messages:decode-response expr)
         ((coalton:Some (mine/protocol/messages:ResponseReturn coalton:_ (coalton-prelude:Ok payload)))
          (mine/protocol/wire:sexpr-to-raw payload))
         (coalton:_ (mine/protocol/wire:sexpr-to-raw mine/protocol/wire:SNil))))
      ((coalton:None) (mine/protocol/wire:sexpr-to-raw mine/protocol/wire:SNil))))

  (coalton:declare %typed-debugger-summary (mine/protocol/wire:RawSExpr coalton:-> coalton:List coalton:String))
  (coalton:define (%typed-debugger-summary raw)
    (coalton:match (mine/protocol/wire:raw-to-sexpr raw)
      ((coalton:Some expr)
       (coalton:match (mine/protocol/messages:decode-response expr)
         ((coalton:Some (mine/protocol/messages:ResponseDebugger snapshot))
          (coalton:make-list
           (coalton-prelude:into (mine/protocol/messages:request-id-value
                          (mine/protocol/messages:debugger-request-id snapshot)))
           (mine/protocol/messages:debugger-condition snapshot)
           (coalton:match (mine/protocol/messages:debugger-restarts snapshot)
             ((coalton:Cons restart coalton:_)
              (mine/protocol/messages:restart-report restart))
             (coalton:_ ""))
           (coalton:match (mine/protocol/messages:debugger-frames snapshot)
             ((coalton:Cons frame coalton:_)
              (mine/protocol/messages:frame-description frame))
             (coalton:_ ""))))
         (coalton:_ coalton:Nil)))
      ((coalton:None) coalton:Nil))))

(defun check-runtime-typed-response-payloads-and-snapshots ()
  (let ((payload '((:name . "foo") (:arglist x &optional y) (:heap . (12 . 4096)))))
    (%runtime-check (equal payload (%typed-response-payload (list :return 91 (list :ok payload))))
                    "Typed return boundary lost dotted pairs or nested metadata")
    (%runtime-check (equal payload (%typed-wire-roundtrip payload))
                    "Raw S-expression conversion failed its alist round trip"))
  (%runtime-check (equal '("92" "boom" "Supply a value" "frame description")
                         (%typed-debugger-summary
                          '(:debug 92 "boom" ((0 "USE-VALUE" "Supply a value"))
                            ((0 "frame description")))))
                  "Typed debugger snapshot lost its structured fields")
  (dolist (message '((:return 1 (:ok nil))
                     (:return 1 (:error "failed"))
                     (:debug 1 "boom" nil nil)
                     (:io-request 1 "")
                     (:notify (:output "line"))
                     (:notify (:output-chunk 1 "text"))
                     (:notify (:package 1 "lowercase-package"))
                     (:notify (:debug-restart-rejected 1 "unavailable"))
                     (:notify (:diagnostic :request-id 1 :summary "failure"))
                     (:connection-lost)
                     (:background-connection-lost)))
    (%runtime-check (%typed-response-valid? message) "Valid reply was rejected: ~S" message)))

(defun check-runtime-typed-response-rejects-malformed-data ()
  (dolist (message (list '(:return -1 (:ok nil))
                         (list :return (ash 1 100) '(:ok nil))
                         '(:return "1" (:ok nil))
                         '(:return 1 (:error nil))
                         '(:return 1 (:ok))
                         '(:return 1 (:ok 2 3))
                         '(:return 1 (:maybe "unknown"))
                         '(:debug 1 "boom" ((-1 "X" "bad")) nil)
                         '(:debug 1 "boom" nil ((0 12)))
                         '(:io-request 1 nil)
                         '(:notify (:output-chunk -1 "text"))
                         '(:notify (:package 1))
                         '(:connection-lost 1)))
    (%runtime-check (not (%typed-response-valid? message))
                    "Malformed reply was accepted: ~S" message))
  (let ((cycle (list :cycle)))
    (setf (cdr cycle) cycle)
    (%runtime-check (eq coalton:None (mine/protocol/wire:raw-to-sexpr cycle))
                    "Circular host data should be rejected"))
  (%runtime-check (eq coalton:None (mine/protocol/wire:raw-to-sexpr (make-hash-table)))
                  "Unsupported host objects should be rejected"))

;;; A separate SBCL process exercises the real socket/Windows lifecycle.  Only
;;; the saved-image launcher is substituted: start, interrupt, and stop use the
;;; production manager, client, protocol server, and OS process adapters.

(defun %runtime-child-source-directories ()
  (remove-duplicates
   (loop for name in (asdf:registered-systems)
         for system = (asdf:find-system name nil)
         for source = (and system (asdf:system-source-file system))
         when source collect (uiop:pathname-directory-pathname source))
   :test #'equal))

(defun %call-with-runtime-process (function)
  (uiop:with-temporary-file (:stream bootstrap :pathname bootstrap-path :type "lisp")
    (with-standard-io-syntax
      (dolist (form
                (append
                 (list '(require :asdf)
                       `(asdf:initialize-source-registry
                         '(:source-registry
                           ,@(mapcar (lambda (path) (list :directory path))
                                     (%runtime-child-source-directories))
                           :ignore-inherited-configuration))
                       `(setf (symbol-plist :coalton-config)
                              ',(copy-list (symbol-plist :coalton-config))))
                 (when (member :coalton-portable-bigfloat *features*)
                   '((pushnew :coalton-portable-bigfloat *features*)))
                 '((let ((*standard-output* *error-output*))
                     (asdf:load-system "mine/runtime"))
                   (mine/runtime/server-main:main))))
        (write form :stream bootstrap)
        (terpri bootstrap)))
    :close-stream
    (uiop:with-temporary-file (:stream errors :pathname error-path :type "log")
      (finish-output errors)
      :close-stream
      (let ((manager (mine/protocol/lifecycle::make-%runtime-manager)))
        (unwind-protect
             (handler-case
                 (%call-with-replaced-runtime-function
                  'mine/bindings/process:spawn-subprocess
                  (lambda (program args)
                    (declare (ignore program args))
                    (sb-ext:run-program
                     (namestring sb-ext:*runtime-pathname*)
                     (list "--noinform" "--no-userinit" "--no-sysinit"
                           "--script" (namestring bootstrap-path))
                     :input nil :output :stream :error error-path
                     :if-error-exists :supersede :wait nil :search t))
                  (lambda ()
                    (%runtime-check (mine/protocol/lifecycle::%runtime-do-start manager)
                                    "Controlled runtime process did not start")
                    (funcall function manager)))
               (error (condition)
                 (error "Runtime process test failed: ~A~%Child stderr:~%~A"
                        condition (uiop:read-file-string error-path))))
          (mine/protocol/lifecycle::%runtime-do-stop manager))))))

(defun %runtime-read-until (manager predicate)
  (let ((stream (mine/protocol/client::%connection-stream
                 (mine/protocol/lifecycle::%runtime-manager-connection manager))))
    (sb-ext:with-timeout 5
      (loop for message = (mine/protocol/server::read-message stream)
            do (%runtime-check message "Runtime connection ended unexpectedly")
            when (funcall predicate message) return message))))

(defun %runtime-process-send-eval (manager id text)
  (%runtime-check
   (mine/protocol/client:connection-send-checked!
    (mine/protocol/lifecycle::%runtime-manager-connection manager)
    (mine/protocol/messages:ReqEval (mine/protocol/messages:RequestId id)
                                    text "CL-USER" coalton:False))
   "Could not send evaluation ~D to runtime process" id))

(defun %runtime-process-return (manager id)
  (let ((message (%runtime-read-until
                  manager (lambda (message)
                            (and (eq :return (first message)) (eql id (second message)))))))
    (mine/protocol/client:connection-finish-request!
     (mine/protocol/lifecycle::%runtime-manager-connection manager)
     (mine/protocol/messages:RequestId id))
    message))

(defun %runtime-process-check-definition (manager id)
  (%runtime-process-send-eval manager id "(mine-regression-retained-definition)")
  (let ((message (%runtime-process-return manager id)))
    (%runtime-check
     (and (eq :ok (first (third message)))
          (equal '(:values ("42"))
                 (mine/protocol/server::decode-protocol-sexp (second (third message)))))
     "Runtime lost the previously defined function: ~S" message))
  (%runtime-check (mine/protocol/lifecycle:runtime-process-alive? manager)
                  "Cancellation terminated the runtime process")
  (%runtime-check (not (mine/protocol/lifecycle:runtime-interrupt! manager))
                  "Completed request remained the lifecycle interrupt target"))

(defun %runtime-process-interrupt-and-abort (manager id)
  (%runtime-check (mine/protocol/lifecycle:runtime-interrupt! manager)
                  "Lifecycle did not deliver a scoped interrupt for request ~D" id)
  (%runtime-read-until manager (lambda (message)
                                (and (eq :debug (first message)) (eql id (second message)))))
  (%runtime-check
   (mine/protocol/client:connection-send-checked!
    (mine/protocol/lifecycle::%runtime-manager-connection manager)
    (mine/protocol/messages:ReqDebugAbort (mine/protocol/messages:RequestId id)))
   "Could not send debugger abort for request ~D" id)
  (%runtime-check (equal (list :return id '(:error "Aborted."))
                         (%runtime-process-return manager id))
                  "Interrupted request ~D did not finish through debugger abort" id))

(defun check-runtime-process-survives-scoped-interruption ()
  (%call-with-runtime-process
   (lambda (manager)
     (%runtime-process-send-eval
      manager 101 "(defun mine-regression-retained-definition () 42)")
     (%runtime-check (eq :ok (first (third (%runtime-process-return manager 101))))
                     "Could not define function in controlled runtime")
     ;; READ-LINE is blocked in the input protocol, while a separate connection
     ;; carries the interrupt.  The original connection then accepts DEBUG-ABORT.
     (%runtime-process-send-eval manager 102 "(read-line)")
     (%runtime-read-until manager (lambda (message)
                                   (and (eq :io-request (first message))
                                        (eql 102 (second message)))))
     (%runtime-process-interrupt-and-abort manager 102)
     (%runtime-process-check-definition manager 103)
     ;; A flushed marker proves the looping evaluation is active before Ctrl-C.
     (%runtime-process-send-eval
      manager 104 "(progn (write-string \"loop-started\") (force-output) (loop (sleep 1)))")
     (%runtime-read-until manager (lambda (message)
                                   (equal '(:notify (:output-chunk 104 "loop-started"))
                                          message)))
     (%runtime-process-interrupt-and-abort manager 104)
     (%runtime-process-check-definition manager 105))))

(defun run-runtime-regression-tests ()
  (dolist (test '(check-runtime-protocol-io-isolation
                  check-runtime-protocol-rejects-reader-evaluation
                  check-runtime-input-preserves-lines-and-unread
                  check-runtime-output-flushes-before-debugger-and-abort
                  check-runtime-multiform-eval-and-history
                  check-runtime-package-identity-and-success-reporting
                  check-runtime-coalton-multiform-eval
                  check-runtime-debugger-interactive-and-invalid-restarts
                  check-runtime-beam-errors-reach-debugger
                  check-runtime-request-scoped-interruption
                  check-runtime-registers-every-foreground-request
                  check-runtime-typed-control-requests-and-send-status
                  check-runtime-typed-response-payloads-and-snapshots
                  check-runtime-typed-response-rejects-malformed-data
                  check-runtime-process-survives-scoped-interruption))
    (handler-case (funcall test)
      (error (condition)
        (format *error-output* "~&Runtime regression ~A failed: ~A~%" test condition)
        (error condition))))
  t)
