(in-package #:mine-tests)

(defun check-background-reader-loss-keeps-its-connection ()
  (let* ((state (%test-state))
         (manager (mine/app/state:get-runtime-mgr state))
         (old-stream (make-string-input-stream ""))
         (new-stream (make-string-input-stream ""))
         (old (mine/protocol/client::make-%connection :stream old-stream :active t))
         (replacement (mine/protocol/client::make-%connection :stream new-stream :active t)))
    (unwind-protect
         (progn
           (setf (mine/protocol/lifecycle::%runtime-manager-background-connection manager) old)
           (app::%start-background-proto-reader-cl state old)
           (let ((thread (mine/app/state::%native-box-value
                          (mine/app/state:get-bg-proto-thread state))))
             (sb-thread:join-thread thread :timeout 2 :default :timeout)
             (%check (not (sb-thread:thread-alive-p thread)) "EOF reader did not stop"))
           ;; EOF has reached the mailbox, but a render replaced the connection
           ;; before the main event loop had a chance to drain it.
           (setf (mine/protocol/lifecycle::%runtime-manager-background-connection manager) replacement)
           (app::%register-pending-request state 9101 mine/app/requests:PendingHeap)
           (app::process-protocol-messages! state)
           (%check (mine/protocol/client::%connection-active replacement)
                   "Retired reader's EOF deactivated the replacement connection")
           (%check (app::%pop-pending-request state 9101)
                   "Retired reader's EOF discarded the replacement's request"))
      (close old-stream :abort t)
      (close new-stream :abort t))))

(defun check-background-loss-retires-native-stream ()
  (let* ((state (%test-state))
         (manager (mine/app/state:get-runtime-mgr state))
         (stream (make-string-input-stream ""))
         (connection (mine/protocol/client::make-%connection :stream stream :active t))
         (mailbox (app::%ensure-proto-mailbox state)))
    (unwind-protect
         (progn
           (setf (mine/protocol/lifecycle::%runtime-manager-background-connection manager) connection)
           (app::%register-pending-request state 9102 mine/app/requests:PendingHeap)
           (sb-concurrency:send-message
            mailbox (app::make-%protocol-reader-loss
                     :connection connection :message '(:background-connection-lost)))
           (app::process-protocol-messages! state)
           (%check (not (open-stream-p stream)) "Lost background stream was left open")
           (%check (not (mine/protocol/client::%connection-active connection))
                   "Lost background connection was left active")
           (%check (null (mine/protocol/lifecycle::%runtime-manager-background-connection manager))
                   "Retired background connection remained installed")
           (%check (null (app::%pop-pending-request state 9102))
                   "Lost connection's request was retained"))
      (close stream :abort t))))

(defun check-editor-services-use-package-before-cursor ()
  (with-test-directory (directory)
    (let* ((state (%test-state))
           (path (namestring (merge-pathnames "packages.lisp" directory)))
           (text (format nil "(in-package :first)~%(target )~%(in-package :second)~%(other )"))
           (terminal (mine-tests/editor-layout::fake-terminal 100 30))
           (seen-hints nil)
           (seen-definitions nil))
      (%write-utf8-file path text)
      (app::open-loose-file! state path)
      (%call-with-replaced-runtime-function
       'app::%request-symbol-hint
       (lambda (state name package)
         (declare (ignore state))
         (push (list name package) seen-hints))
       (lambda ()
         (%call-with-replaced-runtime-function
          'app::%request-remote-definition
          (lambda (state name package path position)
            (declare (ignore state path position))
            (push (list name package) seen-definitions))
          (lambda ()
            (dolist (name '("target" "other"))
              (cursor:cursor-move-to-position! (mine/app/state:get-cursor-state state)
                                               (+ 2 (search name text)))
              (app::render-app state terminal)
              (app::jump-to-definition! state))))))
      (%check (equal '(("target" "FIRST") ("other" "SECOND")) (reverse seen-hints))
              "Hints used package declarations after the cursor: ~S" seen-hints)
      (%check (equal '(("target" "FIRST") ("other" "SECOND")) (reverse seen-definitions))
              "Definition lookup used package declarations after the cursor: ~S" seen-definitions))))

(defun check-compile-temporary-belongs-to-compile-after-startup ()
  (with-test-directory (directory)
    (let* ((state (%test-state))
           (path (namestring (merge-pathnames "unsaved.lisp" directory)))
           (wire-path (merge-pathnames "protocol.bin" directory))
           (initialization-id 9110)
           (mine/app/diagnostics::*diagnostic-store*
             (mine/app/diagnostic-store:diagnostic-store-new)))
      (%write-utf8-file path "original")
      (app::open-loose-file! state path)
      (let ((buffer (%test-current-buffer state)))
        (ops:insert-string! buffer (buf:buffer-undo buffer)
                            (mine/app/state:get-cursor-state state) "unsaved ")
        (with-open-file (wire wire-path :direction :output :element-type '(unsigned-byte 8))
          (let ((connection (mine/protocol/client::make-%connection :stream wire :active t)))
            (unwind-protect
                 (%call-with-replaced-runtime-function
                  'mine/protocol/lifecycle::%runtime-get-connection
                  (lambda (manager)
                    (declare (ignore manager))
                    ;; Model initialization being tracked while ensure-runtime!
                    ;; establishes the first connection for this command.
                    (mine/app/diagnostics:track-diagnostic-request initialization-id)
                    (coalton:Some connection))
                  (lambda ()
                    (%call-with-replaced-runtime-function
                     'app::%start-foreground-proto-reader-cl
                     (lambda (state connection) (declare (ignore state connection)) t)
                     (lambda () (app::compile-file! state)))
                    (let* ((request (app::%coalton-optional-value-or-nil
                                     (coalton/cell:read (mine/app/state:get-active-request-cell state))))
                           (compile-id (mine/protocol/messages:request-id-value request))
                           (temporary-files
                             (mine/app/diagnostic-store:store-request-temporary-files
                              mine/app/diagnostics::*diagnostic-store* compile-id)))
                      (%check (null (mine/app/diagnostic-store:store-request-temporary-files
                                     mine/app/diagnostics::*diagnostic-store* initialization-id))
                              "Initialization captured the pending compile source")
                      (%check (= 1 (length temporary-files)) "Compile request did not own its source")
                      (let ((temporary (first temporary-files)))
                        (mine/app/diagnostics:forget-diagnostic-request initialization-id)
                        (%check (probe-file temporary) "Completing initialization deleted compile input")
                        (%check (string= (buf:buffer-document-key buffer)
                                         (mine/app/diagnostics::remap-diagnostic-filepath temporary compile-id))
                                "Compile diagnostic did not map to the unsaved document")
                        (mine/app/diagnostics:forget-diagnostic-request compile-id)
                        (%check (not (probe-file temporary)) "Completing compilation leaked its source")))))
              (mine/app/diagnostics:forget-all-diagnostic-requests))))))))

(defun run-app-source-service-tests ()
  (dolist (test '(check-background-reader-loss-keeps-its-connection
                  check-background-loss-retires-native-stream
                  check-editor-services-use-package-before-cursor
                  check-compile-temporary-belongs-to-compile-after-startup))
    (format t "~&~A~%" test)
    (funcall test))
  t)
