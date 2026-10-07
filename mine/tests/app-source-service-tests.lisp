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
             (sb-thread:join-thread thread :timeout 2 :default ':timeout)
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

(defun run-app-source-service-tests ()
  (dolist (test '(check-background-reader-loss-keeps-its-connection
                  check-background-loss-retires-native-stream))
    (funcall test))
  t)
