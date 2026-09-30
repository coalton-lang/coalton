(in-package #:mine-tests)

(defun %call-with-app-connection (function)
  (with-test-directory (directory)
    (with-open-file (output (merge-pathnames "session-wire.bin" directory)
                            :direction :io :element-type '(unsigned-byte 8)
                            :if-exists :supersede :if-does-not-exist :create)
      (let* ((state (%test-state))
             (connection (mine/protocol/client::make-%connection :stream output :active t)))
        (setf (mine/protocol/lifecycle::%runtime-manager-connection
               (mine/app/state:get-runtime-mgr state)) connection)
        (unwind-protect (funcall function state connection (cons output 0))
          (mine/app/diagnostics:forget-all-diagnostic-requests))))))

(defun %app-sent-messages (output)
  (let* ((stream (car output))
         (end (file-position stream)))
    (file-position stream (cdr output))
    (unwind-protect
         (loop while (< (file-position stream) end)
               for message = (mine/protocol/server::read-message stream)
               do (%check message "Malformed outbound request frame")
               collect message)
      (setf (cdr output) end)
      (file-position stream end))))

(defun %active-app-id (state)
  (let ((request (app::%coalton-optional-value-or-nil
                  (coalton/cell:read (mine/app/state:get-active-request-cell state)))))
    (and request (mine/protocol/messages:request-id-value request))))

(defun check-initialization-owns-prompts-and-defers-first-evaluation ()
  (%call-with-app-connection
   (lambda (state connection output)
     (app::%start-initialization! state connection "initialization")
     (%check (eql 0 (%active-app-id state)) "Initialization is not the active request")
     (app::%send-request! state connection
       (mine/protocol/messages:ReqEval (mine/protocol/messages:RequestId 901)
                                       "user-form" "old-package" coalton:False))
     (let ((sent (%app-sent-messages output)))
       (%check (and (= 1 (length sent)) (eq :eval (first (first sent)))
                    (eql 0 (second (first sent)))
                    (search "initialization" (third (first sent))))
               "The first evaluation was sent before initialization completed"))
     (%check (eql 0 (mine/protocol/client::%connection-foreground-request-id connection))
             "A deferred request stole the initialization interrupt target")
     (app::handle-proto-msg! state (app::%parse-one-message '(:io-request 0 "init input")))
     (%check (coalton/cell:read (mine/app/state:get-io-request-active-cell state))
             "Initialization input was discarded")
     (app::handle-proto-msg! state
       (app::%parse-one-message '(:debug 0 "init condition" ((0 "ABORT" "Abort")) nil)))
     (%check (coalton/cell:read (mine/app/state:get-debugger-active-cell state))
             "Initialization debugger was discarded")
     (app::handle-proto-msg! state (app::%parse-one-message '(:notify (:package 0 "exact-init-package"))))
     (app::handle-proto-msg! state (app::%parse-one-message '(:return 0 (:ok "(:values ())"))))
     (%check (eql 901 (%active-app-id state)) "Deferred evaluation was not activated")
     (%check (equal '((:eval 901 "user-form" "exact-init-package" nil)) (%app-sent-messages output))
             "Deferred REPL evaluation did not inherit the acknowledged package")
     (app::handle-proto-msg! state (app::%parse-one-message '(:return 901 (:ok "(:values (\"42\"))"))))
     (%check (null (%active-app-id state)) "Deferred evaluation did not complete")
     (%check (null (mine/protocol/client::%connection-foreground-request-id connection))
             "Completed evaluation remained interruptible"))))

(defun check-initialization-failure-cancels-deferred-request ()
  (%call-with-app-connection
   (lambda (state connection output)
     (app::%start-initialization! state connection "initialization")
     (app::%send-request! state connection
       (mine/protocol/messages:ReqEval (mine/protocol/messages:RequestId 902)
                                       "user-form" "CL-USER" coalton:False))
     (%app-sent-messages output)
     (app::handle-proto-msg! state (app::%parse-one-message '(:return 0 (:error "Aborted."))))
     (%check (null (%app-sent-messages output)) "Initialization failure still ran deferred code")
     (%check (null (%active-app-id state)) "Initialization failure left a phantom active request")
     (%check (some (lambda (line) (search "deferred request cancelled" line))
                   (repl:repl-pane-output-lines (mine/app/state:get-repl-pane state)))
             "A cancelled deferred request was not explained"))))

(defun check-initialization-preserves-default-package-unless-explicitly-changed ()
  (dolist (example '(("(cl:values 42)" "COALTON-USER")
                     ("(cl:in-package :cl-user) (cl:values 42)" "COMMON-LISP-USER")))
    (%call-with-app-connection
     (lambda (state connection output)
       (app::%start-initialization! state connection (first example))
       (let ((message (first (%app-sent-messages output))))
         (%check (equal '("CL-USER" nil) (cdddr message))
                 "Initialization must use the Common Lisp reader context")
         (multiple-value-bind (result ignored-output package)
             (mine/runtime/eval::%debug-eval (third message) (fourth message))
           (declare (ignore ignored-output))
           (%check (equal '(:values ("42")) (mine/protocol/server::decode-protocol-sexp result))
                   "Initialization did not evaluate the Lisp form")
           (%check (string= (second example) package)
                   "Initialization chose ~A instead of ~A" package (second example))
           (app::handle-proto-msg! state (app::%parse-one-message `(:notify (:package 0 ,package))))
           (app::handle-proto-msg! state (app::%parse-one-message `(:return 0 (:ok ,result))))
           (%check (string= package (coalton/cell:read (mine/app/state:get-repl-package-cell state)))
                   "The acknowledged initialization package was not retained")))))))

(defun check-failed-cancellation-preserves-foreground-request ()
  (let* ((state (%test-state))
         (id (mine/protocol/messages:RequestId 903)))
    (coalton/cell:write! (mine/app/state:get-active-request-cell state) (coalton:Some id))
    (coalton/cell:write! (mine/app/state:get-quick-result-request-id-cell state) (coalton:Some id))
    (%call-with-replaced-runtime-function
     'app::%send-quick-result-interrupt-cl (lambda (&rest args) (declare (ignore args)) nil)
     (lambda () (app::%cancel-quick-result! state)))
    (%check (eql 903 (%active-app-id state)) "Failed cancellation forgot the live foreground evaluation")
    (%check (not (coalton-impl/runtime/optional:cl-none-p
                  (coalton/cell:read (mine/app/state:get-quick-result-request-id-cell state))))
            "Failed cancellation cannot be retried")))

(defun check-runtime-restart-resets-obsolete-package ()
  (let ((state (%test-state)))
    (coalton/cell:write! (mine/app/state:get-repl-package-cell state) "vanished-user-package")
    (%call-with-replaced-runtime-function
     'mine/protocol/lifecycle::%runtime-do-stop (lambda (manager) (declare (ignore manager)) nil)
     (lambda ()
       (%call-with-replaced-runtime-function
        'mine/protocol/lifecycle::%runtime-do-start (lambda (manager) (declare (ignore manager)) nil)
        (lambda () (app::restart-runtime! state)))))
    (%check (not (string= "vanished-user-package"
                         (coalton/cell:read (mine/app/state:get-repl-package-cell state))))
            "Restart retained a package belonging to the old image")))

(defun run-app-runtime-session-tests ()
  (dolist (test '(check-initialization-owns-prompts-and-defers-first-evaluation
                  check-initialization-failure-cancels-deferred-request
                  check-initialization-preserves-default-package-unless-explicitly-changed
                  check-failed-cancellation-preserves-foreground-request
                  check-runtime-restart-resets-obsolete-package))
    (format t "~&~A~%" test)
    (funcall test))
  t)
