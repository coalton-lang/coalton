(in-package #:mine-tests)

(defvar *source-read-marker* nil)

(defun check-package-inspection-does-not-evaluate-source ()
  (let ((*source-read-marker* nil))
    (app::%detect-buffer-package
     "#.(setf mine-tests::*source-read-marker* t) (in-package #:cl-user)"
     "DEFAULT")
    (%check (null *source-read-marker*) "Source inspection executed a reader form")))

(defun check-mailbox-preserves-diagnostic-generations ()
  (let* ((state (%test-state))
         (mailbox (sb-concurrency:make-mailbox))
         (key "buffer://mailbox-regression")
         (diagnostic (list :request 7001 :file key :start 0 :end 1
                           :severity :error :summary "obsolete" :label-kind :primary)))
    (setf (mine/app/state::%native-box-value (mine/app/state:get-proto-mailbox state))
          mailbox)
    (unwind-protect
         (progn
           (mine/app/diagnostics:track-diagnostic-request 7001)
           (mine/app/diagnostics:invalidate-diagnostics-for-file key)
           (%check (mine/app/diagnostics:diagnostic-stale-p diagnostic)
                   "Expected diagnostic to be stale before draining")
           (sb-concurrency:send-message mailbox (list :notify (cons :diagnostic diagnostic)))
           (sb-concurrency:send-message mailbox '(:return 7001 (:ok "done")))
           (app::%drain-and-parse-messages state)
           (%check (null (mine/app/diagnostics:line-diagnostic-spans key 0 2))
                   "A stale diagnostic was restored while draining the mailbox"))
      (mine/app/diagnostics:forget-diagnostic-request 7001)
      (mine/app/diagnostics:clear-diagnostics-for-file key))))

(defun check-mailbox-callbacks-run-in-arrival-order ()
  (let* ((state (%test-state))
         (mailbox (sb-concurrency:make-mailbox))
         (app::*pending-requests* nil)
         (seen nil))
    (setf (mine/app/state::%native-box-value (mine/app/state:get-proto-mailbox state))
          mailbox)
    (dolist (id '(8001 8002))
      (let ((request-id id))
        (app::%register-pending-request
         request-id (lambda (state payload)
                      (declare (ignore state payload))
                      (push request-id seen))))
      (sb-concurrency:send-message mailbox (list :return id '(:ok "done"))))
    (app::%drain-and-parse-messages state)
    (%check (equal '(8001 8002) (reverse seen))
            "Callbacks ran out of arrival order: ~S" (reverse seen))))

(defun run-app-regression-tests ()
  (dolist (test '(check-package-inspection-does-not-evaluate-source
                  check-mailbox-preserves-diagnostic-generations
                  check-mailbox-callbacks-run-in-arrival-order))
    (format t "~&~A~%" test)
    (funcall test))
  t)
