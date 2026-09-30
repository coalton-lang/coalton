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

(defun check-editor-undo-redo-after-save-is-dirty ()
  (with-test-directory (directory)
    (let* ((state (%test-state))
           (path (merge-pathnames "undo.txt" directory))
           (manager (mine/app/state:get-bufmgr state))
           (cs (mine/app/state:get-cursor-state state)))
      (%write-utf8-file path "base")
      (app::open-loose-file! state (namestring path))
      (let ((buffer (%test-current-buffer state)))
        (ops:insert-string! buffer (buf:buffer-undo buffer) cs "changed-")
        (mine/buffer/manager:bufmgr-save! manager (buf:buffer-id buffer))
        (app::handle-editor-key state (input:KeyCtrl #\z) input:ModNone)
        (%check (string= "base" (gap:gap-to-string (buf:buffer-gap buffer)))
                "Undo did not restore original text")
        (%check (buf:buffer-dirty? buffer) "Undo after save was incorrectly clean")
        (mine/buffer/manager:bufmgr-save! manager (buf:buffer-id buffer))
        (app::handle-editor-key state (input:KeyCtrl #\y) input:ModNone)
        (%check (string= "changed-base" (gap:gap-to-string (buf:buffer-gap buffer)))
                "Redo did not restore changed text")
        (%check (buf:buffer-dirty? buffer) "Redo after save was incorrectly clean")))))

(defun check-preview-keeps-permanently-open-buffer ()
  (with-test-directory (directory)
    (let* ((state (%test-state))
           (a (namestring (merge-pathnames "a.txt" directory)))
           (b (namestring (merge-pathnames "b.txt" directory))))
      (%write-utf8-file a "aaa")
      (%write-utf8-file b "bbb")
      (app::open-loose-file! state a)
      (let ((original (%test-current-buffer state)))
        (app::tree-open-file! state a)
        (app::tree-open-file! state b)
        (%check (eq original
                    (app::%coalton-optional-value-or-nil
                     (mine/buffer/manager:bufmgr-find-by-path
                      (mine/app/state:get-bufmgr state) a)))
                "Previewing another file closed a permanently open buffer")))))

(defun check-streamed-output-keeps-line-boundaries ()
  (let* ((state (%test-state))
         (pane (mine/app/state:get-repl-pane state))
         (widget (repl::.output pane)))
    (mine/widget/text:text-widget-clear! widget)
    (dolist (chunk (list "a" (format nil "b~%") (format nil "~%") "c"))
      (app::handle-proto-msg! state
                             (app::%parse-one-message
                              (list :notify (list :output-chunk 91 chunk)))))
    (repl:repl-pane-append-output! pane "=> result")
    (let* ((lines (coalton/cell:read (mine/widget/text:.tw-lines widget)))
           (actual (loop for i below (coalton/vector:length lines)
                         collect (coalton/vector:index-unsafe i lines))))
      (%check (equal '("ab" "" "c" "=> result") actual)
              "Streaming inserted or lost line breaks: ~S" actual))))

(defun check-repl-package-follows-matching-runtime-acknowledgment ()
  (let* ((state (%test-state))
         (package-cell (mine/app/state:get-repl-package-cell state)))
    (coalton/cell:write! (mine/app/state:.ms-active-request state)
                        (coalton:Some (mine/protocol/messages:RequestId 92)))
    (app::handle-proto-msg! state
                           (app::%parse-one-message '(:notify (:package 91 "stale"))))
    (%check (not (string= "stale" (coalton/cell:read package-cell)))
            "An unrelated request changed the REPL package")
    (app::handle-proto-msg! state
                           (app::%parse-one-message '(:notify (:package 92 "exact-case"))))
    (%check (string= "exact-case" (coalton/cell:read package-cell))
            "Matching package acknowledgment lost exact package identity")))

(defun run-app-regression-tests ()
  (dolist (test '(check-package-inspection-does-not-evaluate-source
                  check-mailbox-preserves-diagnostic-generations
                  check-mailbox-callbacks-run-in-arrival-order
                  check-editor-undo-redo-after-save-is-dirty
                  check-preview-keeps-permanently-open-buffer
                  check-streamed-output-keeps-line-boundaries
                  check-repl-package-follows-matching-runtime-acknowledgment))
    (format t "~&~A~%" test)
    (funcall test))
  t)
