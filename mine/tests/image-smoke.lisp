;;; Run with the saved image: mine --run-lisp-script tests/image-smoke.lisp
;;; Exercise the production child launcher and transport without starting the TUI.

(in-package :cl-user)
(let ((manager (mine/protocol/lifecycle::make-%runtime-manager)))
  (unwind-protect
       (progn
         (assert (mine/protocol/lifecycle::%runtime-do-start manager))
         (let* ((connection (mine/protocol/lifecycle::%runtime-manager-connection manager))
                (stream (mine/protocol/client::%connection-stream connection)))
           (flet ((send-eval (id source)
                    (assert (mine/protocol/client:connection-send-checked!
                             connection (mine/protocol/messages:ReqEval
                                         (mine/protocol/messages:RequestId id) source "CL-USER" coalton:False))))
                  (read-until (predicate)
                    (sb-ext:with-timeout 10
                      (loop for message = (mine/protocol/server::read-message stream)
                            do (assert message) when (funcall predicate message) return message))))
             (send-eval 1 "(defun mine-image-smoke-value () 42) (mine-image-smoke-value)")
             (let ((reply (read-until (lambda (message) (and (eq :return (first message)) (= 1 (second message)))))))
               (assert (eq :ok (first (third reply))))
               (assert (equal '(:values ("42"))
                              (mine/protocol/server::decode-protocol-sexp (second (third reply))))))
             (mine/protocol/client:connection-finish-request! connection (mine/protocol/messages:RequestId 1))
             (send-eval 2 "(progn (write-string \"image-loop-started\") (force-output) (loop))")
             (read-until (lambda (message) (equal '(:notify (:output-chunk 2 "image-loop-started")) message)))
             (assert (mine/protocol/lifecycle:runtime-interrupt! manager))
             (read-until (lambda (message) (and (eq :debug (first message)) (= 2 (second message)))))
             (assert (mine/protocol/client:connection-send-checked!
                      connection (mine/protocol/messages:ReqDebugAbort (mine/protocol/messages:RequestId 2))))
             (assert (equal '(:return 2 (:error "Aborted."))
                            (read-until (lambda (message) (and (eq :return (first message)) (= 2 (second message)))))))
             (mine/protocol/client:connection-finish-request! connection (mine/protocol/messages:RequestId 2))
             (send-eval 3 "(mine-image-smoke-value)")
             (let ((reply (read-until (lambda (message) (and (eq :return (first message)) (= 3 (second message)))))))
               (assert (equal '(:values ("42"))
                              (mine/protocol/server::decode-protocol-sexp (second (third reply))))))
             (mine/protocol/client:connection-finish-request! connection (mine/protocol/messages:RequestId 3))
             (assert (mine/protocol/lifecycle:runtime-process-alive? manager))
             (assert (not (mine/protocol/lifecycle:runtime-interrupt! manager)))
             (format t "~&Saved mine executable: startup, multi-form eval, streaming, scoped interrupt, recovery passed.~%"))))
    (let ((process (mine/protocol/lifecycle::%runtime-manager-process manager)))
      (mine/protocol/lifecycle::%runtime-do-stop manager)
      (when process (sb-ext:process-close process)))))