;;;; process-unix.lisp -- Unix process termination via signals.

(in-package #:mine/bindings/process)

(defun process-kill (process)
  "Terminate PROCESS.
Sends SIGTERM first; if the process is still alive after 1 second,
sends SIGKILL."
  (when (sb-ext:process-alive-p process)
    (sb-ext:process-kill process sb-unix:sigterm)
    ;; Give the process a moment to exit gracefully
    (loop :repeat 10
          :while (sb-ext:process-alive-p process)
          :do (sleep 0.1))
    ;; Force-kill if still alive
    (when (sb-ext:process-alive-p process)
      (sb-ext:process-kill process sb-unix:sigkill)
      ;; Wait briefly for the kernel to reap
      (sleep 0.05)))
  (values))
