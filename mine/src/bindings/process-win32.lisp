;;;; process-win32.lisp -- Windows process termination via TerminateProcess.

(in-package #:mine/bindings/process)

(defun process-kill (process)
  "Terminate PROCESS via TerminateProcess.
On Windows there is no graceful signal mechanism, so this always
forces immediate termination."
  (when (sb-ext:process-alive-p process)
    (sb-ext:process-kill process 1)
    (sleep 0.05))
  (values))
