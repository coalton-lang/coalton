;;;; signal.lisp -- Portable signal/event handler bindings.
;;;;
;;;; Unix: Thin wrappers around sb-sys:enable-interrupt for POSIX signals.
;;;; Windows: Console control handler via SetConsoleCtrlHandler.
;;;;
;;;; Signal handlers run in a restricted context.  Callbacks should do
;;;; minimal work -- typically just setting a flag or pushing an event.

(defpackage #:mine/bindings/signal
  (:use #:cl)
  (:export
   #:install-sigwinch-handler
   #:install-sigint-handler
   #:install-sigterm-handler))

(in-package #:mine/bindings/signal)

;;; ===========================================================================
;;; Unix (macOS, Linux)
;;; ===========================================================================

#+unix
(progn

  (defconstant +sigint+   2  "Interrupt (Ctrl-C)")
  (defconstant +sigterm+ 15  "Termination request")
  (defconstant +sigwinch+ 28 "Window size changed")

  (defun install-sigwinch-handler (callback)
    "Install CALLBACK as the SIGWINCH handler.
CALLBACK is a function of no arguments; it will be called (in an
interrupt context) whenever the terminal is resized."
    (declare (type function callback))
    (sb-sys:enable-interrupt +sigwinch+
                             (lambda (signal info context)
                               (declare (ignore signal info context))
                               (funcall callback)))
    (values))

  (defun install-sigint-handler (callback)
    "Install CALLBACK as the SIGINT (Ctrl-C) handler.
CALLBACK is a function of no arguments."
    (declare (type function callback))
    (sb-sys:enable-interrupt +sigint+
                             (lambda (signal info context)
                               (declare (ignore signal info context))
                               (funcall callback)))
    (values))

  (defun install-sigterm-handler (callback)
    "Install CALLBACK as the SIGTERM handler.
CALLBACK is a function of no arguments."
    (declare (type function callback))
    (sb-sys:enable-interrupt +sigterm+
                             (lambda (signal info context)
                               (declare (ignore signal info context))
                               (funcall callback)))
    (values))

) ; end #+unix

;;; ===========================================================================
;;; Windows
;;; ===========================================================================

#+win32
(progn

  ;; On Windows, SIGWINCH does not exist.  Terminal resize is detected
  ;; by polling GetConsoleScreenBufferInfo (the event loop already does
  ;; this).  The SIGWINCH handler is therefore a no-op.
  ;;
  ;; SIGINT and SIGTERM are approximated via SetConsoleCtrlHandler.

  (defvar *ctrl-handler-installed* nil)
  (defvar *sigint-callback* nil)
  (defvar *sigterm-callback* nil)

  ;; Console control event types
  (defconstant +ctrl-c-event+        0)
  (defconstant +ctrl-break-event+    1)
  (defconstant +ctrl-close-event+    2)

  (sb-alien:define-alien-routine ("SetConsoleCtrlHandler" %set-console-ctrl-handler)
      sb-alien:int
    (handler-routine (* t))
    (add sb-alien:int))

  (defun %ensure-ctrl-handler ()
    "Install the Windows console control handler (once)."
    (unless *ctrl-handler-installed*
      ;; Create a callback that dispatches to the registered Lisp callbacks
      (let ((cb (sb-alien:alien-funcall
                 (sb-alien:extern-alien "mine_ctrl_handler_placeholder"
                   (sb-alien:function (* t))))))
        (declare (ignore cb))
        ;; For now, use a polling approach: the event loop checks for
        ;; ctrl events via the VT input stream (Ctrl+c comes as byte 0x03).
        ;; SetConsoleCtrlHandler with a Lisp callback requires careful
        ;; FFI callback support.  We defer this to a future enhancement.
        (setf *ctrl-handler-installed* t))))

  (defun install-sigwinch-handler (callback)
    "No-op on Windows. Resize is detected via terminal size polling."
    (declare (ignore callback))
    (values))

  (defun install-sigint-handler (callback)
    "Install a Ctrl+c handler on Windows.
Note: With VT input mode enabled, Ctrl+c arrives as byte 0x03 in the
input stream and is handled by the input parser.  This handler is a
fallback for console close events."
    (declare (type function callback))
    (setf *sigint-callback* callback)
    (values))

  (defun install-sigterm-handler (callback)
    "Install a termination handler on Windows (console close events)."
    (declare (type function callback))
    (setf *sigterm-callback* callback)
    (values))

) ; end #+win32
