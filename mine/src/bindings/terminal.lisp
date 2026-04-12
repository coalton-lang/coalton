;;;; terminal.lisp -- Portable terminal I/O interface.
;;;;
;;;; Thin wrapper over mine/bindings/platform providing the public API
;;;; for terminal lifecycle, I/O, and size queries.  No platform-specific
;;;; code lives here -- it all delegates to platform.lisp.

(defpackage #:mine/bindings/terminal
  (:use #:cl)
  (:export
   #:terminal-enable-raw-mode
   #:terminal-disable-raw-mode
   #:terminal-get-size
   #:terminal-set-pipe-size
   #:terminal-read-bytes
   #:terminal-write-string
   #:terminal-flush
   #:terminal-reset-for-image-save))

(in-package #:mine/bindings/terminal)

;;; Lifecycle

(defun terminal-enable-raw-mode ()
  "Enable raw terminal mode (no echo, no canonical, no signals)."
  (mine/bindings/platform:platform-enable-raw-mode)
  (values))

(defun terminal-disable-raw-mode ()
  "Restore the terminal to its original state."
  (mine/bindings/platform:platform-disable-raw-mode)
  (values))

;;; Size

(defun terminal-get-size ()
  "Return (VALUES rows cols)."
  (mine/bindings/platform:platform-get-size))

(defun terminal-set-pipe-size (rows cols)
  "Set pipe-mode terminal size (used on Windows when running under mine-app)."
  (mine/bindings/platform:platform-set-pipe-size rows cols))

;;; Input

(defun terminal-read-bytes (timeout-ms)
  "Read available bytes from stdin with TIMEOUT-MS wait.
Returns (VALUES byte-vector count) or (VALUES NIL 0) on timeout."
  (mine/bindings/platform:platform-read-bytes timeout-ms))

;;; Output

(defun terminal-write-string (s)
  "Write string S to the terminal."
  (mine/bindings/platform:platform-write-string s)
  (values))

(defun terminal-flush ()
  "Flush terminal output."
  (mine/bindings/platform:platform-flush)
  (values))

;;; Image save

(defun terminal-reset-for-image-save ()
  "Reset platform state before save-lisp-and-die."
  (mine/bindings/platform:platform-reset-for-image-save)
  (values))
