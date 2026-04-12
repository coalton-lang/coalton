;;;; platform-package.lisp -- Package definition for platform-specific terminal I/O.
;;;;
;;;; Defines the interface that platform-unix.lisp and platform-win32.lisp must implement.

(defpackage #:mine/bindings/platform
  (:use #:cl)
  (:export
   #:platform-enable-raw-mode
   #:platform-disable-raw-mode
   #:platform-get-size
   #:platform-set-pipe-size
   #:platform-read-bytes
   #:platform-write-string
   #:platform-flush
   #:platform-reset-for-image-save))
