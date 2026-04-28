;;;; package.lisp -- CL binding and runtime package definitions for mine.
;;;;
;;;; Only defines packages needed by mine/runtime (the server-only system).
;;;; Editor-only binding packages (platform, terminal, signal) define their
;;;; own packages in their respective .lisp files.

;;; Remove Quicklisp packages inherited from the user's SBCL environment.
;;; Mine loads Quicklisp on demand via :repl-init, so these must not persist
;;; in the saved image -- otherwise the runtime subprocess inherits stale
;;; packages that conflict with a fresh load of quicklisp/setup.lisp.

(setf *features* (remove :quicklisp *features*))
;; Clear ASDF's loaded-system state so :repl-init can reload from scratch.
;; Without this, ASDF considers quicklisp "already loaded" and skips the
;; load-op, leaving the QUICKLISP package undefined.
(dolist (sys '("quicklisp" "quicklisp-abcl" "quicklisp-slime-helper"))
  (ignore-errors (asdf:clear-system sys)))
(dolist (pkg (list-all-packages))
  (let ((name (package-name pkg)))
    (when (or (search "QUICKLISP" name :test #'char-equal)
              (and (>= (length name) 3)
                   (string-equal "QL-" name :end2 3))
              (string-equal name "QL"))
      (dolist (user (package-used-by-list pkg))
        (ignore-errors (unuse-package pkg user)))
      (ignore-errors (delete-package pkg)))))

;;; CL binding packages (used via `lisp` escape from Coalton)

(defpackage #:mine/bindings/socket
  (:use #:cl)
  (:export
   #:socket-listen
   #:socket-accept
   #:socket-connect
   #:socket-write-string
   #:socket-close
   #:socket-local-port
   #:+socket-error+))

(defpackage #:mine/bindings/process
  (:use #:cl)
  (:export
   #:spawn-subprocess
   #:process-alive-p
   #:process-kill
   #:process-wait
   #:process-exit-code
))

(defpackage #:mine/bindings/thread
  (:use #:cl)
  (:export
   #:make-thread
   #:make-mutex
   #:with-mutex
   #:make-waitqueue
   #:condition-wait
   #:condition-notify
   #:thread-alive-p
   #:current-thread
   #:thread-name))

;;; Protocol server (CL side)

(defpackage #:mine/protocol/server
  (:use #:cl)
  (:export
   #:start-server
   #:stop-server
   #:tui-input-stream))

(defpackage #:mine/protocol/diagnostics
  (:use #:cl)
  (:export
   #:condition-diagnostics))

;;; Runtime packages (CL side)

(defpackage #:mine/runtime/eval
  (:use #:cl)
  (:export
   #:safe-eval
   #:eval-in-package
   #:debug-eval
   #:quick-result
   #:compile-string-source-prefix
   #:debug-compile-string))

(defpackage #:mine/runtime/introspect
  (:use #:cl)
  (:export
   #:symbol-info
   #:function-arglist))

(defpackage #:mine/runtime/asdf
  (:use #:cl)
  (:export
   #:load-system
   #:beam-system))

(defpackage #:mine/runtime/server-main
  (:use #:cl)
  (:export
   #:main))
