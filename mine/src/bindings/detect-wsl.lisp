;;;; detect-wsl.lisp -- Push :wsl onto *features* when running on WSL.
;;;;
;;;; WSL looks like Linux to SBCL (:unix and :linux are both present),
;;;; but the usual Linux clipboard utilities (xclip, xsel, wl-clipboard)
;;;; are not installed by default. Detecting WSL lets callers reach for
;;;; clip.exe / powershell.exe via WSL interop instead.

(in-package #:cl-user)

(when (or (uiop:getenvp "WSL_DISTRO_NAME")
          (uiop:getenvp "WSL_INTEROP")
          ;; Fallback for WSL1: kernel release string contains "microsoft".
          (ignore-errors
           (with-open-file (s "/proc/sys/kernel/osrelease" :direction :input)
             (search "microsoft"
                     (read-line s nil "")
                     :test #'char-equal))))
  (pushnew :wsl *features*))
