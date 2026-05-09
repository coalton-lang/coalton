(defpackage #:mine/app/find-clipboard
  (:use #:cl)
  (:export
   #:*clipboard-copy*
   #:*clipboard-paste*
   #:initialize-clipboard
   #:find-clipboard-copy-command
   #:find-clipboard-paste-command))

(in-package #:mine/app/find-clipboard)

;;; System clipboard command detection (pbcopy/pbpaste on macOS,
;;; clip/Get-Clipboard on Windows and WSL, and wl-clipboard/xclip/xsel
;;; on Unix).
;;;
;;; OS dispatch is done at runtime via UIOP, not via #+ reader
;;; conditionals.  Two reasons: (1) SBCL on Windows ships only :win32
;;; (not :windows), so #+windows is unreliable, and (2) the Linux
;;; release is cross-built on a non-WSL CI runner, so a compile-time
;;; #+wsl check would never fire and we'd fall through to xclip/xsel,
;;; which aren't installed on stock WSL Ubuntu, leaving copy/paste a
;;; silent no-op.

(defvar *clipboard-copy* nil)
(defvar *clipboard-paste* nil)

(defun %program-available-p (program)
  (handler-case
      (let ((proc (sb-ext:run-program
                   "sh"
                   (list "-c" (format nil "command -v ~A >/dev/null 2>&1" program))
                   :output nil
                   :error nil
                   :wait t
                   :search t)))
        (eql 0 (sb-ext:process-exit-code proc)))
    (error () nil)))

(defun %non-empty-env-p (name)
  (let ((value (uiop:getenv name)))
    (and value (plusp (length value)))))

(defun %wayland-session-p ()
  (or (%non-empty-env-p "WAYLAND_DISPLAY")
      (string-equal (or (uiop:getenv "XDG_SESSION_TYPE") "")
                    "wayland")))

(defun %wsl-p ()
  "True when running inside the Windows Subsystem for Linux."
  (or (%non-empty-env-p "WSL_DISTRO_NAME")
      (%non-empty-env-p "WSL_INTEROP")
      ;; Fallback for WSL1: kernel release string contains "microsoft".
      (ignore-errors
        (with-open-file (s "/proc/sys/kernel/osrelease" :direction :input)
          (search "microsoft" (read-line s nil "")
                  :test #'char-equal)))))

(defun %find-first-command (candidates)
  (or (find-if (lambda (candidate)
                 (%program-available-p (first candidate)))
               candidates)
      (first candidates)))

(defun find-clipboard-copy-command ()
  (cond
    ((uiop:os-macosx-p)
     (list "/usr/bin/pbcopy" nil))
    ((or (uiop:os-windows-p) (%wsl-p))
     (list "clip.exe" nil))
    ((%wayland-session-p)
     (%find-first-command
      (list (list "wl-copy" nil)
            (list "xclip" (list "-selection" "clipboard"))
            (list "xsel" (list "--clipboard" "--input")))))
    (t
     (%find-first-command
      (list (list "xclip" (list "-selection" "clipboard"))
            (list "xsel" (list "--clipboard" "--input"))
            (list "wl-copy" nil))))))

(defun find-clipboard-paste-command ()
  (cond
    ((uiop:os-macosx-p)
     (list "/usr/bin/pbpaste" nil))
    ((or (uiop:os-windows-p) (%wsl-p))
     (list "powershell.exe" (list "-command" "Get-Clipboard")))
    ((%wayland-session-p)
     (%find-first-command
      (list (list "wl-paste" (list "--no-newline"))
            (list "xclip" (list "-selection" "clipboard" "-o"))
            (list "xsel" (list "--clipboard" "--output")))))
    (t
     (%find-first-command
      (list (list "xclip" (list "-selection" "clipboard" "-o"))
            (list "xsel" (list "--clipboard" "--output"))
            (list "wl-paste" (list "--no-newline")))))))

(defun initialize-clipboard ()
  (setf *clipboard-copy* (find-clipboard-copy-command)
        *clipboard-paste* (find-clipboard-paste-command))
  (values))
