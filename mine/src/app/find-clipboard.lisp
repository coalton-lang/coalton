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
;;; clip/Get-Clipboard on Windows, and wl-clipboard/xclip/xsel on Unix).

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

(defun %find-first-command (candidates)
  (or (find-if (lambda (candidate)
                 (%program-available-p (first candidate)))
               candidates)
      (first candidates)))

(defun find-clipboard-copy-command ()
  #+darwin  (list "/usr/bin/pbcopy" nil)
  #+windows (list "clip.exe" nil)
  #-(or darwin windows)
  (%find-first-command
   (if (%wayland-session-p)
       (list (list "wl-copy" nil)
             (list "xclip" (list "-selection" "clipboard"))
             (list "xsel" (list "--clipboard" "--input")))
       (list (list "xclip" (list "-selection" "clipboard"))
             (list "xsel" (list "--clipboard" "--input"))
             (list "wl-copy" nil)))))

(defun find-clipboard-paste-command ()
  #+darwin  (list "/usr/bin/pbpaste" nil)
  #+windows (list "powershell.exe" (list "-command" "Get-Clipboard"))
  #-(or darwin windows)
  (%find-first-command
   (if (%wayland-session-p)
       (list (list "wl-paste" (list "--no-newline"))
             (list "xclip" (list "-selection" "clipboard" "-o"))
             (list "xsel" (list "--clipboard" "--output")))
       (list (list "xclip" (list "-selection" "clipboard" "-o"))
             (list "xsel" (list "--clipboard" "--output"))
             (list "wl-paste" (list "--no-newline"))))))

(defun initialize-clipboard ()
  (setf *clipboard-copy* (find-clipboard-copy-command)
        *clipboard-paste* (find-clipboard-paste-command))
  (values))
