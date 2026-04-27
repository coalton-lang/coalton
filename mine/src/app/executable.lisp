;;;; executable.lisp -- Build the mine executable image.

(defpackage #:mine/app/executable
  (:use #:cl)
  (:export #:build))

(in-package #:mine/app/executable)

(defun %detect-version ()
  "Return a version string from the environment or git."
  (or (uiop:getenvp "MINE_VERSION")
      (ignore-errors
        (let ((hash (string-trim
                     '(#\Space #\Newline #\Return)
                     (with-output-to-string (s)
                       (uiop:run-program '("git" "rev-parse" "--short" "HEAD")
                                         :output s)))))
          (when (plusp (length hash))
            (format nil "[~A]" hash))))
      "[unknown-build]"))

(defun build ()
  ;; Load SBCL contrib modules so they are available in the saved image.
  ;; sb-alien is part of core SBCL and is always available.
  ;; sb-perf is Linux-only; sb-simd is x86-64-only.
  (let ((modules '("sb-bsd-sockets" "sb-cltl2"
                    "sb-concurrency" "sb-cover" "sb-introspect"
                    "sb-md5" "sb-posix" "sb-queue"
                    "sb-rotate-byte" "sb-sprof"
                    #+x86-64 "sb-simd"
                    #+linux  "sb-perf")))
    (dolist (name modules)
      (require name))
    ;; Register each with ASDF as immutable now, since `require' may
    ;; bypass ASDF and the modules wouldn't be caught by the
    ;; immutability loop below.
    (dolist (name modules)
      (asdf:register-immutable-system name)))
  ;; Reset platform state that won't survive the image save
  (mine/bindings/terminal:terminal-reset-for-image-save)
  ;; Remove Quicklisp from the image so the runtime can detect whether
  ;; the *user* has it installed, rather than always finding the
  ;; build-time copy.
  (setf *features* (remove :quicklisp *features*))
  (when (find-package "QUICKLISP-CLIENT")
    (delete-package "QUICKLISP-CLIENT"))
  (when (find-package "QUICKLISP")
    (delete-package "QUICKLISP"))
  ;; Clear ASDF state that contains build-machine paths.  Without this,
  ;; *central-registry* keeps the builder's Quicklisp path and the
  ;; output-translations point at the builder's FASL cache, causing
  ;; errors (e.g. trying to create /home/runner/) on the end-user's
  ;; machine.  The runtime re-populates these from the user's config.
  (setf asdf:*central-registry* nil)
  (asdf:clear-output-translations)
  (asdf:clear-source-registry)
  ;; Mark all loaded systems as immutable so ASDF never tries to
  ;; recompile them or probe their (now stale) source paths.  Remove
  ;; unloaded systems entirely -- their source won't exist on the
  ;; user's machine.
  (let ((to-remove nil))
    (asdf:map-systems
     (lambda (s)
       (if (asdf:component-loaded-p s)
           (asdf:register-immutable-system (asdf:component-name s))
           (push (asdf:component-name s) to-remove))))
    (dolist (name to-remove)
      (asdf:clear-system name)))
  (setf mine/version:*mine-version* (%detect-version))
  (let ((toplevel (lambda ()
                    ;; The build bypasses uiop:dump-image, so UIOP's image
                    ;; restore hooks haven't run.  Call them now to recompute
                    ;; *user-cache* (and other state) from the runtime user's
                    ;; environment instead of the build machine's.
                    (uiop:call-image-restore-hook)
                    (handler-bind
                        ((error
                           (lambda (c)
                             ;; Write error + backtrace to log file before unwinding
                             (ignore-errors
                               (with-open-file (f "mine-error.log"
                                                   :direction :output
                                                   :if-exists :supersede
                                                   :if-does-not-exist :create)
                                 (format f "MINE ERROR: ~A~%~%Backtrace:~%" c)
                                 (sb-debug:print-backtrace :stream f :count 30)))
                             ;; Restore terminal and print to stdout
                             (ignore-errors
                               (mine/bindings/terminal:terminal-disable-raw-mode))
                             (format t "~&MINE ERROR: ~A~%Written to mine-error.log~%" c)
                             (sb-ext:exit :code 1))))
                      (mine/app/mine:mine-main))
                    (sb-ext:exit))))
    ;; When MINE_SAVE_CORE is set, save a bare core image instead of a
    ;; standalone executable.  The build script then embeds the core
    ;; into a proper Mach-O / PE section via
    ;; make-embedded-core-executable.lisp, producing a binary that can
    ;; be code-signed (Apple notarization, Windows Authenticode).
    ;; The build scripts set this env var when SBCL_SRC_DIR points to an
    ;; SBCL source tree containing the embedding tool.
    (if (uiop:getenvp "MINE_SAVE_CORE")
        (progn
          (format t "~&;; Saving mine.core for embedded-core linking~%")
          (sb-ext:save-lisp-and-die "mine.core" :toplevel toplevel :purify t))
        (sb-ext:save-lisp-and-die
         #+win32 "mine.exe" #-win32 "mine"
         :toplevel toplevel
         :executable t
         :purify t))))
