(defpackage #:mine-tests
  (:use #:cl)
  (:local-nicknames
   (#:diag #:mine/protocol/diagnostics)
   (#:server #:mine/protocol/server)
   (#:source #:coalton-impl/source))
  (:export #:run-mine-tests
           #:run-mine-tests-in-subprocess))

(in-package #:mine-tests)

(defun run-mine-tests-in-subprocess ()
  (let* ((mine-dir (asdf:system-source-directory "mine-tests"))
         (repo-root (truename (merge-pathnames "../" mine-dir)))
         (config-path (merge-pathnames "coalton-config.lisp" mine-dir))
         (quicklisp-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
         (cache-dir (merge-pathnames "mine-tests-cache/" (uiop:temporary-directory)))
         (init-registry
           (format nil
                   "(asdf:initialize-source-registry '(:source-registry (:tree #P~S) :ignore-inherited-configuration))"
                   (namestring repo-root)))
         (command
           (list "env"
                 (format nil "XDG_CACHE_HOME=~A" (namestring cache-dir))
                 "sbcl"
                 "--noinform"
                 "--no-userinit"
                 "--no-sysinit"
                 "--non-interactive"
                 "--load" (namestring quicklisp-setup)
                 "--eval" "(pushnew :coalton-portable-bigfloat *features*)"
                 "--load" (namestring config-path)
                 "--eval" init-registry
                 "--eval" "(asdf:load-system \"mine-tests\")"
                 "--eval" "(unless (mine-tests:run-mine-tests) (uiop:quit 1))")))
    (ensure-directories-exist cache-dir)
    (multiple-value-bind (_output _error-output exit-code)
        (uiop:run-program command
                          :output *standard-output*
                          :error-output *error-output*
                          :ignore-error-status t)
      (declare (ignore _output _error-output))
      (when (or (null exit-code) (not (zerop exit-code)))
        (error "mine-tests failed with exit code ~S" exit-code)))))

(defun run-mine-tests ()
  (dolist (test '(check-diagnostics-locate-style-warning-spans
                  check-diagnostics-use-character-offsets-for-unicode-files
                  check-coalton-source-conditions-expand-to-grouped-diagnostics
                  check-wrapped-coalton-source-conditions-use-note-spans
                  check-generic-coalton-toplevel-notes-stay-textual
                  check-source-diagnostic-hook-sees-source-error-subclasses
                  check-reader-errors-produce-point-diagnostics
                  check-beam-system-emits-diagnostics-before-return
                  check-beam-system-preserves-coalton-error-spans))
    (format t "~&~A~%" test)
    (funcall test))
  t)
