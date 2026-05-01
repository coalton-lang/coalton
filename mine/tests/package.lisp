(defpackage #:mine-tests
  (:use #:cl)
  (:local-nicknames
   (#:diag #:mine/protocol/diagnostics)
   (#:app #:mine/app/mine)
   (#:buf #:mine/buffer/buffer)
   (#:gap #:mine/buffer/gap)
   (#:indent #:mine/syntax/indent)
   (#:input #:mine/term/input)
   (#:cursor #:mine/edit/cursor)
   (#:ops #:mine/edit/operations)
   (#:undo #:mine/edit/undo)
   (#:paredit #:mine/syntax/paredit)
   (#:repl #:mine/pane/repl)
   (#:server #:mine/protocol/server)
   (#:source #:coalton-impl/source)
   (#:wt #:mine/widget/types))
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
                  check-indent-hunchentoot-style-handler-body
                  check-indent-multiple-value-bind-special-form
                  check-indent-lambda-list-keyword-alignment
                  check-indent-lambda-list-keyword-parameter-alignment
                  check-indent-keyword-call-alignment
                  check-indent-flet-local-function-body
                  check-indent-cl-prefixed-multiple-value-bind
                  check-indent-non-cl-prefixed-multiple-value-bind-is-generic
                  check-indent-plain-text-mode-never-indents
                  check-indent-runtime-rules-resolve-shadowed-cl-symbols
                  check-indent-runtime-rules-use-cl-user-for-lisp-default
                  check-indent-newline-before-close-paren-uses-blank-context
                  check-indent-line-tab-hop-to-source
                  check-indent-line-preserves-source-position
                  check-editor-paste-clamps-stale-cursor-to-buffer-end
                  check-repl-structural-editing-pairs-delimiters
                  check-repl-structural-close-paren-in-string-inserts
                  check-repl-structural-close-paren-collapses-empty-form
                  check-repl-structural-delimiters-in-string-insert-literals
                  check-repl-structural-delimiters-in-line-comment-insert-literals
                  check-repl-structural-doublequote-in-string
                  check-repl-structural-escaped-quote-deletes-as-unit
                  check-paredit-matching-ignores-delimiters-in-strings
                  check-repl-structural-editing-alt-sexp-motion
                  check-repl-hint-symbol-extraction
                  check-editor-completion-prefix-extraction
                  check-quick-result-lisp-expression-shows-result
                  check-quick-result-lisp-format-separates-output-and-result
                  check-quick-result-lisp-no-values-is-distinct
                  check-quick-result-lisp-error-is-short
                  check-quick-result-interrupt-request-cancels-eval-thread
                  check-quick-result-selection-range
                  check-quick-result-target-uses-smallest-enclosing-form
                  check-quick-result-popup-ellipsizes-clipped-lines
                  check-quick-result-popup-layout-prioritizes-results
                  check-quick-result-popup-uses-terminal-height
                  check-coalton-none-is-not-current-buffer-at-cl-boundary
                  check-beam-system-emits-diagnostics-before-return
                  check-beam-system-preserves-coalton-error-spans))
    (format t "~&~A~%" test)
    (funcall test))
  t)
