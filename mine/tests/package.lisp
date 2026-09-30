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
   (#:proc #:mine/bindings/process)
   (#:server #:mine/protocol/server)
   (#:source #:coalton-impl/source)
   (#:symbols #:mine/app/symbols)
   (#:wt #:mine/widget/types))
  (:export #:run-mine-tests
           #:run-mine-tests-in-subprocess))

(in-package #:mine-tests)

(defun run-mine-tests-in-subprocess ()
  (let* ((mine-dir (asdf:system-source-directory "mine-tests"))
         (command
           (list (or (uiop:getenv "SBCL_BIN") "sbcl")
                 "--dynamic-space-size" "4096"
                 "--noinform"
                 "--no-userinit"
                 "--no-sysinit"
                 "--non-interactive"
                 "--load" (namestring (merge-pathnames "tests/run.lisp" mine-dir)))))
    (multiple-value-bind (_output _error-output exit-code)
        (uiop:run-program command
                          :output *standard-output*
                          :error-output *error-output*
                          :ignore-error-status t)
      (declare (ignore _output _error-output))
      (when (or (null exit-code) (not (zerop exit-code)))
        (error "mine-tests failed with exit code ~S" exit-code)))))

(defun run-mine-tests ()
  (dolist (test '(check-current-release-tag-prefixes-bare-version
                  check-current-release-tag-keeps-prefixed-version
                  check-current-release-tag-accepts-v-prefixed-version
                  check-diagnostics-locate-style-warning-spans
                  check-diagnostics-use-character-offsets-for-unicode-files
                  check-coalton-source-conditions-expand-to-grouped-diagnostics
                  check-wrapped-coalton-source-conditions-use-note-spans
                  check-generic-coalton-toplevel-notes-stay-textual
                  check-source-diagnostic-hook-sees-source-error-subclasses
                  check-reader-errors-produce-point-diagnostics
                  check-symbol-input-fn-alias
                  check-short-lambda-introducer-highlights-as-fn
                  check-chained-short-lambda-introducers-each-highlight
                  check-resize-sequence-emits-resize
                  check-resize-sequence-consumes-before-next-key
                  check-partial-resize-sequence-is-preserved
                  check-terminal-input-zero-timeout-is-nonblocking
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
                  check-editor-nonprinting-character-widths
                  check-crlf-line-text-and-editor-unit-boundaries
                  check-crlf-editor-unit-cursor-motion
                  check-crlf-editor-unit-delete
                  check-crlf-structural-editor-unit-delete
                  check-paredit-forward-join-preserves-newline-separators
                  check-paredit-forward-join-newline-range-detects-formatting
                  check-clipboard-stream-read-preserves-crlf
                  check-indent-line-tab-hop-to-source
                  check-indent-line-preserves-source-position
                  check-indent-line-start-follows-inserted-indentation
                  check-indent-line-after-cursor-preserves-cursor
                  check-editor-paste-clamps-stale-cursor-to-buffer-end
                  check-runtime-coalton-stdlib-packages-classification
                  check-runtime-coalton-auto-wrap-classification
                  check-reexec-program-keeps-bare-command-with-cwd-collision
                  check-reexec-program-canonicalizes-paths
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
                  check-beam-system-preserves-coalton-error-spans
                  check-create-project-creates-new-project
                  check-create-project-refuses-existing-directory
                  check-create-project-rejects-path-like-name
                  check-buffer-manager-any-dirty-sees-non-current-buffer
                  run-runtime-regression-tests
                  run-quick-result-model-tests
                  run-diagnostic-store-tests
                  run-diagnostic-adapter-tests
                  run-diagnostic-cleanup-tests
                  run-editor-regression-tests
                  run-editor-layout-tests
                  run-editor-render-context-tests
                  run-editor-geometry-integration-tests
                  run-editor-wrapped-viewport-tests
                  run-source-context-tests
                  run-lexer-context-tests
                  run-paredit-context-tests
                  run-indent-context-tests
                  run-completion-context-tests
                  run-app-regression-tests
                  run-workflow-tests
                  run-app-workflow-extra-tests
                  run-app-runtime-session-tests
                  run-app-source-service-tests))
    (format t "~&~A~%" test)
    (funcall test))
  t)

(defun run-source-context-tests ()
  (uiop:symbol-call :mine-tests/source-context :run-source-context-tests))

(defun run-indent-context-tests ()
  (uiop:symbol-call :mine-tests/indent-context :run-indent-context-tests))

(defun run-completion-context-tests ()
  (uiop:symbol-call :mine-tests/completion-context :run-completion-context-tests))

(defun run-lexer-context-tests ()
  (uiop:symbol-call :mine-tests/lexer-context :run-lexer-context-tests))

(defun run-paredit-context-tests ()
  (uiop:symbol-call :mine-tests/paredit-context :run-paredit-context-tests))

(defun run-diagnostic-store-tests ()
  (uiop:symbol-call :mine-tests/diagnostic-store :run-diagnostic-store-tests))

(defun run-diagnostic-adapter-tests ()
  (uiop:symbol-call :mine-tests/diagnostic-adapter :run-diagnostic-adapter-tests))

(defun run-diagnostic-cleanup-tests ()
  (uiop:symbol-call :mine-tests/diagnostic-adapter :run-diagnostic-cleanup-tests))
