;;; mine-tests.asd -- Tests for mine.

(asdf:defsystem "mine-tests"
  :description "Tests for mine."
  :depends-on ("mine")
  :perform (asdf:test-op (o s)
                         (declare (ignore o s))
                         (uiop:symbol-call :mine-tests :run-mine-tests-in-subprocess))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "check-update-tests")
               (:file "diagnostics-tests")
               (:file "diagnostic-store-tests")
               (:file "diagnostic-adapter-tests")
               (:file "fixtures")
               (:file "project-tests")
               (:file "syntax-tests")
               (:file "indent-tests")
               (:file "repl-tests")
               (:file "runtime-regression-tests")
               (:file "quick-result-model-tests")
               (:file "editor-regression-tests")
               (:file "editor-layout-tests")
               (:file "source-context-tests")
               (:file "lexer-context-tests")
               (:file "paredit-context-tests")
               (:file "indent-context-tests")
               (:file "completion-context-tests")
               (:file "app-regression-tests")
               (:file "workflow-tests")
               (:file "app-workflow-extra-tests")))
