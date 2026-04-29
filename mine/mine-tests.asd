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
               (:file "diagnostics-tests")
               (:file "indent-tests")
               (:file "repl-tests")))
