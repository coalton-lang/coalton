(asdf:defsystem "my-app"
  :description "Demo lisp applicaiton."
  :author "AUTHOR HERE"
  :license "LICENSE HERE"
  :version (:read-file-form "VERSION.txt")
  :defsystem-depends-on ("coalton-asdf")
  :depends-on ("coalton")
  :serial t
  :pathname "src/"
  :components ((:ct-file "main"))
  :in-order-to ((test-op (test-op "my-app/tests"))))

(asdf:defsystem "my-app/tests"
  :version (:read-file-form "VERSION.txt")
  :depends-on ("my-app" "fiasco")
  :perform (asdf:test-op (o s)
                         (unless (symbol-call :my-app/tests :run-all-tests)
                           (error "Tests failed")))
  :defsystem-depends-on ("coalton-asdf")
  :serial t
  :pathname "tests/"
  :components ((:ct-file "main")))
