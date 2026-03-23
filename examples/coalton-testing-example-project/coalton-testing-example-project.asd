(defsystem "coalton-testing-example-project"
  :depends-on ("coalton")
  :defsystem-depends-on ("coalton-asdf")
  :components ((:ct-file "main"))
  :in-order-to ((test-op (test-op "coalton-testing-example-project/test"))))

(defsystem "coalton-testing-example-project/test"
  :depends-on ("coalton-testing-example-project"
               "coalton/testing"
               "fiasco")
  :defsystem-depends-on ("coalton-asdf")
  :pathname "test/"
  :components ((:ct-file "test"))
  :perform (test-op (o s)
                    (symbol-call '#:coalton-testing-example-project/test '#:run-tests)))
