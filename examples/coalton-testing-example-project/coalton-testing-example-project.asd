(defsystem "coalton-testing-example-project"
  :depends-on ("coalton" "named-readtables")
  :components ((:file "main"))
  :in-order-to ((test-op (test-op "coalton-testing-example-project/test"))))

(defsystem "coalton-testing-example-project/test"
  :depends-on ("coalton-testing-example-project"
               "coalton/testing"
               "fiasco")
  :pathname "test/"
  :components ((:file "test"))
  :perform (test-op (o s)
                    (symbol-call '#:coalton-testing-example-project/test '#:run-tests)))
