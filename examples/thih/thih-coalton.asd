;;;; thih.asd

(asdf:defsystem #:thih-coalton
  :description "An implementation of Typing Haskell in Haskell in coalton"
  :depends-on (#:coalton)
  :pathname "src/"
  :serial t
  :in-order-to ((test-op (test-op "thih-coalton/tests")))
  :components ((:file "package")
               (:file "thih")))

(asdf:defsystem #:thih-coalton/tests
  :description "Tests for THIH-COALTON"
  :depends-on (#:thih-coalton #:fiasco #:coalton/testing)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "thih-coalton-tests"))
  :perform (test-op (o s)
                    (symbol-call '#:thih-coalton/tests '#:run-tests)))
