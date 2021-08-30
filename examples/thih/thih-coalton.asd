;;;; thih.asd

(asdf:defsystem #:thih-coalton
  :description "An implementation of Typing Haskell in Haskell in coalton"
  :depends-on (#:coalton)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "thih")))

(asdf:defsystem #:thih-coalton/tests
  :description "Tests for THIH-COALTON"
  :depends-on (#:thih-coalton #:fiasco)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "thih-coalton-tests")
               (:file "utilities")))
