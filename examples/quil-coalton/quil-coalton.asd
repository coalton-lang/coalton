;;;; quil-coalton.asd

(asdf:defsystem #:quil-coalton
  :description "A parser for QUIL written in coalton"
  :depends-on (#:coalton)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "string-view")
               (:file "parser")
               (:file "combinators")
               (:file "value-parsers")
               (:file "quil-coalton")))

(asdf:defsystem #:quil-coalton/tests
  :description "Tests for QUIL-COALTON"
  :depends-on (#:quil-coalton #:fiasco)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "quil-coalton-tests")
               (:file "utilities")))
