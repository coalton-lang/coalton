;;;; coalton-json.asd

(asdf:defsystem #:coalton-json
  :description "Example JSON parsers using Coalton."
  :depends-on (#:coalton
               #:json-streams)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "representation")
               (:file "json-streams-parser")))

(asdf:defsystem #:coalton-json/tests
  :description "Tests for COALTON-JSON"
  :depends-on (#:coalton-json #:fiasco)
  :pathname "tests/"
  :serial t
  :components ((:file "tests")))
