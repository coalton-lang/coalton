(asdf:defsystem #:small-coalton-programs
  :depends-on (#:coalton)
  :defsystem-depends-on ("coalton-asdf")
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:ct-file "diff")
               (:file "primes")
               (:ct-file "freecalc")
               (:ct-file "brainfold")
               (:file "monads-bank")))
