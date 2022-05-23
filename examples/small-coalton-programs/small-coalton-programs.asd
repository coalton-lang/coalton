(asdf:defsystem #:small-coalton-programs
  :depends-on (#:coalton)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "diff")
               (:file "primes")
               (:file "microbench1")))
