(asdf:defsystem #:small-coalton-programs
  :depends-on (#:coalton #:coalton/benchmarking)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "diff")
               (:file "primes")
               (:file "freecalc")
               (:file "microbench1")
	       (:file "brainfold")))
