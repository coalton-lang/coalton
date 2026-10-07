(asdf:defsystem "coalton-parallel-numerics"
  :description "Numerical simulations parallelized with coalton/threads"
  :license "MIT"
  :depends-on ("coalton" "coalton/threads" "coalton-raytrace")
  :defsystem-depends-on ("coalton-asdf")
  :serial t
  :around-compile
  (lambda (compile)
    (let (#+sbcl (sb-ext:*derive-function-types* t)
          #+sbcl (sb-ext:*block-compile-default* :specified))
      (funcall compile)))
  :components ((:ct-file "common")
               (:ct-file "fdtd")
               (:ct-file "nbody")
               (:ct-file "ising")
               (:ct-file "raytrace")
               (:file "run"))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:coalton-parallel-numerics '#:check-examples)))
