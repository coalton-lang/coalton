(asdf:defsystem "coalton-raytrace"
  :description "A deterministic path tracer for allocation benchmarks"
  :license "MIT"
  :depends-on ("coalton")
  :defsystem-depends-on ("coalton-asdf")
  :serial t
  :around-compile
  (lambda (compile)
    (let (#+sbcl (sb-ext:*derive-function-types* t)
          #+sbcl (sb-ext:*block-compile-default* :specified))
      (funcall compile)))
  :components ((:ct-file "raytrace")
               (:file "benchmark"))
  :in-order-to ((asdf:test-op (asdf:test-op "coalton-raytrace/tests"))))

(asdf:defsystem "coalton-raytrace/tests"
  :depends-on ("coalton-raytrace")
  :defsystem-depends-on ("coalton-asdf")
  :components ((:ct-file "tests"))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:coalton-raytrace/tests '#:run-tests)))
