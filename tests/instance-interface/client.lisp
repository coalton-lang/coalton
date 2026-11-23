(in-package #:coalton-tests/instance-interface)

(coalton-toplevel
  (define-instance (Needs :a => Needs (Wrapper :a))
    (define (tag _) "wrapper"))
  (define (call-outer) (outer (the Integer 1))))

(cl:assert (cl:string= "wrapper" (coalton (call-outer))))
