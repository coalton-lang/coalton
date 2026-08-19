(defpackage #:my-app
  (:use #:coalton)
  (:export #:hello-world))

(coalton-toplevel
 (declare hello-world (Void -> String))
 (define (hello-world)
   "Return \"Hello World\""
   "Hello World!"))
