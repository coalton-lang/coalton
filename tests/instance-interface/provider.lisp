(defpackage #:coalton-tests/instance-interface
  (:use #:coalton #:coalton-prelude)
  (:import-from #:coalton/types #:Proxy #:proxy-of))
(in-package #:coalton-tests/instance-interface)

;; Both tag methods take a Proxy and ignore it. Only the dictionary determines
;; which string the client sees.
(coalton-toplevel
  (define-type (Wrapper :a) (Wrap :a))
  (define-class (Needs :a)
    (tag (Proxy :a -> String)))
  (define-class (Outer :a)
    (outer (:a -> String)))
  (define-instance (Needs Integer)
    (define (tag _) "integer"))
  (define-instance (Needs (Wrapper :a) => Outer :a)
    (define (outer x) (tag (proxy-of (Wrap x))))))
