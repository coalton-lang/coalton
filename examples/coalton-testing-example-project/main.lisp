(defpackage #:coalton-testing-example-project
  (:use #:coalton #:coalton-prelude)
  (:export

   #:always-returns-zero
   #:one-element-list))
(in-package #:coalton-testing-example-project)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare always-returns-zero (Unit -> Integer))
  (define (always-returns-zero)
    "A silly function that should always return 0."
    0)

  (declare one-element-list (:elt -> List :elt))
  (define (one-element-list elt)
    "A silly function that wraps ELT into a list of one element."
    (make-list elt)))
