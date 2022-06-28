(coalton-library/utils:defstdlib-package #:coalton-library/addressable
  (:use
   #:coalton
   #:coalton-library/classes)
  (:export
   #:Addressable #:eq?))
(cl:in-package #:coalton-library/addressable)

;; this package mostly exists to reexport the `Addressable' class, and its method `eq?', which is defined in
;; early-classes.lisp but not exported from coalton-library/classes. it also seems a sensible place to put all
;; the instances we need to manually define.

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(coalton-toplevel
  (declare unsafe-internal-eq? (:any -> :any -> Boolean))
  (define (unsafe-internal-eq? a b)
    (lisp Boolean (a b)
      (cl:eq a b))))

(cl:defmacro define-instance-addressable (ty)
  `(coalton-toplevel
     (define-instance (Addressable ,ty)
       (define eq? unsafe-internal-eq?))))

(define-instance-addressable (List :elt))
(define-instance-addressable String)
