(coalton-library/utils:defstdlib-package #:coalton-library/addressable
  (:use
   #:coalton
   #:coalton-library/classes)
  (:export
   #:Addressable #:eq? #:eq-hash))
(cl:in-package #:coalton-library/addressable)

;; this package mostly exists to reexport the `Addressable' class, and its method `eq?', which is defined in
;; early-classes.lisp but not exported from coalton-library/classes. it also seems a sensible place to put all
;; the instances we need to manually define, and to define functions which operate on `Addressable' instances.

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

(coalton-toplevel
  (declare eq-hash (Addressable :obj => :obj -> UFix))
  (define (eq-hash obj)
    "Compute a hash for OBJ based on its `eq?' identity.

Calling `eq-hash' on objects which are `eq?' will always return the same hash, assuming the `Addressable'
instance is law-abiding, i.e. directly wraps `cl:eq'.

Calls `CL:SXHASH' internally, so results are implementation-dependent. Recent SBCL versions make a reasonable
effort to provide unique hashes for non-`eq?' objects, but other implementations and older SBCL versions may
be prone to hash collisions on some types."
    (lisp UFix (obj)
      (cl:sxhash obj))))
