(coalton-library/utils:defstdlib-package #:coalton-library/hash
  (:use
   #:coalton
   #:coalton-library/classes)
  (:import-from #:coalton-library/math/hash-defining-macros
                #:define-sxhash-hasher)
  (:export
   #:lisp-combine-hashes
   #:combine-hashes
   #:combine-hashes-order-independent
   #:define-sxhash-hasher))

(in-package #:coalton-library/hash)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(cl:defun lisp-combine-hashes (lhs rhs)
  ;; SBCL has a hash combination function
  #+sbcl (sb-int:mix lhs rhs)

  ;;
  ;; Generic hash combination functions copied from:
  ;; https://stackoverflow.com/questions/5889238/why-is-xor-the-default-way-to-combine-hashes/27952689#27952689
  ;;

  ;; 32bit hash combination
  #+allegro (cl:logxor lhs (cl:+ rhs #x9e3779b9 (cl:ash lhs 6) (cl:ash lhs -2)))

  ;; 64bit hash combination
  ;; logand required on ccl to force the output to be a fixnum
  #+ccl (cl:logand (cl:logxor lhs (cl:+ rhs #x517cc1b727220a95 (cl:ash lhs 6) (cl:ash lhs -2))) cl:most-positive-fixnum))

;; NOTE: Both the Hash class and Hash type are defined in classes.lisp.

(coalton-toplevel
  (declare combine-hashes (Hash -> Hash -> Hash))
  (define (combine-hashes lhs rhs)
    (lisp Hash (lhs rhs)
      (lisp-combine-hashes lhs rhs)))

  (declare combine-hashes-order-independent (Hash -> Hash -> Hash))
  (define (combine-hashes-order-independent lhs rhs)
    (lisp Hash (lhs rhs)
      (cl:logxor lhs rhs)))

  (define-instance (Eq Hash)
    (define (== a b)
      (lisp Boolean (a b)
        (cl:= a b))))

  (define-instance (Ord Hash)
    (define (<=> a b)
      (if (== a b)
          EQ
          (if (lisp Boolean (a b) (to-boolean (cl:> a b)))
              GT
              LT))))

  (define-instance (Semigroup Hash)
    (define (<> a b)
      (combine-hashes a b)))

  (define-instance (Monoid Hash)
    (define mempty
      (lisp Hash ()
        0)))

  (define-instance (Default Hash)
    (define (default)
      (lisp Hash ()
        0)))

  (define-sxhash-hasher Hash))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/HASH")
