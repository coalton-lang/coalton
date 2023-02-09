(coalton-library/utils:defstdlib-package #:coalton-library/hash
  (:use
   #:coalton
   #:coalton-library/classes)
  (:export
   #:Hash
   #:combine-hashes
   #:define-sxhash-hasher))

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(in-package #:coalton-library/hash)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  #+sbcl
  (repr :native (cl:unsigned-byte 62))
  #+allegro
  (repr :native (cl:unsigned-byte 0 32))
  #+(not (or sbcl allegro))
  #.(cl:error "hashing is not supported on ~A" (cl:lisp-implementation-type))
  (define-type Hash
    "Implementation dependent hash code")

  (define-class (Eq :a => Hash :a)
    "Types which can be hashed for storage in hash tables.

Invariant (== left right) implies (== (hash left) (hash right))."
    (hash (:a -> Hash)))

  (declare combine-hashes (Hash -> Hash -> Hash))
  (define (combine-hashes lhs rhs)
    (lisp Hash (lhs rhs)
      #+sbcl (sb-int:mix lhs rhs)

      ;; Copied from
      ;; https://stackoverflow.com/questions/5889238/why-is-xor-the-default-way-to-combine-hashes/27952689#27952689
      ;;
      ;; 32bit hash combination
      #+allegro (cl:logxor lhs (cl:+ rhs #x9e3779b9 (cl:ash lhs 6) (cl:ash lhs -2)))))

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
        0))))

;; TODO: There's now some issues with how we do macroexpansion emitting COALTON-TOPLEVEL.
(cl:defmacro define-sxhash-hasher (type)
  `(define-instance (Hash ,type)
     (define (hash item)
       shshlshl
       (lisp Hash (item)
         (cl:sxhash item)))))

(coalton-toplevel
  (define-sxhash-hasher Hash))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/HASH")
