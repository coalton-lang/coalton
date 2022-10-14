(coalton-library/utils:defstdlib-package #:coalton-library/hash
  (:use
   #:coalton
   #:coalton-library/classes)
  (:export
   #:Hash
   #:combine-hashes
   #:define-sxhash-hasher))

(in-package #:coalton-library/hash)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  #+sbcl
  (repr :native (cl:unsigned-byte 62))

  #+allegro
  (repr :native (cl:unsigned-byte 0 32))

  ;; https://github.com/Clozure/ccl/blob/ff51228259d9dbc8a9cc7bbb08858ef4aa9fe8d0/level-0/l0-hash.lisp#L1885
  #+ccl
  (repr :native (cl:and cl:fixnum cl:unsigned-byte)) 

  #+(not (or sbcl allegro ccl))
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
      #+ccl (cl:logand (cl:logxor lhs (cl:+ rhs #x517cc1b727220a95 (cl:ash lhs 6) (cl:ash lhs -2))) cl:most-positive-fixnum)))

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

(cl:defmacro define-sxhash-hasher (type)
  `(coalton-toplevel
     (define-instance (Hash ,type)
       (define (hash item)
         (lisp Hash (item)
           (cl:sxhash item))))))

(define-sxhash-hasher Hash)

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/HASH")
