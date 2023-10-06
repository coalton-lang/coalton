(coalton-library/utils:defstdlib-package #:coalton-library/randomaccess
  (:use
   #:coalton
   #:coalton-library/classes)
  (:export
   #:RandomAccess
   #:make
   #:length
   #:readable?
   #:writable?
   #:unsafe-aref
   #:unsafe-set!

   #:aref
   #:set!))

(in-package #:coalton-library/randomaccess)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;; Class Implementation
(coalton-toplevel
  ;; The decision to use a functional dependency was an ergonomic one,
  ;; not a technical one. If we did not establish this dependency, we
  ;; would win flexibility (a single storage type can store multiple
  ;; data types), but we would lose a lot of practical ergonomics
  ;; (e.g., we would need Proxy types, calls to length would need to be
  ;; disambiguated, etc.).
  ;;
  ;; While we lose some flexibility, we still retain some. For
  ;; instance, we can have multiple storage types for double floats
  ;; (think: Lisp arrays, C arrays, GPU arrays, etc.).
  (define-class (RandomAccess :f :t (:f -> :t))
    "Establishes that `:f` is a random-access store of elements of type `:t`. The **storage type** `:f` implies the **stored type** `:t`. The storage is expected to be sequential and O(1) in random access reads and writes.

It is permitted for any of `make`, `unsafe-aref`, or `unsafe-set!` to error."
    (make (UFix -> :t -> :f))
    (length (:f -> UFix))
    (readable? (:f -> Boolean))
    (writable? (:f -> Boolean))
    (unsafe-aref (:f -> UFix -> :t))
    (unsafe-set! (:f -> UFix -> :t -> Unit))))

;;; Derived Functions
(coalton-toplevel
  (declare aref (RandomAccess :f :t => :f -> UFix -> (Optional :t)))
  (define (aref storage index)
    "Read the element at `index` of the random-access storage `storage`. Return `None` if the read is out-of-bounds or not permitted."
    (if (and (readable? storage)
             (<= 0 index)
             (< index (length storage)))
        (Some (unsafe-aref storage index))
        None))

  (declare set! (RandomAccess :f :t => :f -> UFix -> :t -> (Optional Unit)))
  (define (set! storage index value)
    "Write the element `value` at `index` of the random-access storage `storage`. Return `None` if the write is out-of-bounds or not permitted."
    (if (and (writable? storage)
             (<= 0 index)
             (< index (length storage)))
        (Some (unsafe-set! storage index value))
        None)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/RANDOMACCESS")
