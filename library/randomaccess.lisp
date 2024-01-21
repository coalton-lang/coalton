(coalton-library/utils:defstdlib-package #:coalton-library/randomaccess
  (:use
   #:coalton
   #:coalton-library/classes)
  (:export
   #:RandomAccessBase
   #:make
   #:length

   #:RandomAccessReadable
   #:unsafe-aref

   #:RandomAccessWritable
   #:unsafe-set!

   #:RandomAccess

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
  (define-class (RandomAccessBase :f :t (:f -> :t))
    "Establishes that `:f` is a random-access store of elements of type `:t`. The **storage type** `:f` implies the **stored type** `:t`.

`make` is permitted to error when it is a storage type that cannot be constructed.

Reading and writing to the storage is done through the `RandomAccessRead` and `RandomAccessWrite` classes."
    (make (UFix -> :t -> :f))
    (length (:f -> UFix)))

  (define-class (RandomAccessBase :f :t => RandomAccessReadable :f :t (:f -> :t))
    "Establishes that `:f` is a random-access store of elements of type `:t`. The **storage type** `:f` implies the **stored type** `:t`. The storage is expected to be sequential and O(1) in random access reads.

See also: `RandomAccessBase`, `RandomAccessWritable`, `RandomAccess`
"
    (unsafe-aref (:f -> UFix -> :t)))

  (define-class (RandomAccessBase :f :t => RandomAccessWritable :f :t (:f -> :t))
    "Establishes that `:f` is a random-access store of elements of type `:t`. The **storage type** `:f` implies the **stored type** `:t`. The storage is expected to be sequential and O(1) in random access writes.

See also: `RandomAccessBase`, `RandomAccessReadable`, `RandomAccess`"
    (unsafe-set! (:f -> UFix -> :t -> Unit)))

  (define-class ((RandomAccessReadable :f :t) (RandomAccessWritable :f :t) => RandomAccess :f :t (:f -> :t))
    "Establishes that `:f` is a random-access store of elements of type `:t`. The **storage type** `:f` implies the **stored type** `:t`. The storage is expected to be sequential and O(1) in random access reads and writes.

See also: `RandomAccessBase`, `RandomAccessReadable`, and `RandomAccessWritable`."))

;;; Derived Functions
(coalton-toplevel
  ;(declare aref (RandomAccessReadable :f :t => :f -> UFix -> (Optional :t)))
  (define (aref storage index)
    "Read the element at `index` of the random-access storage `storage`. Return `None` if the read is out-of-bounds."
    (if (and (<= 0 index) (< index (length storage)))
        (Some (unsafe-aref storage index))
        None))

  ;(declare set! (RandomAccessWritable :f :t => :f -> UFix -> :t -> (Optional Unit)))
  (define (set! storage index value)
    "Write the element `value` at `index` of the random-access storage `storage`. Return `None` if the write is out-of-bounds."
    (if (and (<= 0 index) (< index (length storage)))
        (Some (unsafe-set! storage index value))
        None)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/RANDOMACCESS")
