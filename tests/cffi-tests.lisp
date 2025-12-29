(defpackage #:coalton-native-tests/cffi
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-testing
   #:coalton-cffi)
  (:import-from
   #:coalton-library/math
   #:minbound
   #:maxbound
   #:infinity)
  (:import-from
   #:coalton-library/experimental/loops
   #:dotimes)
  (:export
   #:minbound
   #:maxbound
   #:infinity
   #:test-mem-ref-and-set!
   #:test-mem-aref-and-aset!
   #:test-structs
   #:test-functions
   #:test-enums))

(named-readtables:in-readtable coalton:coalton)

(in-package #:coalton-native-tests/cffi)

(coalton-toplevel

  (declare test-mem-ref-and-set! ((Eq :T) (SimpleForeignRepr :T) => :T -> Unit))
  (define (test-mem-ref-and-set! value)
    "Test foreign-alloc-raw, mem-ref, mem-set!, mem-cpy-raw, mem-cpy, and shift-pointer, using `value` and three 1024-byte buffers."

    ;; Ensure the foreign type size is less than 512.
    (assert (<= (foreign-size-of value) 512))

    ;; Allocate the three 1024-byte buffers.
    (let ptr1 = (foreign-alloc-raw 1024))
    (let ptr2 = (foreign-alloc-raw 1024))
    (let ptr3 = (foreign-alloc-raw 1024))

    ;; Insert `value` at bytes 0 and 512.
    (mem-set! ptr1   0 value)
    (mem-set! ptr1 512 value)

    ;; Copy the first buffer into the second.
    (mem-cpy-raw ptr2 ptr1 1024)

    ;; Copy the element at bytes 0 and 512 into the third buffer.
    (mem-cpy ptr3 ptr1)
    (mem-cpy (shift-pointer 512 ptr3) (shift-pointer 512 ptr1))

    ;; Check that we can read back `value`.
    (is (== value (mem-ref ptr1   0)))
    (is (== value (mem-ref ptr1 512)))
    (is (== value (mem-ref (shift-pointer 512 ptr1) 0)))
    (is (== value (mem-ref ptr2   0)))
    (is (== value (mem-ref ptr2 512)))
    (is (== value (mem-ref (shift-pointer 512 ptr2) 0)))
    (is (== value (mem-ref ptr3   0)))
    (is (== value (mem-ref ptr3 512)))
    (is (== value (mem-ref (shift-pointer 512 ptr3) 0)))

    ;; Free the buffer.
    (foreign-free ptr1)
    (foreign-free ptr2)
    (foreign-free ptr3))

  (declare test-mem-aref-and-aset!
           ((Eq :T) (SimpleForeignRepr :T) => :T -> Unit))
  (define (test-mem-aref-and-aset! value)
    "Test foreign-alloc-uninitialized, foreign-alloc, mem-aref, mem-aset!, mem-aptr, and mem-acpy, using `value` and three 10-element buffers."

    ;; Allocate the 10-element buffer.
    (let ptr1 = (foreign-alloc-uninitialized 10))
    (let ptr2 = (foreign-alloc-uninitialized 10))

    ;; Insert `value` at the first and sixth positions.
    (mem-aset! ptr1 0 value)
    (mem-aset! ptr1 5 value)

    ;; Copy the first buffer into the second.
    (mem-acpy ptr2 ptr1 10)

    ;; Check that we can read back `value`.
    (is (== value (mem-aref ptr1 0)))
    (is (== value (mem-aref ptr1 5)))
    (is (== value (mem-ref (mem-aptr ptr1 5) 0)))
    (is (== value (mem-aref ptr2 0)))
    (is (== value (mem-aref ptr2 5)))
    (is (== value (mem-ref (mem-aptr ptr2 5) 0)))

    ;; Free the buffers.
    (foreign-free ptr1)
    (foreign-free ptr2)

    ;; Quick test with initialized buffer.
    (let ((ptr (foreign-alloc 10 value)))
      (dotimes (i 10)
        (is (== value (mem-aref ptr i))))
      (foreign-free ptr))))

(define-foreign-struct (Star "STAR.")
  (age 1 UnsignedLong)
  (temperature 1 Float)
  (distance 1 Double))

(define-foreign-struct (Constellation "CONSTELLATION.")
  "A group of stars."
  (stars 1024 Star)
  (population 1 UnsignedInt))

(coalton-toplevel

  (declare test-structs (Unit -> Unit))
  (define (test-structs)
    "Test the struct interface using the Ursa Minor contellation."

    ;; Allocate a buffer with one constellation.
    (let constellations = (foreign-alloc-uninitialized 1))

    ;; Get a pointer to the first constellation (redundant).
    (let ursa-minor = (mem-aptr constellations 0))

    ;; Populate information for two of the main stars, Polaris and
    ;; Pherkab.
    (let ((stars   (slot-pointer ursa-minor constellation.stars))
          (polaris (mem-aptr stars 0))
          (pherkab (mem-aptr stars 2)))
      (slot-set! polaris star.age 70000000)
      (slot-set! polaris star.temperature 7200f0)
      (slot-set! polaris star.distance 430d0)
      (slot-set! pherkab star.age 100000000)
      (slot-set! pherkab star.temperature 8280f0)
      (slot-set! pherkab star.distance 486d0))

    ;; Ursa Minor has seven main stars.
    (slot-set! ursa-minor constellation.population 7)

    ;; Check that we can access the information we provided about the
    ;; two stars.
    (let ((polaris (slot-aptr ursa-minor constellation.stars 0))
          (pherkab (slot-aptr ursa-minor constellation.stars 2)))
      (is (== 70000000  (slot-ref polaris star.age)))
      (is (== 7200f0    (slot-ref polaris star.temperature)))
      (is (== 430d0     (slot-ref polaris star.distance)))
      (is (== 100000000 (slot-ref pherkab star.age)))
      (is (== 8280f0    (slot-ref pherkab star.temperature)))
      (is (== 486d0     (slot-ref pherkab star.distance))))

    (is (== 7 (slot-ref ursa-minor constellation.population)))

    (foreign-free constellations)))

(define-foreign-function (memcpy "memcpy") (Pointer Void)
  "Copy `n` bytes from `src` into `dest` and return `dest`."
  (dest (Pointer Void))
  (src  (Pointer Void))
  (n    Size))

(coalton-toplevel

  (declare test-functions (Unit -> Unit))
  (define (test-functions)
    "Test the function interface using two 20-byte buffers and memcpy."

    ;; Allocate two 20-byte buffers.
    (let this = (foreign-alloc-raw 20))
    (let that = (foreign-alloc-raw 20))

    ;; Populate the first buffer with 16-bit integers from -5
    ;; (inclusive) to 5 (exclusive).
    (rec (populate (UFix -> I16 -> Unit)) ((index 0) (value -5))
      (when (< index 10)
        (mem-aset! (unsafe-reinterpret this) index value)
        (populate (1+ index) (1+ value))))

    ;; Use `memcpy` as defined by `define-foreign-function` to copy
    ;; 20 bytes from `this` to `that`.
    (memcpy that this 20)

    ;; Check that the `memcpy` was successful.
    (rec (validate (UFix -> I16 -> Unit)) ((index 0) (value -5))
      (when (< index 10)
        (is (== value (mem-aref (unsafe-reinterpret that) index)))
        (validate (1+ index) (1+ value))))

    ;; Free the two buffers.
    (foreign-free this)
    (foreign-free that)))

(define-foreign-enum CardSuit
  "The suits present in a standard deck of playing-cards."
  Spades
  Hearts
  Diamonds
  (Clubs 7))

(coalton-toplevel

  (declare test-enums (Unit -> Unit))
  (define (test-enums)
    "Test the enum interface using a playing-card suits."

    ;; Allocate a buffer of 10 suits.
    (let ptr = (foreign-alloc-uninitialized 10))

    ;; Populate a few entries of the buffer.
    (mem-aset! ptr 0 Spades)
    (mem-aset! ptr 3 Hearts)
    (mem-aset! ptr 4 Diamonds)
    (mem-aset! ptr 8 Clubs)

    ;; Check that we can read out the enum from the buffer.
    (is (== Spades   (mem-aref ptr 0)))
    (is (== Hearts   (mem-aref ptr 3)))
    (is (== Diamonds (mem-aref ptr 4)))
    (is (== Clubs    (mem-aref ptr 8)))

    ;; Free the buffer.
    (foreign-free ptr)))

(in-package #:coalton-native-tests)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  ;;
  ;; `uiop:add-package-local-nickname` should work on SBCL, too.
  ;; It works for me with v2.5.8, but it seems to fail in CI using v2.5.11.
  ;;
  #+SBCL (sb-ext:add-package-local-nickname "*" "COALTON-NATIVE-TESTS/CFFI")
  #-SBCL (uiop:add-package-local-nickname "*" "COALTON-NATIVE-TESTS/CFFI"))

(define-test test-cffi-mem-* ()
  
  (*:test-mem-ref-and-set! (the U8 *:maxbound))
  (*:test-mem-ref-and-set! (the U8 *:minbound))
  (*:test-mem-aref-and-aset! (the U8 *:maxbound))
  (*:test-mem-aref-and-aset! (the U8 *:minbound))

  (*:test-mem-ref-and-set! (the I8 *:maxbound))
  (*:test-mem-ref-and-set! (the I8 *:minbound))
  (*:test-mem-aref-and-aset! (the I8 *:maxbound))
  (*:test-mem-aref-and-aset! (the I8 *:minbound))

  (*:test-mem-ref-and-set! (the U16 *:maxbound))
  (*:test-mem-ref-and-set! (the U16 *:minbound))
  (*:test-mem-aref-and-aset! (the U16 *:maxbound))
  (*:test-mem-aref-and-aset! (the U16 *:minbound))

  (*:test-mem-ref-and-set! (the I16 *:maxbound))
  (*:test-mem-ref-and-set! (the I16 *:minbound))
  (*:test-mem-aref-and-aset! (the I16 *:maxbound))
  (*:test-mem-aref-and-aset! (the I16 *:minbound))

  (*:test-mem-ref-and-set! (the U32 *:maxbound))
  (*:test-mem-ref-and-set! (the U32 *:minbound))
  (*:test-mem-aref-and-aset! (the U32 *:maxbound))
  (*:test-mem-aref-and-aset! (the U32 *:minbound))

  (*:test-mem-ref-and-set! (the I32 *:maxbound))
  (*:test-mem-ref-and-set! (the I32 *:minbound))
  (*:test-mem-aref-and-aset! (the I32 *:maxbound))
  (*:test-mem-aref-and-aset! (the I32 *:minbound))

  (*:test-mem-ref-and-set! (the U64 *:maxbound))
  (*:test-mem-ref-and-set! (the U64 *:minbound))
  (*:test-mem-aref-and-aset! (the U64 *:maxbound))
  (*:test-mem-aref-and-aset! (the U64 *:minbound))

  (*:test-mem-ref-and-set! (the I64 *:maxbound))
  (*:test-mem-ref-and-set! (the I64 *:minbound))
  (*:test-mem-aref-and-aset! (the I64 *:maxbound))
  (*:test-mem-aref-and-aset! (the I64 *:minbound))

  (*:test-mem-ref-and-set! (the F32 *:infinity))
  (*:test-mem-ref-and-set! (the F32 (negate *:infinity)))
  (*:test-mem-aref-and-aset! (the F32 *:infinity))
  (*:test-mem-aref-and-aset! (the F32 (negate *:infinity)))

  (*:test-mem-ref-and-set! (the F64 *:infinity))
  (*:test-mem-ref-and-set! (the F64 (negate *:infinity)))
  (*:test-mem-aref-and-aset! (the F64 *:infinity))
  (*:test-mem-aref-and-aset! (the F64 (negate *:infinity))))

(define-test test-cffi-mem-*/complex ()
  (let components = (make-list (negate *:infinity) 0 *:infinity))
  (loops:dolist (value (do (real <- components)
                           (imag <- components)
                           (pure (complex real imag))))
    (*:test-mem-ref-and-set!   (the (Complex F32) value))
    (*:test-mem-aref-and-aset! (the (Complex F32) value))
    (*:test-mem-ref-and-set!   (as (Complex F64) value))
    (*:test-mem-aref-and-aset! (as (Complex F64) value))))

(define-test test-cffi-structs ()
  (*:test-structs))

(define-test test-cffi-functions ()
  (*:test-functions))

(define-test test-cffi-enums ()
  (*:test-enums))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  #+SBCL (sb-ext:remove-package-local-nickname "*")
  #-SBCL (uiop:remove-package-local-nickname "*"))
