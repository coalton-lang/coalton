(coalton-cffi/utils:define-cffi-package #:coalton-cffi/pointers
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-cffi/aliases
   #:coalton-cffi/complex
   #:coalton-cffi/types)
  (:import-from
   #:coalton-library/types
   #:Proxy
   #:proxy-of
   #:proxy-inner)
  (:export
   #:Pointer
   #:null-pointer
   #:null-pointer?
   #:foreign-alloc-raw
   #:foreign-alloc
   #:foreign-alloc-uninitialized
   #:foreign-free
   #:unsafe-reinterpret
   #:shift-pointer
   #:SimpleForeignRepr
   #:mem-ref
   #:mem-set!
   #:mem-aref
   #:mem-aset!
   #:mem-aptr
   #:mem-cpy-raw
   #:mem-cpy
   #:mem-acpy
   #:define-simple-foreign-repr-instance))

(in-package #:coalton-cffi/pointers)

(named-readtables:in-readtable coalton:coalton)

(cffi:defcfun memcpy :pointer
  "Copy `n` bytes from `src` to `dest` and return `dest`."
  (dest :pointer)
  (src  :pointer)
  (n    :size))

(coalton-toplevel

  (repr :native cffi:foreign-pointer)
  (define-type (Pointer :T)
    "A foreign pointer to elements of type `:T`.")

  (inline)
  (declare null-pointer (Unit -> Pointer :T))
  (define (null-pointer)
    "Get a NULL pointer."
    (lisp (Pointer :T) ()
      (cffi:null-pointer)))

  (inline)
  (declare null-pointer? (Pointer :T -> Boolean))
  (define (null-pointer? ptr)
    "Is `ptr` the NULL pointer?"
    (lisp Boolean (ptr)
      (cffi:null-pointer-p ptr)))

  (inline)
  (declare foreign-alloc-raw (Size -> Pointer :T))
  (define (foreign-alloc-raw count)
    "Allocate `count` bytes of memory on the heap and return a pointer to this memory."
    (lisp (Pointer :T) (count)
      (cffi:foreign-alloc ':uint8 :count count)))

  (inline)
  (declare foreign-alloc (ForeignRepr :T => UFix -> :T -> Pointer :T))
  (define (foreign-alloc n x)
    "Allocate memory on the heap with enough space to store `n` elements initialized to `x` and return a pointer to this memory."
    (let ((type (foreign-repr-of x)))
      (lisp (Pointer :T) (type n x)
        (cffi:foreign-alloc type :count n :initial-element x))))

  (inline)
  (declare foreign-alloc-uninitialized (ForeignRepr :T => UFix -> Pointer :T))
  (define (foreign-alloc-uninitialized n)
    "Allocate memory on the heap with enough space to store `n` elements and return a pointer to this memory. The memory is _not_ initialized."
    (%foreign-alloc-uninitialized Proxy n))

  (inline)
  (declare %foreign-alloc-uninitialized
           (ForeignRepr :T => Proxy :T -> UFix -> Pointer :T))
  (define (%foreign-alloc-uninitialized proxy n)
    "A helper function used to define `foreign-alloc-uninitialized`."
    (let ((type (foreign-repr proxy)))
      (lisp (Pointer :T) (type n)
        (cffi:foreign-alloc type :count n))))

  (inline)
  (declare foreign-free (Pointer :T -> Unit))
  (define (foreign-free ptr)
    "Free memory on the heap, pointed to by `ptr`."
    (lisp Unit (ptr)
      (cffi:foreign-free ptr)
      Unit))

  (inline)
  (declare unsafe-reinterpret (Pointer :T -> Pointer :U))
  (define (unsafe-reinterpret ptr)
    "Reinterpret `ptr` as pointing to elements of a different type."
    (lisp (Pointer :U) (ptr)
      ptr))

  (inline)
  (declare shift-pointer (Offset -> Pointer :T -> Pointer :T))
  (define (shift-pointer offset ptr)
    "Shift `ptr` by `offset` bytes."
    (lisp (Pointer :T) (offset ptr)
      (cffi:inc-pointer ptr offset)))

  ;; Originally, I defined the methods below as generic functions.
  ;; The difficully is that `cffi:mem-ref` and `cffi:mem-aref` are
  ;; defined using compiler macros that expand to optimized
  ;; dereferences when the type is known early. As a convenience, a
  ;; macro for defining instances of `SimpleForeignRepr` is provided
  ;; below as `define-simple-foreign-repr-instance`.

  (define-class (ForeignRepr :T => SimpleForeignRepr :T)
    "A class of types that can parametrize pointers that can be directly dereferenced using `cffi:mem-ref`."
    (mem-ref
     "Get the element at `ptr`, offset by `offset` bytes."
     (Pointer :T -> Offset -> :T))
    (mem-set!
     "Set the element at `ptr` to `value`, offset by `offset` bytes."
     (Pointer :T -> Offset -> :T -> Unit))
    (mem-aref
     "Get the `index`th element of `ptr`."
     (Pointer :T -> UFix -> :T))
    (mem-aset!
     "Set the `index`th element of `ptr` to `value`."
     (Pointer :T -> UFix -> :T -> Unit)))

  (inline)
  (declare mem-aptr (ForeignRepr :T => Pointer :T -> UFix -> Pointer :T))
  (define (mem-aptr ptr index)
    "Get a pointer to the `index`th element of `ptr`."
    (let size = (foreign-size (proxy-inner (proxy-of ptr))))
    (lisp (Pointer :T) (ptr index size)
      (cffi:inc-pointer ptr (cl:* index size))))

  (inline)
  (declare mem-cpy-raw
           (ForeignRepr :T => Pointer :T -> Pointer :T -> Size -> Unit))
  (define (mem-cpy-raw dest src n)
    "Copy `n` bytes from `src` into `dest`."
    (lisp Unit (dest src n)
      (memcpy dest src n)
      Unit))

  (inline)
  (declare mem-cpy (ForeignRepr :T => Pointer :T -> Pointer :T -> Unit))
  (define (mem-cpy dest src)
    "Copy the element at `src` into `dest`."
    (let n = (foreign-size (proxy-inner (proxy-of src))))
    (lisp Unit (dest src n)
      (memcpy dest src n)
      Unit))

  (inline)
  (declare mem-acpy
           (ForeignRepr :T => Pointer :T -> Pointer :T -> UFix -> Unit))
  (define (mem-acpy dest src count)
    "Copy `count` elements from `src` into `dest`."
    (let n = (foreign-size (proxy-inner (proxy-of src))))
    (lisp Unit (dest src n count)
      (memcpy dest src (cl:* n count))
      Unit)))

(cl:defmacro define-simple-foreign-repr-instance (type)
  "Define an instance of `SimpleForeignRepr` for `type`.

There must already be an instance of `ForeignRepr` defined for `type`."
  (cl:let ((ptr    (cl:gensym "PTR"))
           (offset (cl:gensym "OFFSET"))
           (index  (cl:gensym "INDEX"))
           (value  (cl:gensym "VALUE"))
           (ctype  (coalton-type-to-foreign-type type)))
    `(coalton-toplevel
       (define-instance (SimpleForeignRepr ,type)
         (inline)
         (define (mem-ref ,ptr ,offset)
           (lisp ,type (,ptr ,offset)
             (cffi:mem-ref ,ptr ',ctype ,offset)))
         (inline)
         (define (mem-set! ,ptr ,offset ,value)
           (lisp Unit (,ptr ,offset ,value)
             (cl:setf (cffi:mem-ref ,ptr ',ctype ,offset) ,value)
             Unit))
         (inline)
         (define (mem-aref ,ptr ,index)
           (lisp ,type (,ptr ,index)
             (cffi:mem-aref ,ptr ',ctype ,index)))
         (inline)
         (define (mem-aset! ,ptr ,index ,value)
           (lisp Unit (,ptr ,index ,value)
             (cl:setf (cffi:mem-aref ,ptr ',ctype ,index) ,value)
             Unit))))))

(define-foreign-repr-instance (Pointer :T) :pointer)
(define-simple-foreign-repr-instance (Pointer :T))

(define-simple-foreign-repr-instance U8)
(define-simple-foreign-repr-instance U16)
(define-simple-foreign-repr-instance U32)
(define-simple-foreign-repr-instance U64)
(define-simple-foreign-repr-instance I8)
(define-simple-foreign-repr-instance I16)
(define-simple-foreign-repr-instance I32)
(define-simple-foreign-repr-instance I64)

(define-simple-foreign-repr-instance String)
(define-simple-foreign-repr-instance Boolean)

(define-simple-foreign-repr-instance (Complex F32))
(define-simple-foreign-repr-instance (Complex F64))
