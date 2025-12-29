(coalton-cffi/utils:define-cffi-package #:coalton-cffi/boxes
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-cffi/pointers)
  (:export
   #:Box
   #:box
   #:unbox
   #:BoxedPointer
   #:box-pointer
   #:unbox-pointer))

(in-package #:coalton-cffi/boxes)

(named-readtables:in-readtable coalton:coalton)

;;; At times, one may wish to integrate foreign objects with the
;;; garbage collector. The types below, `Box` and `BoxedPointer`, are
;;; useful for placing pointers (and other objects, like file
;;; descriptors) in containers that guarantee heap allocation.
;;; Finalizers can be attached to these containers, such as using
;;; `trivial-garbage`
;;; (https://github.com/trivial-garbage/trivial-garbage), to free
;;; pointers, close file descriptors, or anything else that should
;;; happen when the contained item is out of scope.

(coalton-toplevel

  (repr :lisp)
  (define-type (Box :T)
    "A simple container.

This type is defined using `(repr :lisp)` to ensure heap-allocation. This is especially useful for attaching finalizers when interacting with foreign objects."
    (%Box :T))

  (inline)
  (declare box (:T -> Box :T))
  (define (box item)
    "Put `item` into a box."
    (%Box item))

  (inline)
  (declare unbox (Box :T -> :T))
  (define (unbox box)
    "Take an item out of `box`."
    (match box
      ((%Box item)
       item))))

(coalton-toplevel

  (repr :transparent)
  (define-type (BoxedPointer :T)
    "A container for a pointer that guarantees heap-allocation. This is especially useful for attaching finalizers to a pointer, such as to free a pointer when its container is garbage collected."
    (%BoxedPointer (Box (Pointer :T))))

  (inline)
  (declare box-pointer (Pointer :T -> BoxedPointer :T))
  (define (box-pointer ptr)
    "Put `ptr` into a box."
    (%BoxedPointer (%Box ptr)))

  (inline)
  (declare unbox-pointer (BoxedPointer :T -> Pointer :T))
  (define (unbox-pointer box)
    "Take a pointer out of `box`."
    (match box
      ((%BoxedPointer box)
       (unbox box)))))
