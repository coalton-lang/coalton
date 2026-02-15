(coalton/utils:defstdlib-package #:coalton/slice
  (:use
   #:coalton
   #:coalton/builtin
   #:coalton/functions
   #:coalton/classes
   #:coalton/math)
  (:local-nicknames
   (#:types #:coalton/types)
   (#:cell #:coalton/cell)
   (#:iter #:coalton/iterator)
   (#:list #:coalton/list)
   (#:vector #:coalton/vector))
  (:shadowing-import-from #:coalton/vector #:Vector)
  (:export
   #:Slice
   #:new
   #:length
   #:set!
   #:index
   #:index-unsafe
   #:iter-sliding
   #:iter-chunked
   #:iter-chunked-exact))

(in-package #:coalton/slice)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;;
  ;; Slice
  ;;

  (repr :native (cl:vector cl:t))
  (define-type (Slice :a))

  (define-class (Sliceable :a)
    (%length (:a -> UFix)))

  (define-instance (Sliceable (Vector :a))
    (inline)
    (define %length vector:length))

  (define-instance (Sliceable (Slice :a))
    (inline)
    (define %length length))

  (declare new ((Sliceable (:b :a)) => UFix -> UFix -> :b :a -> Slice :a))
  (define (new start len v)
    "Create a new slice backed by `v` starting at index `start` and continuing for `len` elements."
    (when (< start 0)
      (error "Start of slice cannot be less than 0."))

    (when (<= len 0)
      (error "Length of slice cannot be equal to or less than 0."))

    (let end = (+ start len))
    (when (> end (%length v))
      (error "Slice cannot extend beyond length of backing vector."))

    (lisp (Slice :a) (v start len)
      (cl:make-array
       len
       :element-type cl:t
       :displaced-to v
       :displaced-index-offset start)))

  (inline)
  (declare length (Slice :a -> UFix))
  (define (length s)
    "Returns the length of `s`."
    (lisp UFix (s)
      (cl:array-dimension s 0)))

  (inline)
  (declare set! (UFix -> :a -> (Slice :a) -> Unit))
  (define (set! idx item s)
    "Set the element at index `idx` in `s` to `item`."
    (lisp :a (idx item s)
      (cl:setf (cl:aref s idx) item))
    Unit)

  (declare index (UFix -> (Slice :a) -> (Optional :a)))
  (define (index idx s)
    "Lookup the element at `index` in `s`."
    (if (>= idx (length s))
        None
        (Some (index-unsafe idx s))))

  (inline)
  (declare index-unsafe (UFix -> (Slice :a) -> :a))
  (define (index-unsafe idx s)
    "Lookup the element at `index` in `s` without bounds checking."
    (lisp :a (idx s)
      (cl:aref s idx)))

  (declare iter-sliding ((Sliceable (:b :a)) => UFix -> :b :a -> iter:Iterator (Slice :a)))
  (define (iter-sliding size s)
    "Returns an iterator that yields a series of overlapping slices of length `size`."
    (let length = (%length s))
    (let offset_ = (cell:new 0))
    (iter:with-size 
        (fn ()
          (let offset = (cell:read offset_))
          (when (> (+ offset size) length)
            (return None))

          (cell:increment! offset_)
          (Some (new offset size s)))
      (if (> size length)
          0
          (- (+ length 1) size))))

  (declare iter-chunked ((Sliceable (:b :a)) => UFix -> :b :a -> iter:Iterator (Slice :a)))
  (define (iter-chunked size s)
    "Divide `s` into a series of slices of length `size`. Will return a final shorter slice if `s` does not divide evenly."
    (let length = (%length s))
    (let offset_ = (cell:new 0))
    (iter:with-size
        (fn ()
          (let offset = (cell:read offset_))
          (when (>= offset length)
            (return None))

          (when (> (+ offset size) length)
            (let remaining = (- length offset))
            (cell:update! (+ size) offset_)
            (return (Some (new offset remaining s))))

          (cell:update! (+ size) offset_)
          (Some (new offset size s)))
      (cond
        ;; If size is greater than length the iterator is empty
        ((> size length) 0)
        ;; If size evenly divides length
        ((zero? (mod length size))
         (div length size))
        ;; If there is a final shorter slice
        (True
         (+ 1 (div length size))))))

  (declare iter-chunked-exact ((Sliceable (:b :a)) => UFix -> :b :a -> iter:Iterator (Slice :a)))
  (define (iter-chunked-exact size s)
    "Divide `s` into a series of slices of length `size`. Will skip trailing elements if `s` does not divide evenly."
    (let length = (%length s))
    (let offset_ = (cell:new 0))
    (iter:with-size
      (fn ()
        (let offset = (cell:read offset_))
        (when (> (+ offset size) length)
          (return None))

        (cell:update! (+ size) offset_)
        (Some (new offset size s)))
      (div length size)))

  ;;
  ;; Instances
  ;;

  (define-instance (iter:IntoIterator (Slice :a) :a)
    (define (iter:into-iter s)
      (let idx = (cell:new 0))
      (iter:with-size
        (fn ()
          (let res = (index (cell:read idx) s))
          (cell:increment! idx)
          res)
        (length s))))

  (define-instance (iter:FromIterator (Slice :a) :a)
    (define (iter:collect! iter)
      ;; NOTE: This will create a non displaced array. It should be
      ;; fine, because it isn't observable with the slice API.
      (let vec = (vector:with-capacity (with-default 0 (iter:size-hint iter))))
      (vector:extend! vec iter)
      (lisp (Slice :a) (vec) vec)))

  (define-instance (Eq :a => Eq (Slice :a))
    (define (== s1 s2)
      (if (/= (length s1) (length s2))
          False
          (iter:every! id (iter:zip-with! == (iter:into-iter s1) (iter:into-iter s2))))))

  (define-instance (Foldable Slice)
    (define (fold f init s)
      (lisp :a (f init s)
        (cl:reduce
         (cl:lambda (b a)
           (call-coalton-function f b a))
         s
         :initial-value init)))
    (define (foldr f init s)
      (lisp :a (f init s)
        (cl:reduce
         (cl:lambda (a b)
           (call-coalton-function f a b))
         s
         :initial-value init
         :from-end cl:t))))

  (define-instance (Into (Slice :a) (Vector :a))
    (inline)
    (define (into s)
      (let v = (vector:with-capacity (length s)))
      (vector:extend! v (iter:into-iter s))
      v))

  (define-instance (Into (Vector :a) (Slice :a))
    (inline)
    (define (into v)
      (new 0 (vector:length v) v)))

  (define-instance (Iso (Slice :a) (Vector :a))))

#+sb-package-locks
(sb-ext:lock-package "COALTON/SLICE")
