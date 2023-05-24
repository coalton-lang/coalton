(coalton-library/utils:defstdlib-package #:coalton-library/slice
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/functions
   #:coalton-library/classes
   #:coalton-library/math/arith
   #:coalton-library/math/integral)
  (:local-nicknames
   (#:types #:coalton-library/types)
   (#:cell #:coalton-library/cell)
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:vector #:coalton-library/vector))
  (:shadowing-import-from #:coalton-library/vector #:Vector)
  (:export
   #:Slice
   #:new
   #:length
   #:element-type
   #:set!
   #:index
   #:index-unsafe
   #:iter-sliding
   #:iter-chunked
   #:iter-chunked-exact))

(in-package #:coalton-library/slice)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;;
  ;; Slice
  ;;

  (repr :native (cl:and (cl:vector cl:*) (cl:not cl:simple-array)))
  (define-type (Slice :a))

  (define-class (Sliceable :a)
    (%length (:a -> UFix)))

  (define-instance (Sliceable (Vector :a))
    (define %length vector:length))

  (define-instance (Sliceable (Slice :a))
    (define %length length))

  (declare new ((types:RuntimeRepr :a) (Sliceable (:b :a)) => UFix -> UFix -> :b :a -> Slice :a))
  (define (new start length v)
    "Create a new slice backed by V starting at index START and continuing for LENGTH elements."
    (when (< start 0)
      (error "Start of slice cannot be less than 0."))

    (when (<= length 0)
      (error "Length of slice cannot be equal to or less than 0."))

    (let end = (+ start length))
    (when (> end (%length v))
      (error "Slice cannot extend beyond length of backing vector."))

    (let p = types:Proxy)
    (let p_ = (types:proxy-inner p))
    (let t = (types:runtime-repr p_))

    (types:as-proxy-of
     (lisp (Slice :a) (v start length t)
       (cl:make-array
        length
        :element-type t
        :displaced-to v
        :displaced-index-offset start))
     p))

  (declare length (Slice :a -> UFix))
  (define (length s)
    "Returns the length of S"
    (lisp UFix (s)
      (cl:array-dimension s 0)))

  (declare element-type (Slice :a -> types:LispType))
  (define (element-type s)
    "Returns the element type of S as a LispType"
    (lisp types:LispType (s)
      (cl:array-element-type s)))

  (declare set! (UFix -> :a -> (Slice :a) -> Unit))
  (define (set! index item s)
    "Set the element at INDEX in S to ITEM"
    (lisp :a (index item s)
      (cl:setf (cl:aref s index) item))
    Unit)

  (declare index (UFix -> (Slice :a) -> (Optional :a)))
  (define (index idx s)
    "Lookup the element at INDEX in S"
    (if (>= idx (length s))
        None
        (Some (index-unsafe idx s))))

  (declare index-unsafe (UFix -> (Slice :a) -> :a))
  (define (index-unsafe idx s)
    "Lookup the element at INDEX in S without bounds checking"
    (lisp :a (idx s)
      (cl:aref s idx)))

  (declare iter-sliding ((types:RuntimeRepr :a) (Sliceable (:b :a)) => UFix -> :b :a -> iter:Iterator (Slice :a)))
  (define (iter-sliding size s)
    "Returns an iterator that yeilds a series of overlapping slices of length SIZE."
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

  (declare iter-chunked ((types:RuntimeRepr :a) (Sliceable (:b :a)) => UFix -> :b :a -> iter:Iterator (Slice :a)))
  (define (iter-chunked size s)
    "Divide S into a series of slices of length SIZE. Will return a final shorter slice if S does not divide evenly."
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

  (declare iter-chunked-exact ((types:RuntimeRepr :a) (Sliceable (:b :a)) => UFix -> :b :a -> iter:Iterator (Slice :a)))
  (define (iter-chunked-exact size s)
    "Divide S into a series of slices of length SIZE. Will skip trailing elements if S does not divide evenly."
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

  (define-instance (types:RuntimeRepr :a => iter:FromIterator (Slice :a) :a)
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

  (define-instance (types:RuntimeRepr :a => Into (Slice :a) (Vector :a))
    (define (into s)
      (let v = (vector:with-capacity (length s)))
      (vector:extend! v (iter:into-iter s))
      v))

  (define-instance (types:RuntimeRepr :a => Into (Vector :a) (Slice :a))
    (define (into v)
      (new 0 (vector:length v) v)))

  (define-instance (types:RuntimeRepr :a => Iso (Slice :a) (Vector :a))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/SLICE")
