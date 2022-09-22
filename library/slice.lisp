(coalton-library/utils:defstdlib-package #:coalton-library/slice
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/functions
   #:coalton-library/classes)
  (:local-nicknames
   (#:types #:coalton-library/types)
   (#:list #:coalton-library/list)
   (#:cell #:coalton-library/cell)
   (#:vector #:coalton-library/vector)
   (#:addr #:coalton-library/addressable))
  (:shadowing-import-from #:coalton-library/vector #:Vector)
  (:export
   #:Slice
   #:new
   #:length
   #:copy
   #:set!
   #:index
   #:index-unsafe
   #:foreach
   #:foreach-index
   #:foreach2
   #:iter-sliding
   #:iter-chunked))

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(in-package #:coalton-library/slice)

(coalton-toplevel
  ;;
  ;; Slice
  ;;

  (repr :native (cl:vector cl:t))
  (define-type (Slice :a))

  (declare new (UFix -> UFix -> (Vector :a) -> (Slice :a)))
  (define (new start length v)
    (when (< start 0)
      (error "Start of slice cannot be less than 0."))

    (when (<= length 0)
      (error "Length of slice cannot be equal to or less than 0."))

    (let end = (+ start length))
    (when (> end (vector:length v))
      (error "Slice cannot extend beyond length of backing vector."))

    (lisp (Slice :a) (v start length)
      (cl:make-array
       length
       :displaced-to v
       :displaced-index-offset start)))

  (declare length ((Slice :a) -> UFix))
  (define (length s)
    "Returns the length of S"
    (lisp UFix (s)
      (cl:array-dimension s 0)))

  (declare copy ((Slice :a) -> (Slice :a)))
  (define (copy s)
    "Returns a new slice containg the same elements as S"
    (lisp (Slice :a) (s)
      (alexandria:copy-array s)))

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

  (declare foreach ((:a -> :b) -> (Slice :a) -> Unit))
  (define (foreach f s)
    "Call the function F once for each item in S"
    (lisp :a (f s)
      (cl:loop :for elem :across s
         :do (call-coalton-function f elem)))
    Unit)

  (declare foreach-index ((UFix -> :a -> :b) -> (Slice :a) -> Unit))
  (define (foreach-index f s)
    "Call the function F once for each item in S with its index"
    (lisp :a (f s)
      (cl:loop
         :for elem :across s
         :for i :from 0
         :do (call-coalton-function f i elem)))
    Unit)

  (declare foreach2 ((:a -> :b -> :c) -> (Slice :a) -> (Slice :b) -> Unit))
  (define (foreach2 f s1 s2)
    "Iterate over S1 and S2 calling F once on each iteration"
    (lisp :a (f s1 s2)
      (cl:loop
         :for e1 :across s1
         :for e2 :across s2
         :do (call-coalton-function f e1 e2)))
    Unit)

  ;;
  ;; Vector functions
  ;;

  (declare iter-sliding (((Slice :a) -> :b) -> UFix -> (Vector :a) -> Unit))
  (define (iter-sliding f size v)
    "Sliding iteration over a vector"
    (let ((inner
            (fn (offset)
              (if (> (+ offset size) (vector:length v))
                  Unit
                  (progn
                    (let s = (new offset size v))
                    (f s)
                    (inner (+ offset 1)))))))
      (inner 0)))

  (declare iter-chunked (((Slice :a) -> :b) -> UFix -> (Vector :a) -> Unit))
  (define (iter-chunked f size v)
    "Chunked iteration over a vector. Ignores elements at the end if the vector does not evenly divide by the chunk size."
    (let ((inner
            (fn (offset)
              (if (> (+ offset size) (vector:length v))
                  Unit
                  (progn
                    (let s = (new offset size v))
                    (f s)
                    (inner (+ offset size)))))))
      (inner 0)))

  ;;
  ;; Instances
  ;;

  (define-instance (Eq :a => (Eq (Slice :a)))
    (define (== s1 s2)
      (if (/= (length s1) (length s2))
          False
          (progn
            (let out = (cell:new True))
            (foreach2
             (fn (e1 e2)
               (unless (== e1 e2)
                 (cell:write! out False)
                 Unit))
             s1 s2)
            (cell:read out)))))

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
      (foreach
       (fn (x)
         (vector:push! x v))
       s)
      v))

  (define-instance (Into (Vector :a) (Slice :a))
    (define (into v)
      (new 0 (vector:length v) v)))

  (define-instance (types:RuntimeRepr :a => Iso (Slice :a) (Vector :a)))

  (define-instance (addr:Addressable (Slice :elt))
    (define addr:eq? addr::unsafe-internal-eq?)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/SLICE")
