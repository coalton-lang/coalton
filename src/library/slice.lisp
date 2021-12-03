(in-package #:coalton-library)

(coalton-toplevel
  ;;
  ;; Slice
  ;;

  (define-type (Slice :a)
    (Slice Lisp-Object))

  (declare make-slice (Integer -> Integer -> (Vector :a) -> (Slice :a)))
  (define (make-slice start length v)
    (progn
      (when (< start 0)
        (error "Start of slice cannot be less than 0."))

      (when (<= length 0)
        (error "Length of slice cannot be equal to or less than 0."))

      (let end = (+ start length))
      (when (> end (vector-length v))
        (error "Slice cannot extend beyond length of backing vector."))

      (match v
        ((Vector v)
         (lisp (Slice :a) (v start length)
           (Slice (cl:make-array
                   length
                   :displaced-to v
                   :displaced-index-offset start)))))))

  (declare slice-length ((Slice :a) -> Integer))
  (define (slice-length s)
    "Returns the length of S"
    (match s
      ((Slice s)
       (lisp Integer (s)
         (cl:array-dimension s 0)))))

  (declare slice-copy ((Slice :a) -> (Slice :a)))
  (define (slice-copy s)
    "Returns a new slice containg the same elements as S"
    (match s
      ((Slice s)
       (lisp (Slice :a) (s)
         (Slice (alexandria:copy-array s))))))

  (declare slice-set (Integer -> :a -> (Slice :a) -> Unit))
  (define (slice-set index item s)
    "Set the element at INDEX in S to ITEM"
    (match s
      ((Slice s)
       (progn
         (lisp Lisp-Object (index item s)
           (cl:setf (cl:aref s index) item))
         Unit))))

  (declare slice-index (Integer -> (Slice :a) -> (Optional :a)))
  (define (slice-index index s)
    "Lookup the element at INDEX in S"
    (if (>= index (slice-length s))
        None
        (Some (slice-index-unsafe index s))))

  (declare slice-index-unsafe (Integer -> (Slice :a) -> :a))
  (define (slice-index-unsafe index s)
    "Lookup the element at INDEX in S without bounds checking"
    (match s
      ((Slice s)
       (lisp :a (index s)
         (cl:aref s index)))))

  (declare slice-foreach ((:a -> :b) -> (Slice :a) -> Unit))
  (define (slice-foreach f s)
    "Call the function F once for each item in S"
    (match s
      ((Slice s)
       (progn
         (lisp Lisp-Object (f s)
           (cl:loop :for elem :across s
              :do (coalton-impl/codegen::A1 f elem)))
         Unit))))

  (declare slice-foreach-index ((Integer -> :a -> :b) -> (Slice :a) -> Unit))
  (define (slice-foreach-index f s)
    "Call the function F once for each item in S with its index"
    (match s
      ((Slice s)
       (progn
         (lisp Lisp-Object (f s)
           (cl:loop
              :for elem :across s
              :for i :from 0
              :do (coalton-impl/codegen::A2 f i elem)))
         Unit))))

  (declare slice-foreach2 ((:a -> :b -> :c) -> (Slice :a) -> (Slice :b) -> Unit))
  (define (slice-foreach2 f s1 s2)
    "Iterate over S1 and S2 calling F once on each iteration"
    (match (Tuple s1 s2)
      ((Tuple (Slice s1) (Slice s2))
       (progn
         (lisp Lisp-Object (f s1 s2)
           (cl:loop
              :for e1 :across s1
              :for e2 :across s2
              :do (coalton-impl/codegen::A2 f e1 e2)))
         Unit))))

  ;;
  ;; Vector functions
  ;;

  (declare vector-sliding (((Slice :a) -> :b) -> Integer -> (Vector :a) -> Unit))
  (define (vector-sliding f size v)
    "Sliding iteration over a vector"
    (let ((inner
            (fn (offset)
              (if (> (+ offset size) (vector-length v))
                  Unit
                  (progn
                    (let s = (make-slice offset size v))
                    (f s)
                    (inner (+ offset 1)))))))
      (inner 0)))

  (declare vector-chunked (((Slice :a) -> :b) -> Integer -> (Vector :a) -> Unit))
  (define (vector-chunked f size v)
    "Chunked iteration over a vector. Ignores elements at the end if the vector does not evenly divide by the chunk size."
    (let ((inner
            (fn (offset)
              (if (> (+ offset size) (vector-length v))
                  Unit
                  (progn
                    (let s = (make-slice offset size v))
                    (f s)
                    (inner (+ offset size)))))))
      (inner 0)))

  ;;
  ;; Instances
  ;;

  (define-instance (Eq :a => (Eq (Slice :a)))
    (define (== s1 s2)
      (if (/= (slice-length s1) (slice-length s2))
          False
          (progn
            (let out = (make-cell True))
            (slice-foreach2
             (fn (e1 e2)
               (unless (== e1 e2)
                 (cell-write False out)))
             s1 s2)
            (cell-read out))))
    (define (/= s1 s2)
      (not (== s1 s2))))

  (define-instance (Into (Slice :a) (Vector :a))
    (define (into s)
      (progn
        (let v = (make-vector-capacity (slice-length s)))
        (slice-foreach
         (fn (x)
           (vector-push x v))
         s)
        v)))

  (define-instance (Into (Vector :a) (Slice :a))
    (define (into v)
      (make-slice 0 (vector-length v) v)))

  (define-instance (Iso (Slice :a) (Vector :a))))
