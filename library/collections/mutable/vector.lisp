(coalton-library/utils:defstdlib-package #:coalton-library/collections/mutable/vector
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/functions
   #:coalton-library/classes)
  (:local-nicknames
   (#:opt #:coalton-library/optional)
   (#:types #:coalton-library/types)
   (#:list #:coalton-library/collections/immutable/list)
   (#:cell #:coalton-library/cell)
   (#:iter #:coalton-library/iterator)
   (#:ram #:coalton-library/randomaccess)
   (#:mth #:coalton-library/math/real)
   (#:m-ath #:coalton-library/math/arith)
   (#:m-int #:coalton-library/math/integral)
   (#:m-elm #:coalton-library/math/elementary))
  (:import-from #:coalton-library/math/arith
   #:1+
   #:1-)
  (:export
   ;; Standard Collections Functions
   #:new-collection
   #:new-repeat
   #:new-from
   #:new-convert
   #:filter
   #:remove-duplicates
   #:remove-elt
   #:empty?
   #:size
   #:contains-elt?
   #:contains-where?
   #:count-where
   #:add

   #:copy
   #:filter!
   #:remove-duplicates!
   #:remove-elt!
   #:add!

   #:head
   #:head#
   #:last
   #:last#
   #:tail
   #:at
   #:at#
   #:take
   #:drop
   #:length
   #:index-elt
   #:index-elt#
   #:index-where
   #:index-where#
   #:find-where
   #:indices-elt
   #:indices-where
   #:subseq
   #:split-at
   #:split-elt
   #:split-where
   #:reverse
   #:sort
   #:sort-by
   #:zip
   #:zip-with
   #:push
   #:push-end
   #:insert-at
   #:remove-at
   #:remove-at#
   #:set-at

   #:reverse!
   #:sort!
   #:sort-by!
   #:push!
   #:push-end!
   #:pop!
   #:pop!#
   #:pop-end!
   #:pop-end!#
   #:insert-at!
   #:remove-at!
   #:remove-at!#
   #:set-at!

   ;; Vector Specific
   #:Vector
   #:with-capacity
   #:singleton
   #:capacity
   #:singleton?
   #:set-capacity!
   #:ensure-capacity!
   #:clear!
   #:extend!
   #:append
   #:swap-remove!
   #:swap-remove-unsafe!
   #:resect
   #:make))

(in-package #:coalton-library/collections/mutable/vector)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                       ;;;
  ;;; Datatype Definition & Internal Helper Functions                       ;;;
  ;;;                                                                       ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;
  ;; Vector
  ;;

  (repr :native (cl:and (cl:vector cl:t) (cl:not cl:simple-vector)))
  (define-type (Vector :a))

  (inline)
  (declare with-capacity (UFix -> Vector :a))
  (define (with-capacity n)
    "Create a new vector with `n` elements preallocated."
    (lisp (Vector :a) (n)
      (cl:make-array n :fill-pointer 0 :adjustable cl:t :element-type cl:t)))

  (inline)
  (declare capacity (Vector :a -> UFix))
  (define (capacity v)
    "Returns the number of elements that `v` can store without resizing."
    (lisp UFix (v)
      (cl:array-dimension v 0)))
  
  (declare set-capacity! (UFix -> Vector :a -> Unit))
  (define (set-capacity! new-capacity v)
    "Set the capacity of `v` to `new-capacity`. Setting the capacity to lower then the length will remove elements from the end."
    (let shrinking = (< new-capacity (length v)))
    (lisp Unit (v shrinking new-capacity)
      ;; If the array is getting larger then don't change the
      ;; fill pointer
      (cl:adjust-array v new-capacity :fill-pointer shrinking)
      Unit))

  (declare ensure-capacity! (UFix -> Vector :a -> Unit))
  (define (ensure-capacity! ensured-capacity v)
    "Ensure that `v` has at least `ensured-capacity` capacity. If not, doubles the capacity until the threshold is reached. (Only sets the capacity once for efficiency.)"
    (when (< (capacity v) ensured-capacity)
      (let ((a (into (capacity v)))
            (b (into ensured-capacity))
            (k (max 0 (mth:ceiling (m-elm:log 2 (mth:inexact/ b a)))))
            (new-capacity (* a (mth:round (m-int:^ 2 k)))))
        (set-capacity! 
              ;; TODO: Figure out how to actually convert an Integer into a UFix...
              (lisp UFix (new-capacity)
                new-capacity)
              v))))

  (inline)
  (declare %push-vector! (:a -> Vector :a -> UFix))
  (define (%push-vector! item v)
    "Append `item` to `v` and resize `v` if necessary, returning the index of the new item."
    (lisp UFix (item v)
      (cl:vector-push-extend item v)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                       ;;;
  ;;; Collections API Methods                                               ;;;
  ;;;                                                                       ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;
  ;;; Collection Vector
  ;;;

  (inline)
  (declare new-collection (Unit -> Vector :a))
  (define (new-collection)
    "Create a new empty vector"
    (with-capacity 0))

  (inline)
  (declare new-repeat (UFix -> :a -> Vector :a))
  (define (new-repeat n x)
    "Create a new vector with `n` elements equal to `x`."
    (let v = (with-capacity n))
    (extend! v (iter:repeat-for x n))
    v)

  (inline)
  (declare new-from (UFix -> (UFix -> :a) -> Vector :a))
  (define (new-from n f)
    (let ((vec (with-capacity n)))
      (for i in (iter:range-increasing 1 0 n)
        (%push-vector! (f i) vec))
      vec))

  (inline)
  (declare new-convert (iter:IntoIterator :m :a => :m -> Vector :a))
  (define (new-convert coll)
    (iter:collect! (iter:into-iter coll)))

  (inline)
  (declare filter ((:a -> Boolean) -> Vector :a -> Vector :a))
  (define (filter pred vec)
    "Return a new vector with only elements x of VEC where (`pred` x) == True"
    (iter:collect! (iter:filter! pred (iter:into-iter vec))))

  ;; TODO: Replace with a call to cl:remove-duplicates if possible
  (declare remove-duplicates (Eq :a => Vector :a -> Vector :a))
  (define (remove-duplicates vec)
    (if (empty? vec) vec
        (let ((res (with-capacity (length vec))))
          (for i in (list:range 0 (- (length vec) 1))
            (let elt = (at# i vec))
            (unless (contains-elt? elt res)
              (%push-vector! elt res)
              Unit))
          res)))

  (inline)
  (declare remove-elt (Eq :a => :a -> Vector :a -> Vector :a))
  (define (remove-elt elt vec)
    "Return a copy of `vec` with all instances of `elt` removed."
    (filter (/= elt) vec))

  (inline)
  (declare empty? (Vector :a -> Boolean))
  (define (empty? v)
    "Is `v` empty?"
    (== 0 (size v)))

  (inline)
  (declare size (Vector :a -> UFix))
  (define (size v)
    "Returns the length of `v`."
    (lisp UFix (v)
      (cl:length v)))

  (inline)
  (declare contains-elt? (Eq :a => :a -> Vector :a -> Boolean))
  (define (contains-elt? elt vec)
    (contains-where? (== elt) vec))

  (inline)
  (declare contains-where? ((:a -> Boolean) -> Vector :a -> Boolean))
  (define (contains-where? f vec)
    (for x in vec
      (when (f x)
        (return True)))
    False)

  (declare count-where? ((:a -> Boolean) -> Vector :a -> UFix))
  (define (count-where? f vec)
    (fold (fn (sum elt)
            (if (f elt)
                (+ sum 1)
                sum))
          0
          vec))

  (inline)
  (declare add (:a -> Vector :a -> Vector :a))
  (define (add elt vec)
    "Return a new copy of `vec` with `elt` added to it."
    (let ((res (copy vec)))
      (%push-vector! elt res)
      res))

  ;;;
  ;;; MutableCollection Vector
  ;;;

  (inline)
  (declare copy (Vector :a -> Vector :a))
  (define (copy v)
    "Return a new vector containing the same elements as `v`."
    (lisp (Vector :a) (v)
      ;; We use COPY-ARRAY and not COPY-SEQ to get identical
      ;; adjustable properties.
      (alexandria:copy-array v)))

  (declare filter! ((:a -> Boolean) -> Vector :a -> Vector :a))
  (define (filter! f vec)
    "Remove elements not satisfying the predicate function."
    (lisp (Vector :a) (f vec)
      (cl:delete-if-not (cl:lambda (x)
                          (call-coalton-function f x))
                        vec)))

  ;; TODO: Replace with a call to cl:delete-duplicates if possible
  (inline)
  (declare remove-duplicates! (Eq :a => Vector :a -> Vector :a))
  (define (remove-duplicates! vec)
    (let cleared = (remove-duplicates vec))
    (clear! vec)
    (extend! vec cleared)
    vec)

  (inline)
  (declare remove-elt! (Eq :a => :a -> Vector :a -> Vector :a))
  (define (remove-elt! elt vec)
    "Remove all copies of `elt` from `vec`. Returns `vec` for convenience."
    (filter! (/= elt) vec))

  (inline)
  (declare add! (:a -> Vector :a -> Vector :a))
  (define (add! elt vec)
    "Add `elt` to `vec`. Returns `vec` for convenience."
    (%push-vector! elt vec)
    vec)

  ;;;
  ;;; LinearCollection Vector
  ;;;

  (inline)
  (declare head (Vector :a -> Optional :a))
  (define (head v)
    (at 0 v))

  (inline)
  (declare head# (Vector :a -> :a))
  (define (head# v)
    "Return the first item of `v` without first checking if `v` is empty."
    (at# 0 v))

  (inline)
  (declare last (Vector :a -> Optional :a))
  (define (last v)
    (at (1- (length v)) v))

  (inline)
  (declare last# (Vector :a -> :a))
  (define (last# v)
    "Return the last element of `v` without first checking if `v` is empty."
    (at# (- (length v) 1) v))

  (inline)
  (declare tail (Vector :a -> Vector :a))
  (define (tail vec)
    "Return a copy of `vec` without the first element."
    (subseq 1 (length vec) vec))

  (inline)
  (declare at (UFix -> Vector :a -> Optional :a))
  (define (at index v)
    "Return the `index`th element of `v`."
    (if (>= index (length v))
        None
        (Some (at# index v))))

  (inline)
  (declare at# (UFix -> Vector :a -> :a))
  (define (at# idx v)
    "Return the `idx`th element of `v` without checking if the element exists."
    (lisp :a (idx v)
      (cl:aref v idx)))

  (inline)
  (declare take (UFix -> Vector :a -> Vector :a))
  (define (take n vec)
    "Return a copy of `vec` with only the first `n` elements."
    (subseq 0 n vec))

  (inline)
  (declare drop (UFix -> Vector :a -> Vector :a))
  (define (drop n vec)
    "Return a copy of `vec` with the first `n` elements removed."
    (subseq n (length vec) vec))

  (inline)
  (declare length (Vector :a -> UFix))
  (define length size)

  (inline)
  (declare index-elt (Eq :a => :a -> Vector :a -> Optional UFix))
  (define (index-elt elt coll)
     "Return the index of the first occurence of `elt`, if it can be found."
    (index-where (== elt) coll))

  (inline)
  (declare index-elt# (Eq :a => :a -> Vector :a -> UFix))
  (define (index-elt# elt coll)
     "Return the index of the first occurence of `elt`, erroring if it cannot be found."
    (opt:from-some "Cannot find element in collection." (index-elt elt coll)))

  (declare index-where ((:a -> Boolean) -> Vector :a -> Optional UFix))
  (define (index-where pred vec)
    "Find the index of the first element matching `pred` in `vec`."
    (lisp (Optional UFix) (pred vec)
      (cl:let ((pos (cl:position-if
                      (cl:lambda (x)
                        (cl:eq cl:t (call-coalton-function pred x)))
                      vec)))
        (cl:if pos
               (Some pos)
               None))))

  (inline)
  (declare index-where# ((:a -> Boolean) -> Vector :a -> UFix))
  (define (index-where# f coll)
     "Return the index of the first element matching a predicate function, erroring if none can be found."
    (opt:from-some "Cannot find matching element in collection." (inline (index-where f coll))))

  (declare find-where ((:a -> Boolean) -> Vector :a -> Optional :a))
  (define (find-where pred vec)
    "Find the first element matching `pred` in `vec`."
    (for x in vec
      (when (pred x)
        (return (Some x))))
    None)

  (inline)
  (declare indices-elt (Eq :a => :a -> Vector :a -> List UFix))
  (define (indices-elt elt vec)
    (indices-where (== elt) vec))

  (declare indices-where ((:a -> Boolean) -> Vector :a -> List UFix))
  (define (indices-where f vec)
    (let ((indices (cell:new Nil)))
      (for i in (iter:range-increasing 1 0 (length vec))
        (when (f (at# i vec))
          (cell:push! indices i)
          Unit))
      (list:reverse (cell:read indices))))

  (inline)
  (declare subseq (UFix -> UFix -> Vector :a -> Vector :a))
  (define (subseq start end v)
    "Compute a subseq of a vector bounded by given indices.

`start` index is inclusive and `end` index is exclusive."
    (let ((real-start (min start end))
          (real-end (min (length v) (max start end))))
      (lisp (Vector :a) (real-start real-end v)
        (cl:let ((result (cl:make-array (cl:- real-end real-start) :adjustable cl:t)))
          (cl:replace result v :start2 real-start)))))

  (inline)
  (declare split-at (UFix -> Vector :a -> Tuple (Vector :a) (Vector :a)))
  (define (split-at i vec)
    (Tuple (subseq 0 i vec)
           (subseq (+ 1 i) (length vec) vec)))

  (inline)
  (declare split-elt (Eq :a => :a -> Vector :a -> List (Vector :a)))
  (define (split-elt elt vec)
    (split-where (== elt) vec))

  (declare split-where ((:a -> Boolean) -> Vector :a -> List (Vector :a)))
  (define (split-where pred vec)
    (let ((results (cell:new Nil))
          (indices (list:push-end (length vec)
                                  (indices-where pred vec))))
      (if (== 1 (list:length indices))
        (make-list (copy vec))
        (progn
          (cell:push! results (subseq 0 (list:at# 0 indices) vec))
          (for i in (iter:range-increasing 1 1 (list:length indices))
            (cell:push! results (subseq (+ 1 (list:at# (- i 1) indices))
                                            (list:at# i indices)
                                            vec)))
          (list:reverse (cell:read results))))))

  (inline)
  (declare reverse (Vector :a -> Vector :a))
  (define (reverse vec)
    "Return a reversed copy of `vec`."
    (reverse! (copy vec)))

  (inline)
  (declare sort (Ord :a => Vector :a -> Vector :a))
  (define (sort vec)
    "Return a sorted copy of `vec`."
    (let ((result (copy vec)))
      (sort! result)
      result))

  (inline)
  (declare sort-by ((:a -> :a -> Ord) -> Vector :a -> Vector :a))
  (define (sort-by ord-func vec)
    (let ((result (copy vec)))
      (sort-by! (fn (a b) (== LT (ord-func a b)))
                result)
      result))

  (inline)
  (declare zip (iter:IntoIterator :m :b => Vector :a -> :m -> Vector (Tuple :a :b)))
  (define (zip vec col)
    (zip-with Tuple vec col))

  (declare zip-with (iter:IntoIterator :m :b => (:a -> :b -> :c) -> Vector :a -> :m -> Vector :c))
  (define (zip-with f vec col)
    (let ((it (iter:into-iter col))
          (size-guess (min (length vec)
                           (with-default (length vec) (iter:size-hint it))))
          (ret (with-capacity size-guess))
          (i (cell:new 0)))
      (while-let (Tuple (Some a) (Some b)) =
          (Tuple (at (cell:read i) vec) (iter:next! it))
        (%push-vector! (f a b) ret)
        (cell:increment! i))
      ret))

  (declare push (:a -> Vector :a -> Vector :a))
  (define (push elt vec)
    "Return a copy of `vec` with `elt` inserted at the front."
    (let ((result (with-capacity (max (capacity vec)
                                      (1+ (length vec))))))
      (set! 0 elt result)
      (for i in (iter:range-increasing 1 1 (1+ (length vec)))
        (set! i (at (1- i) vec) result))
      result))

  (declare push-end (:a -> Vector :a -> Vector :a))
  (define (push-end elt vec)
    "Return a copy of `vec` with `elt` inserted at the end."
    (let ((result (with-capacity (max (capacity vec)
                                      (1+ (length vec))))))
      (for i in (iter:range-increasing 1 0 (length vec))
        (set! i (at i vec) result))
      (set! (length vec) elt result)
      result))

  (declare insert-at (UFix -> :a -> Vector :a -> Vector :a))
  (define (insert-at i elt vec)
    "Return a copy of `vec` with `elt` inserted at `i`."
    (let ((result (with-capacity (max (capacity vec)
                                      (1+ (length vec))))))
      (for j in (iter:range-increasing 1 0 i)
        (set! j (at j vec) result))
      (set! i elt result)
      (for j in (iter:range-increasing 1 (1+ i) (1+ (length vec)))
        (set! j (at (1- j) vec) result))
      result))

  (declare remove-at (UFix -> Vector :a -> Optional (Tuple :a (Vector :a))))
  (define (remove-at i vec)
    "Return a copy of `vec` with the element at `i` removed."
    (if (>= i (length vec))
        None
        (let ((result (with-capacity (capacity vec))))
          (for j in (iter:range-increasing 1 0 i)
               (set! j (at j vec) result))
          (for j in (iter:range-increasing 1 (1+ i) (1+ (length vec)))
               (set! j (at (1- j) vec) result))
          (Some (Tuple (at# i vec) result)))))

  (inline)
  (declare remove-at# (UFix -> Vector :a -> Tuple :a (Vector :a)))
  (define (remove-at# i vec)
    "Return a copy of `vec` with the element at `i` removed."
    (opt:from-some "Vector index out of bounds." (remove-at i vec)))

  (inline)
  (declare set-at (UFix -> :a -> Vector :a -> Vector :a))
  (define (set-at i elt vec)
    "Return a copy of `vec` with the element at `i` set to `elt`."
    (let ((result (copy vec)))
      (set! i elt result)
      result))

  ;;;
  ;;; MutableLinearCollection Vector
  ;;;
  
  (declare reverse! (Vector :a -> Vector :a))
  (define (reverse! vec)
    "Reverse a vector in place and return the vector for convenience."
    (lisp :x (vec)
      (cl:dotimes (i (cl:floor (cl:/ (cl:length vec) 2)))
        (cl:rotatef (cl:aref vec i)
                    (cl:aref vec (cl:- (cl:length vec) i 1)))))
    vec)

  (inline)
  (declare sort! (Ord :a => Vector :a -> Unit))
  (define (sort! v)
    "Sort a vector in-place in ascending order."
    (sort-by! < v))

  (declare sort-by! ((:a -> :a -> Boolean) -> Vector :a -> Unit))
  (define (sort-by! f v)
    "Sort a vector in-place with predicate function `f`."
    (lisp Void (v f)
      (cl:sort
       v
       (cl:lambda (a b)
         (call-coalton-function f a b))))
    Unit)

  (declare push! (:a -> Vector :a -> Vector :a))
  (define (push! elt vec)
    "Push `elt` onto the beginning of `vec`. Return `vec` for convenienc."
      (cond
        ((empty? vec)
         (%push-vector! elt vec)
         vec)
        (True
         (insert-at! 0 elt (copy vec)))))

  (inline)
  (declare push-end! (:a -> Vector :a -> Vector :a))
  (define (push-end! elt vec)
    (%push-vector! elt vec)
    vec)

  (inline)
  (declare pop! (Vector :a -> Optional :a))
  (define (pop! vec)
    (if (empty? vec)
        None
        (Some (pop!# vec))))

  (inline)
  (declare pop!# (Vector :a -> :a))
  (define (pop!# v)
    "Remove and return the first item of `v` without checking if the vector is empty."
    (remove-at!# 0 v))

  (inline)
  (declare pop-end! (Vector :a -> Optional :a))
  (define (pop-end! vec)
    (if (empty? vec)
        None
        (Some (pop-end!# vec))))

  (inline)
  (declare pop-end!# (Vector :a -> :a))
  (define (pop-end!# v)
    "Remove and return the last item of `v` without checking if the vector is empty."
    (lisp :a (v)
      (cl:vector-pop v)))

  (declare insert-at! (UFix -> :a -> Vector :a -> Vector :a))
  (define (insert-at! idx item v)
    "Insert `item` into `v` at index `idx`, shifting the existing elements starting at `idx` right by 1."
    (cond
     ((empty? v)
      (%push-vector! item v)
      v)
     (True
      (let idx-safe = (max 0 (min (length v) idx)))
      (ensure-capacity! (m-ath:1+ (length v)) v)
      (%push-vector! (last# v) v)
      (for i in (iter:range-decreasing 1 (length v) (m-ath:1+ idx-safe))
        (set! i (at# (m-ath:1- i) v) v))
      (set! idx-safe item v)
      v)))

  (declare remove-at! (UFix -> Vector :a -> Optional :a))
  (define (remove-at! idx v)
    "Remove and return item at index `idx` in `v`, shifting the existing elements after `idx` left by 1."
    (match (at idx v)
      ((None) None)
      ((Some result)
       (for i in (iter:range-increasing 1 idx (m-ath:1- (length v)))
         (set! i (at# (m-ath:1+ i) v) v))
       (pop-end!# v)
       (Some result))))

  (declare remove-at!# (UFix -> Vector :a -> :a))
  (define (remove-at!# idx v)
    "Remove and return item at index `idx` in `v`, shifting the existing elements after `idx` left by 1. Error if vector is empty."
    (let result = (at# idx v))
    (for i in (iter:range-increasing 1 idx (m-ath:1- (length v)))
      (set! i (at# (m-ath:1+ i) v) v))
    (pop-end!# v)
    result)

  (inline)
  (declare set-at! (UFix -> :a -> Vector :a -> Vector :a))
  (define (set-at! i elt v)
    "Set element at `i` in `v` to `elt`. Return `v` for convenience."
    (set! i elt v)
    v)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                       ;;;
  ;;; Extra Public Functions                                                ;;;
  ;;;                                                                       ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (inline)
  (declare singleton (:a -> Vector :a))
  (define (singleton x)
    "Create a new vector with a single element equal to `x`"
    (new-repeat 1 x))

  (inline)
  (declare singleton? (Vector :a -> Boolean))
  (define (singleton? v)
    "Is `v` a singleton?"
    (== 1 (length v)))

  (inline)
  (declare clear! (Vector :a -> Unit))
  (define (clear! v)
    "Set the capacity of `v` to `0`."
    (set-capacity! 0 v))

  (inline)
  (declare set! (UFix -> :a -> Vector :a -> Unit))
  (define (set! idx item v)
    "Set the `idx`th element of `v` to `item`. This function left intentionally unsafe because it does not have a return value to check."
    (lisp Void (idx item v)
      (cl:setf (cl:aref v idx) item))
    Unit)
  
  (declare remove-and-get-at (UFix -> Vector :a -> Optional (Tuple :a (Vector :a))))
  (define (remove-and-get-at idx v)
    (if (>= idx (length v))
        None
        (let ((result (copy v))
              (elt (remove-at!# idx result)))
          (Some (Tuple elt result)))))

  (inline)
  (declare append (Vector :a -> Vector :a -> Vector :a))
  (define (append v1 v2)
    "Create a new vector containing the elements of `v1` followed by the elements of `v2`."
    (let out = (with-capacity (+ (length v1) (length v2))))
    (extend! out v1)
    (extend! out v2)
    out)

  (inline)
  (declare swap-remove! (UFix -> Vector :a -> Optional :a))
  (define (swap-remove! idx vec)
    "Remove the element `idx` from `vec` and replace it with the last element in `vec`. Then return the removed element."
    (if (>= idx (length vec))
        None
        (Some (swap-remove-unsafe! idx vec))))

  (declare swap-remove-unsafe! (UFix -> Vector :a -> :a))
  (define (swap-remove-unsafe! idx vec)
    "Remove the element `idx` from `vec` and replace it with the last element in `vec` without bounds checking. Then return the removed element."
    (if (== (+ 1 idx) (length vec))
        (pop-end!# vec)
        (progn
          (let out = (at# idx vec))
          (set! idx (pop-end!# vec) vec)
          out)))

  (declare extend! (iter:IntoIterator :container :elt => Vector :elt -> :container -> Unit))
  (define (extend! vec iter)
    "Push every element in `iter` to the end of `vec`."
    (let iter = (iter:into-iter iter))

    ;; If the iterator is known to require more capacity then vec has,
    ;; resize before pushing elements
    (let size = (with-default 0 (iter:size-hint iter)))
    (let remaining-capacity = (- (capacity vec) (length vec)))
    (when (> size remaining-capacity)
      (set-capacity! (- size remaining-capacity) vec))

    (iter:for-each!
     (fn (x)
       (%push-vector! x vec)
       Unit)
     iter)
    Unit)

  (inline)
  (declare resect! (Vector :a -> UFix -> UFix -> Unit))
  (define (resect! v start end)
    "Destructively kills a subsequence in a vector bounded by given indices.

`start` index is inclusive and `end` index is exclusive."
    (let ((real-start (min start end))
          (real-end (min (length v) (max start end)))
          (new-size (- (length v) (- real-end real-start))))
      (lisp (Vector :a) (real-start real-end v)
        (cl:replace v v :start1 real-start :start2 real-end))
      (set-capacity! new-size v)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                       ;;;
  ;;; Instances                                                             ;;;
  ;;;                                                                       ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-instance (Eq :a => Eq (Vector :a))
    (define (== v1 v2)
      (if (/= (length v1) (length v2))
          False
          (iter:every! id (iter:zip-with! == (iter:into-iter v1) (iter:into-iter v2))))))

  (define-instance (Monoid (Vector :a))
    (inline)
    (define mempty (new-collection)))

  (define-instance (Semigroup (Vector :a))
    (inline)
    (define <> append))

  (define-instance (Functor Vector)
    (define (map f v)
      (let out = (with-capacity (length v)))
      (iter:for-each!
       (fn (x)
         (%push-vector! (f x) out)
         Unit)
       (iter:into-iter v))
      out))

  (define-instance (Foldable Vector)
    (define (fold f init vec)
      (lisp :a (f init vec)
        (cl:reduce
         (cl:lambda (b a)
           (call-coalton-function f b a))
         vec
         :initial-value init)))
    (define (foldr f init vec)
      (lisp :a (f init vec)
        (cl:reduce
         (cl:lambda (a b)
           (call-coalton-function f a b))
         vec
         :initial-value init
         :from-end cl:t))))

  (define-instance (ram:RandomAccess (Vector :t) :t)
    (inline)
    (define (ram:make n x)
      (new-repeat n x))
    (inline)
    (define (ram:make-uninitialized n)
      (with-capacity n))
    (inline)
    (define (ram:length a)
      (length a))
    (inline)
    (define (ram:readable? _)
      True)
    (inline)
    (define (ram:writable? _)
      True)
    (inline)
    (define (ram:unsafe-aref a n)
      (at# n a))
    (inline)
    (define (ram:unsafe-set! a n x)
      (set! n x a)))

  (define-instance (Into (List :a) (Vector :a))
    (define (into lst)
      (let ((out (with-capacity (list:length lst)))
            (inner
              (fn (lst)
                (match lst
                  ((Cons x xs)
                   (progn
                     (%push-vector! x out)
                     (inner xs)))
                  ((Nil) Unit)))))
        (progn
          (inner lst)
          out))))

  (define-instance (Into (Vector :a) (List :a))
    (inline)
    (define (into v)
      (iter:collect! (iter:into-iter v))))

  (define-instance (Iso (Vector :a) (List :a)))

  (define-instance (iter:IntoIterator (Vector :a) :a)
    (define (iter:into-iter vec)
      (let idx = (cell:new 0))
      (iter:with-size
          (fn ()
            (let res = (at (cell:read idx) vec))
            (cell:increment! idx)
            res)
        (length vec))))

  (define-instance (iter:FromIterator (Vector :a) :a)
    (define (iter:collect! iter)
      (let size = (with-default 0 (iter:size-hint iter)))
      (let vec = (with-capacity size))
      (iter:for-each! (fn (x)
                        (%push-vector! x vec)
                        Unit)
                      iter)
      vec))

  (define-instance (Default (Vector :a))
    (inline)
    (define default new-collection))
  )

(cl:defmacro make (cl:&rest elements)
  "Construct a `Vector` containing the ELEMENTS, in the order listed."
  (cl:let* ((length (cl:length elements))
            (vec (cl:gensym "VEC-")))
    `(progn
       (let ,vec = (with-capacity ,length))
       ,@(cl:loop :for elt :in elements
            :collect `(%push-vector! ,elt ,vec))
       ,vec)))

; #+sb-package-locks
; (sb-ext:lock-package "COALTON-LIBRARY/COLLECTIONS/MUTABLE/VECTOR")
