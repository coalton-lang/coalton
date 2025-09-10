(coalton-library/utils:defstdlib-package #:coalton-library/collections/mutable/vector
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/functions
   #:coalton-library/classes)
  (:local-nicknames
   (#:cln #:coalton-library/collections/classes)
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
  (:export
   #:Vector
   #:with-capacity
   #:singleton
   #:capacity
   #:singleton?
   #:set-capacity!
   #:ensure-capacity!
   #:clear!
   #:index
   #:index-unsafe
   #:remove-at!
   #:remove-at-unsafe!
   #:extend!
   #:append
   #:swap-remove!
   #:swap-remove-unsafe!
   #:sort-by!
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

  (declare with-capacity (UFix -> Vector :a))
  (define (with-capacity n)
    "Create a new vector with `n` elements preallocated."
    (lisp (Vector :a) (n)
      (cl:make-array n :fill-pointer 0 :adjustable cl:t :element-type cl:t)))
  
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
  
  (declare copy (Vector :a -> Vector :a))
  (define (copy v)
    "Return a new vector containing the same elements as `v`."
    (lisp (Vector :a) (v)
      ;; We use COPY-ARRAY and not COPY-SEQ to get identical
      ;; adjustable properties.
      (alexandria:copy-array v)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                       ;;;
  ;;; Collections API Methods (Shouldn't be exported)                       ;;;
  ;;;                                                                       ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;
  ;;; Collection Vector
  ;;;

  (declare new (Unit -> Vector :a))
  (define (new _)
    "Create a new empty vector"
    (with-capacity 0))

  (declare with-initial-element (UFix -> :a -> Vector :a))
  (define (with-initial-element n x)
    "Create a new vector with `n` elements equal to `x`."
    (let v = (with-capacity n))
    (extend! v (iter:repeat-for x n))
    v)

  (declare empty? (Vector :a -> Boolean))
  (define (empty? v)
    "Is `v` empty?"
    (== 0 (length v)))
  
  (declare length (Vector :a -> UFix))
  (define (length v)
    "Returns the length of `v`."
    (lisp UFix (v)
      (cl:length v)))
  
  (declare contains-elt? (Eq :a => :a -> Vector :a -> Boolean))
  (define (contains-elt? elt vec)
    (opt:some? (find-elem elt vec)))

  (declare contains-where? ((:a -> Boolean) -> Vector :a -> Boolean))
  (define (contains-where? f vec)
    (for x in vec
      (when (f x)
        (return True)))
    False)

  ;;;
  ;;; LinearCollection Vector
  ;;;

  (declare head-unsafe (Vector :a -> :a))
  (define (head-unsafe v)
    "Return the first item of `v` without first checking if `v` is empty."
    (index-unsafe 0 v))

  (declare last-unsafe (Vector :a -> :a))
  (define (last-unsafe v)
    "Return the last element of `v` without first checking if `v` is empty."
    (index-unsafe (- (length v) 1) v))

  (declare find-elem (Eq :a => :a -> Vector :a -> Optional UFix))
  (define (find-elem e v)
    "Find the index of element `e` in `v`."
    (let ((test (fn (elem)
                  (== elem e))))

      (lisp (Optional UFix) (v test)
        (cl:let ((pos (cl:position-if
                       (cl:lambda (x)
                         (cl:eq cl:t (call-coalton-function test x)))
                       v)))
          (cl:if pos
                 (Some pos)
                 None)))))
  
  (declare find-where ((:a -> Boolean) -> Vector :a -> Optional UFix))
  (define (find-where pred vec)
    "Find the index of the first element matching `pred` in `vec`."
    (lisp (Optional UFix) (pred vec)
      (cl:let ((pos (cl:position-if
                      (cl:lambda (x)
                        (cl:eq cl:t (call-coalton-function pred x)))
                      vec)))
        (cl:if pos
               (Some pos)
               None))))
  
  (declare find ((:a -> Boolean) -> Vector :a -> Optional :a))
  (define (find pred vec)
    "Find the first element matching `pred` in `vec`."
    (for x in vec
      (when (pred x)
        (return (Some x))))
    None)
 
  (declare indices-where ((:a -> Boolean) -> Vector :a -> List UFix))
  (define (indices-where f vec)
    (let ((indices (cell:new Nil)))
      (for i in (iter:range-increasing 1 0 (length vec))
        (when (f (index-unsafe i vec))
          (cell:push! indices i)
          Unit))
      (cln:reverse (cell:read indices))))

  (declare subseq-vec (UFix -> UFix -> Vector :a -> Vector :a))
  (define (subseq-vec start end vec)
    (if (or (>= start end)
            (>= start (length vec)))
      (new)
      (let ((end-point (min end (length vec)))
            (new-length (- end-point start))
            (new-vec (with-capacity new-length)))
        (for i in (iter:range-increasing 1 start end-point)
          (push! (index-unsafe i vec) new-vec))
        new-vec)))
  
  (declare split-at-vec (UFix -> Vector :a -> Tuple (Vector :a) (Vector :a)))
  (define (split-at-vec i vec)
    (Tuple (subseq-vec 0 i vec)
           (subseq-vec (+ 1 i) (length vec) vec)))
  
  (declare split-elt-vec (Eq :a => :a -> Vector :a -> List (Vector :a)))
  (define (split-elt-vec elt vec)
    (split-where-vec (== elt) vec))
  
  ;; TODO: Someday when the List API has a more efficient way of building the result
  ;; list up, so that we don't have to call reverse, use that.
  (declare split-where-vec ((:a -> Boolean) -> Vector :a -> List (Vector :a)))
  (define (split-where-vec pred vec)
    (let ((results (cell:new Nil))
          (indices (cln:push-end (length vec)
                                 (indices-where pred vec))))
      (if (== 1 (cln:length indices))
        (make-list (copy vec))
        (progn
          (cell:push! results (subseq-vec 0 (list:nth 0 indices) vec))
          (for i in (iter:range-increasing 1 1 (cln:length indices))
            (cell:push! results (subseq-vec (+ 1 (list:nth (- i 1) indices))
                                            (list:nth i indices)
                                            vec)))
          (cln:reverse (cell:read results))))))

  (declare zip-itr (iter:IntoIterator :m :b => Vector :a -> :m -> Vector (Tuple :a :b)))
  (define (zip-itr vec col)
    (zip-with-itr Tuple vec col))

  (declare zip-with-itr (iter:IntoIterator :m :b => (:a -> :b -> :c) -> Vector :a -> :m -> Vector :c))
  (define (zip-with-itr f vec col)
    (let ((it (iter:into-iter col))
          (size-guess (min (length vec)
                           (with-default (length vec) (iter:size-hint it))))
          (ret (with-capacity size-guess))
          (i (cell:new 0)))
      (while-let (Tuple (Some a) (Some b)) = 
                  (Tuple (index (cell:read i) vec) (iter:next! it))
        (push! (f a b) ret)
        (cell:increment! i))
      ret))

  ;;;
  ;;; MutableCollection Vector
  ;;;
  
  (declare reverse! (Vector :a -> Vector :a))
  (define (reverse! vec)
    "Reverse a vector in place and return the vector for convenience."
    (lisp :x (vec)
      (cl:dotimes (i (cl:floor (cl:/ (cl:length vec) 2)))
        (cl:rotatef (cl:aref vec i)
                    (cl:aref vec (cl:- (cl:length vec) i 1)))))
    vec)
  
  (declare sort! (Ord :a => Vector :a -> Unit))
  (define (sort! v)
    "Sort a vector in-place in ascending order."
    (sort-by! < v))
  
  (declare push! (:a -> Vector :a -> UFix))
  (define (push! item v)
    "Append `item` to `v` and resize `v` if necessary, returning the index of the new item."
    (lisp UFix (item v)
      (cl:vector-push-extend item v)))

  (declare pop-unsafe! (Vector :a -> :a))
  (define (pop-unsafe! v)
    "Remove and return the first item of `v` without checking if the vector is empty."
    (remove-at-unsafe! 0 v))

  (declare pop-end-unsafe! (Vector :a -> :a))
  (define (pop-end-unsafe! v)
    "Remove and return the last item of `v` without checking if the vector is empty."
    (lisp :a (v)
      (cl:vector-pop v)))

  (declare insert-at! (UFix -> :a -> Vector :a -> Vector :a))
  (define (insert-at! idx item v)
    "Insert `item` into `v` at index `idx`, shifting the existing elements starting at `idx` right by 1."
    (cond
     ((empty? v)
      (push! item v)
      v)
     (True
      (let idx-safe = (max 0 (min (length v) idx)))
      (ensure-capacity! (m-ath:1+ (length v)) v)
      (push! (last-unsafe v) v)
      (for i
 in (iter:range-decreasing 1 (length v) (m-ath:1+ idx-safe))
        (set! i (index-unsafe (m-ath:1- i) v) v))
      (set! idx-safe item v)
      v)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                       ;;;
  ;;; Extra Public Functions                                                ;;;
  ;;;                                                                       ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (declare singleton (:a -> Vector :a))
  (define (singleton x)
    "Create a new vector with a single element equal to `x`"
    (with-initial-element 1 x))

  (declare singleton? (Vector :a -> Boolean))
  (define (singleton? v)
    "Is `v` a singleton?"
    (== 1 (length v)))

  (declare clear! (Vector :a -> Unit))
  (define (clear! v)
    "Set the capacity of `v` to `0`."
    (set-capacity! 0 v))
    
  (declare index (UFix -> Vector :a -> Optional :a))
  (define (index index v)
    "Return the `index`th element of `v`."
    (if (>= index (length v))
        None
        (Some (index-unsafe index v))))

  (declare index-unsafe (UFix -> Vector :a -> :a))
  (define (index-unsafe idx v)
    "Return the `idx`th element of `v` without checking if the element exists."
    (lisp :a (idx v)
      (cl:aref v idx)))

  (declare set! (UFix -> :a -> Vector :a -> Unit))
  (define (set! idx item v)
    "Set the `idx`th element of `v` to `item`. This function left intentionally unsafe because it does not have a return value to check."
    (lisp Void (idx item v)
      (cl:setf (cl:aref v idx) item))
    Unit)
  
  (declare remove-at! (UFix -> Vector :a -> Optional :a))
  (define (remove-at! idx v)
    "Remove and return item at index `idx` in `v`, shifting the existing elements after `idx` left by 1."
    (match (index idx v)
      ((None) None)
      ((Some result)
       (for i in (iter:range-increasing 1 idx (m-ath:1- (length v)))
         (set! i (index-unsafe (m-ath:1+ i) v) v))
       (pop-end-unsafe! v)
       (Some result))))
  
  (declare remove-at-unsafe! (UFix -> Vector :a -> :a))
  (define (remove-at-unsafe! idx v)
    "Remove and return item at index `idx` in `v`, shifting the existing elements after `idx` left by 1. Error if vector is empty."
    (let result = (index-unsafe idx v))
    (for i in (iter:range-increasing 1 idx (m-ath:1- (length v)))
      (set! i (index-unsafe (m-ath:1+ i) v) v))
    (pop-end-unsafe! v)
    result)

  (declare append (Vector :a -> Vector :a -> Vector :a))
  (define (append v1 v2)
    "Create a new vector containing the elements of `v1` followed by the elements of `v2`."
    (let out = (with-capacity (+ (length v1) (length v2))))
    (extend! out v1)
    (extend! out v2)
    out)

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
        (pop-end-unsafe! vec)
        (progn
          (let out = (index-unsafe idx vec))
          (set! idx (pop-end-unsafe! vec) vec)
          out)))

  (declare sort-by! ((:a -> :a -> Boolean) -> Vector :a -> Unit))
  (define (sort-by! f v)
    "Sort a vector in-place with predicate function `f`."
    (lisp Void (v f)
      (cl:sort
       v
       (cl:lambda (a b)
         (call-coalton-function f a b))))
    Unit)

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
       (push! x vec)
       Unit)
     iter)
    Unit)

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
    (define mempty (new)))

  (define-instance (Semigroup (Vector :a))
    (inline)
    (define <> append))

  (define-instance (Functor Vector)
    (define (map f v)
      (let out = (with-capacity (length v)))
      (iter:for-each!
       (fn (x)
         (push! (f x) out)
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
      (with-initial-element n x))
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
      (index-unsafe n a))
    (inline)
    (define (ram:unsafe-set! a n x)
      (set! n x a)))

  (define-instance (Into (List :a) (Vector :a))
    (define (into lst)
      (let ((out (with-capacity (cln:length lst)))
            (inner
              (fn (lst)
                (match lst
                  ((Cons x xs)
                   (progn
                     (push! x out)
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
            (let res = (index (cell:read idx) vec))
            (cell:increment! idx)
            res)
        (length vec))))

  (define-instance (iter:FromIterator (Vector :a) :a)
    (define (iter:collect! iter)
      (let size = (with-default 0 (iter:size-hint iter)))
      (let vec = (with-capacity size))
      (iter:for-each! (fn (x)
                        (push! x vec)
                        Unit)
                      iter)
      vec))

  (define-instance (Default (Vector :a))
    (inline)
    (define default new)))

;;
;; Collections Instances
;;

(coalton-toplevel
  (define-instance (cln:Collection (Vector :a) :a)
    (define cln:new-collection new)
    (define cln:new-repeat with-initial-element)
    (define (cln:new-from n f)
      (let ((vec (with-capacity n)))
        (for i in (iter:range-increasing 1 0 n)
          (push! (f i) vec))
        vec))
    (define (cln:new-convert coll)
      (iter:collect! (iter:into-iter coll)))
    (define (cln:filter f vec)
      (iter:collect! (iter:filter! f (iter:into-iter vec))))
    (define (cln:remove-duplicates vec)
      (if (cln:empty? vec)
        vec
        (let ((res (with-capacity (length vec))))
          (for i in (list:range 0 (- (length vec) 1))
            (let elt = (index-unsafe i vec))
            (unless (contains-elt? elt res)
              (push! elt res)
              Unit))
          res)))
    (define (cln:remove-elt elt vec)
      (iter:collect! (iter:filter! (/= elt) (iter:into-iter vec))))
    (define cln:empty? empty?)
    (define cln:size length)
    (define cln:contains-where? contains-where?)
    (define (cln:count-where f vec)
      (fold (fn (sum elt)
              (if (f elt)
                  (+ sum 1)
                  sum))
            0
            vec))
    (define (cln:add elt vec)
      (let ((res (copy vec)))
        (push! elt res)
        res)))

  (define-instance (cln:MutableCollection (Vector :a) :a)
    (define cln:copy copy)
    (define (cln:add! elt vec)
      (push! elt vec)
      vec))
  
  (define-instance (cln:LinearCollection (Vector :a) :a)
    (define cln:head# head-unsafe)
    (define cln:last# last-unsafe)
    (define (cln:tail vec)
      (subseq-vec 1 (length vec) vec))
    (define (cln:drop n vec)
      (subseq-vec n (length vec) vec))
    (define (cln:take n vec)
      (subseq-vec 0 n vec))
    (define cln:index-where find-where)
    (define cln:find-where find)
    (define (cln:indices-elt elt vec)
      (indices-where (== elt) vec))
    (define cln:indices-where indices-where)
    (define cln:subseq subseq-vec)
    (define cln:split-at split-at-vec)
    (define cln:split-elt split-elt-vec)
    (define cln:split-where split-where-vec)
    (define (cln:reverse vec)
      (reverse! (copy vec)))
    (define (cln:sort vec)
      (let ((result (copy vec)))
        (sort! result)
        result))
    (define (cln:sort-with ord-func vec)
      (let ((result (copy vec)))
        (sort-by! (fn (a b) (== LT (ord-func a b)))
                  result)
        result))
    ;; (define cln:zip zip-itr)
    ;; (define cln:zip-with zip-with-itr)
    (define (cln:push elt vec)
      (cond
        ((empty? vec)
         (push! elt vec)
         vec)
        (True
         (insert-at! 0 elt (copy vec)))))
    (define (cln:push-end elt vec)
      (let ((result (copy vec)))
        (push! elt result)
        result))
    (define (cln:insert-at i elt vec)
      (insert-at! i elt (copy vec)))
    (define (cln:set-at i elt vec)
      (let ((result (copy vec)))
        (set! i elt result)
        result)))

  (define-instance (cln:MutableLinearCollection (Vector :a) :a)
    (define cln:reverse! reverse!)
    (define (cln:sort! vec)
      (sort! vec)
      vec)
    (define (cln:sort-with! ord-func vec)
      (sort-by! (fn (a b) (== LT (ord-func a b)))
                vec)
      vec)
    (define (cln:push! elt vec)
      (insert-at! 0 elt vec))
    (define (cln:push-end! elt vec)
      (push! elt vec)
      vec)
    (define cln:pop!# pop-unsafe!)
    (define cln:pop-end!# pop-end-unsafe!)
    (define cln:insert-at! insert-at!)
    (define (cln:set-at! i elt vec)
      (set! i elt vec)
      vec)))

(cl:defmacro make (cl:&rest elements)
  "Construct a `Vector' containing the ELEMENTS, in the order listed."
  (cl:let* ((length (cl:length elements))
            (vec (cl:gensym "VEC-")))
    `(progn
       (let ,vec = (with-capacity ,length))
       ,@(cl:loop :for elt :in elements
            :collect `(coalton-library/collections/classes:push-end! ,elt ,vec))
       ,vec)))

; #+sb-package-locks
; (sb-ext:lock-package "COALTON-LIBRARY/COLLECTIONS/MUTABLE/VECTOR")
