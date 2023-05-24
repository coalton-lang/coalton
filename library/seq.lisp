;; TODO
;;
;; - consistency of PUSH?

(coalton-library/utils:defstdlib-package #:coalton-library/seq
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/functions
   #:coalton-library/classes)
  (:local-nicknames
   (#:pvec #:coalton/persistent-vector)
   (#:math #:coalton-library/math/arith)
   (#:list #:coalton-library/list)
   (#:cell #:coalton-library/cell)
   (#:iter #:coalton-library/iterator))
  (:export
   #:Seq
   #:new
   #:length
   #:empty?
   #:index
   #:index-unsafe
   #:head
   #:head-unsafe
   #:last
   #:last-unsafe
   #:set
   #:push
   #:pop
   #:pop-unsafe
   #:prune
   ;; TODO: find-elem
   ;; TODO: swap
   ;; TODO: swap-remove
   ;; TODO: swap-remove-unsafe
   ;; TODO: sort
   ;; TODO: sort-by
   #:subseq
   #:extend
   #:append
   #:copy
   #:make
   ))

(in-package #:coalton-library/seq)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (repr :native pvec:persistent-vector)
  (define-type (Seq :a)
    "An immutable vector-like sequence data structure with efficient random access, random modification, pushing, popping, and subsequence. New seqs usually efficiently share memory with existing seqs.")

  (declare new (Unit -> (Seq :a)))
  (define (new)
    "Return an empty seq."
    (lisp (Seq :a) ()
      (pvec:persistent-vector)))

  (declare length ((Seq :a) -> UFix))
  (define (length v)
    "Compute the length of a seq `v`."
    (lisp UFix (v)
      (pvec:length v)))

  (declare empty? ((Seq :a) -> Boolean))
  (define (empty? v)
    "Is the seq `v` empty?"
    (math:zero? (length v)))

  (declare index-unsafe (UFix -> (Seq :a) -> :a))
  (define (index-unsafe i v)
    "Access the `i`th element of the seq `v`. The consequences are undefined if `i` is out of range."
    (lisp :a (i v)
      (pvec:get v i)))

  (declare index (UFix -> (Seq :a) -> (Optional :a)))
  (define (index i v)
    "Access the `i`th element of the seq `v`. Return an optional."
    (if (< i (length v))
        (Some (index-unsafe i v))
        None))

  (declare head ((Seq :a) -> (Optional :a)))
  (define (head v)
    "Returns the first element of the seq `v`."
    (if (math:zero? (length v))
        None
        (Some (index-unsafe 0 v))))

  (declare head-unsafe ((Seq :a) -> :a))
  (define (head-unsafe v)
    "Returns the first element of the seq `v`. The consequences are undefined if `v` is empty."
    (index-unsafe 0 v))

  (declare last ((Seq :a) -> (Optional :a)))
  (define (last v)
    "Returns the last element of the seq `v`."
    (let n = (length v))
    (if (math:zero? n)
        None
        (Some (index-unsafe (math:1- n) v))))

  (declare last-unsafe ((Seq :a) -> :a))
  (define (last-unsafe v)
    "Returns the last element of the seq `v`. The consequences are undefined if `v` is empty."
    (index-unsafe (math:1- (length v)) v))

  (declare set ((Seq :a) -> UFix -> :a -> (Seq :a)))
  (define (set v i x)
    "\"Set\" the `i`th element of the seq `v` to `x`. Returns a structure-sharing copy of `v`."
    (lisp (Seq :a) (v i x)
      (pvec:put v i x)))

  (declare push (:a -> (Seq :a) -> (Seq :a)))
  (define (push x v)
    "\"Push\" `x` onto the end of `v`. Returns a structure-sharing copy of `v`."
    (lisp (Seq :a) (v x)
      (pvec:push-back v x)))

  (declare pop ((Seq :a) -> (Optional (Tuple (Seq :a) :a))))
  (define (pop v)
    "Pop an item off of the back of the seq `v`. Return both the new sequence, as well as the element popped. If `v` is empty, return `None`."
    (if (math:zero? (length v))
        None
        (Some (pop-unsafe v))))

  (declare pop-unsafe ((Seq :a) -> (Tuple (Seq :a) :a)))
  (define (pop-unsafe v)
    "Pop an item off of the back of the seq `v`. Return both the new sequence, as well as the element popped. The consequences are undefined if `v` is empty."
    (lisp (Tuple (Seq :a) :a) (v)
      (cl:multiple-value-bind (new-v popped-elt)
          (pvec:pop-back v)
        (Tuple new-v popped-elt))))

  (declare prune (UFix -> (Seq :a) -> (Seq :a)))
  (define (prune n v)
    "Prune the seq `v` to length `n`, where `n` is no larger than the size of `v`."
    (lisp (Seq :a) (n v)
      (pvec:prune v n)))

  (declare fold-seq ((:b -> :a -> :b) -> :b -> Seq :a -> :b))
  (define (fold-seq f init v)
    (lisp :b (f init v)
      (cl:flet ((cf (x acc)
                  (call-coalton-function f x acc)))
        (cl:declare (cl:dynamic-extent #'cf))
        (pvec:reduce #'cf v :initial-value init))))

  (declare append (Seq :a -> Seq :a -> Seq :a))
  (define (append a b)
    "Append the seq `a` to the seq `b`."
    (fold-seq (flip push) a b))

  (declare subseq ((Seq :a) -> UFix -> UFix -> (Seq :a)))
  (define (subseq v from below)
    "Extract a contiguous subsequence of elements of `v` starting at index `from` up until `below` exclusive.

**Note**: The resulting sequence usually holds on to a reference to `v`. Use `copy`, if needed, to ensure no memory is shared."
    (lisp (Seq :a) (v from below)
      (pvec:subseq v from below)))

  (define-instance (Functor Seq)
    (define (map f x)
      (lisp (Seq :a) (f x)
        (cl:flet ((cf (y)
                    (call-coalton-function f y)))
          (cl:declare (cl:dynamic-extent #'cf))
          (pvec:map #'cf x)))))

  (define-instance (iter:IntoIterator (Seq :a) :a)
    (define (iter:into-iter v)
      (let idx = (cell:new 0))
      (iter:with-size
          (fn ()
            (let res = (index (cell:read idx) v))
            (cell:increment! idx)
            res)
        (length v))))

  (define-instance (iter:FromIterator (Seq :a) :a)
    (define (iter:collect! iter)
      (let ((rec (fn (result)
                   (match (iter:next! iter)
                     ((Some x)
                      (rec (push x result)))
                     ((None)
                      result)))))
        (rec (new)))))



  ;; (define-instance (Foldable Seq))
  ;;
  ;; use REDUCE/FOLD-SEQ

  ;; (define-instance (Into (List :a) (Seq :a)))

  (define-instance (Into (Seq :a) (List :a))
    (define (into v)
      (lisp (List :a) (v)
        (pvec:to-list v))))

  ;; (define-instance (Into (Vector :a) (Seq :a)))
  ;; (define-instance (Into (Seq :a) (Vector :a)))
  ;; (define-instance (Eq (Seq :a)))

  (define-instance (Default (Seq :a))
    (define default new))

  (declare extend (iter:IntoIterator :container :elt => Seq :elt -> :container -> Seq :elt))
  (define (extend vec iter)
    "Push every element in `iter` to the end of `vec`."
    (iter:fold! (flip push) vec (iter:into-iter iter)))

  (define (copy s)
    "Create a distinct (shallow) copy of the seq `s`. The result will be guaranteed to not share structural memory with the input."
    ;; N.B., We don't use `append` since it makes no guarantees about structure sharing.
    (extend (new) s)))

(cl:defmacro make (cl:&rest elements)
  "Construct a `Seq' containing the ELEMENTS, in the order listed."
  (cl:loop :with form := '(new)
           :for element :in elements
           :do (cl:setf form `(push ,element ,form))
           :finally (cl:return form)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/SEQ")
