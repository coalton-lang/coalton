;;;; persistent-vector.lisp
;;;;
;;;; This file implements a "persistent bit-partitioned vector trie"
;;;; data structure. (We refer to this specific structure as just a
;;;; "persisrent vector".) The basic idea is this. Elements of a
;;;; linear sequence are stored in-order as leaves of an K-ary tree,
;;;; where we have selected K=32. Every element is indexed by a string
;;;; (like a normal prefix trie for character strings), but the string
;;;; is a series of (log 32 2) integers. Extra information like the
;;;; tree's height H and the number of elements C in the tree
;;;; eliminates the need for a full allocation of 32^(H+1), which is
;;;; 32^H elements at the leaves and 32^H-worth of intermediate nodes.
;;;;
;;;; We use this tree structure because modication or addition to the
;;;; structure only requires paths from the root to the target
;;;; modification to be copied and manipulated. For a tree of C
;;;; elements, with C very large, we only need to copy and modify on
;;;; the order of log(C) nodes. One billion elements requires around 6
;;;; nodes to be modified. In general, the "efficient" operations of a
;;;; persistent vector will share a majority of the structure with the
;;;; input.
;;;;
;;;; We also implement a common optimization, which is to append
;;;; elements to a "tail node", before that tail node gets absorbed
;;;; into the tree. That way, 31/32 = 96.8% of the time we don't need
;;;; to do costly tree operations and we can push elements in O(1)
;;;; time.
;;;;
;;;; Efficient operations (linear to logarithmic):
;;;;
;;;;     - Compute length (LENGTH)
;;;;     - Push an element to the end (PUSH-BACK)
;;;;     - Pop an element from the end (POP-BACK)
;;;;     - Prune the end of the sequence (PRUNE)
;;;;     - Random access (GET)
;;;;     - Random modification (PUT)
;;;;
;;;; Iteration through all of the elements is log-linear. (It could be
;;;; made linear, but would incur a memory cost.)
;;;;
;;;; Persistent vectors are not silver bullets. They incur around a
;;;; 5-10x speed penalty on even simple benchmarks compared to
;;;; adjustable arrays. They also don't support efficient:
;;;;
;;;;     - Pushing or deleting from the front
;;;;     - Splicing or deleting from the middle
;;;;     - Concatenation
;;;;
;;; Each of these requires a "linear rebuild" of at least one of the
;;; inputs.

(defpackage #:coalton/persistent-vector
  (:documentation "A portable implementation of persistent bit-partitioned vector tries. These are vector-like data structures which are immutable. These are used to implement Coalton's `Seq` type.")
  (:use #:cl)
  (:shadow #:get #:length #:map #:reduce #:subseq)
  (:export #:persistent-vector
           #:persistent-vector-p
           #:put
           #:get
           #:length
           #:push-back
           #:pop-back
           #:prune
           #:for-each
           #:map
           #:reduce
           #:subseq
           #:range
           #:to-list
           #:to-vector
           #:index-error))

;;; TODO: This file can be tremendously optimized with appropriate
;;; type declarations.

(in-package #:coalton/persistent-vector)

;;; These are markers that are used internally to denote vacancy. We
;;; don't use NIL since NIL can represent valid values, If a user sees
;;; |@vacant@|, it's definitely an error.

(defconstant +vacant+ '|@vacant@|
  "A unique marker used for intermediate empty spots in the tree. Should not be seen by the user.")

(defun vacantp (x)
  (eq x +vacant+))

(deftype vacant (x)
  `(or ,x (member |@vacant@|)))


;;; Misc Utilities

(defun copy-vector (vector &key (adjoin nil adjoin-supplied-p))
  "Copy the vector VECTOR as a simple-vector. Optionally adjoin a single element by supplying it as ADJOIN."
  (declare (type simple-vector vector))
  (let ((n (cl:length vector)))
    (cond
      (adjoin-supplied-p
       (let ((new-vector (make-array (1+ n) :element-type t :initial-element +vacant+)))
         (replace new-vector vector)
         (setf (svref new-vector n) adjoin)
         new-vector))
      (t
       ;; We cannot displace in general because the vector may be
       ;; overwritten after the copy.
       (make-array n :element-type t :initial-contents vector)))))

(defun shrink-vector (vector shrink-by)
  "Copy the vector VECTOR as a simple-vector, shrinking it by BY elements."
  (declare (type simple-vector vector))
  (let* ((n (cl:length vector))
         (m (- n shrink-by)))
    (cond
      ((minusp m)
       (error "Invalid vector shrink."))
      ((= m n)
       (copy-vector vector))
      (t
       (let ((new-vector (make-array m :element-type t :initial-element +vacant+)))
         (replace new-vector vector)
         new-vector)))))


;;; The error we use for out-of-bounds access.

(define-condition index-error (error) ())


;;; Configuration
;;;
;;; +BRANCHING-FACTOR+ can be any power-of-two greater than 1, ahd
;;; should be chosen solely for efficiency reasons. Smaller branching
;;; factors may be easier to debug. Non-power-of-two factors should
;;; work fine but are not supported, so long as +COEFFICIENT-BITS+ is
;;; calculated correctly.

(defconstant +branching-factor+ 32
  "The maximum number of branches that each node has.")

(defconstant +coefficient-bits+ (1- (integer-length +branching-factor+))
  "The number of bits that each tree level is addressed by.")


;;; Persistent Vector Types
;;;
;;; The user should only ever be concerned with
;;; PERSISTENT-VECTOR. Sub-types are not exported.

(defstruct (persistent-vector (:predicate   persistent-vector-p)
                              (:copier      nil)
                              (:conc-name   nil)
                              (:constructor nil))
  "Abstract base class for persistent vectors.")

(defstruct (persistent-vector-sequence (:include     persistent-vector)
                                       (:copier      nil)
                                       (:predicate   nil)
                                       (:conc-name   seq-)
                                       (:constructor make-seq))
  "An implementation of a \"persistent bit-partitioned vector trie\", a persistent sequence structure with \"almost constant\" random access, modification, pushing, and popping."
  ;; HEIGHT represents how many levels deep the tree is. Each level of
  ;; the tree is indexed starting at 0 from the leaves. The root has
  ;; level (1- HEIGHT).
  (height 1 :read-only t)
  ;; COUNT keeps track of the number of elements contained in the
  ;; sequence.
  (count 0 :read-only t)
  ;; ROOT is either vacant or a SIMPLE-VECTOR of +BRANCHING-FACTOR+
  ;; nodes.
  (root +vacant+ :read-only t)
  ;; TAIL is a tail node, which is either vacant, or a SIMPLE-VECTOR
  ;; of length from 1 to +BRANCHING-FACTOR+ inclusive. It gets filled
  ;; with elements, and then ingested into the structure when
  ;; full. TAIL only contains elements, not sub-nodes.
  (tail +vacant+ :read-only t))

(defstruct (persistent-vector-view (:include     persistent-vector)
                                   (:copier      nil)
                                   (:predicate   nil)
                                   (:conc-name   view-)
                                   (:constructor make-view))
  "A \"slice\" or \"view\" into an existing persistent vector."
  (base nil :type persistent-vector-sequence :read-only t)
  (start 0 :read-only t)
  ;; END is exclusive.
  (end 0 :read-only t))

#+sbcl
(declaim (sb-ext:freeze-type persistent-vector persistent-vector-sequence persistent-vector-view))


;;; External API
;;;
;;; We use generic functions, though we do not expect consumers of the
;;; API to extend them. These could instead be ordinary functions that
;;; ETYPECASE onto the requisite argument.

(defgeneric get (v index)
  (:documentation "What is the INDEXth element of V?"))

(defgeneric length (v)
  (:documentation "How many elements does V contain?"))

(defgeneric push-back (v x)
  (:documentation "Produce a sequence like V with the object X pushed onto the end."))

(defgeneric pop-back (v)
  (:documentation "Produce a sequence like V with the last element removed. Return two values:

1. The new sequence.
2. The element that was popped.

An empty sequence will error if popped."))

(defgeneric prune (v n)
  (:documentation "Prune the length of V down to N by removing elements off of the end."))

(defgeneric put (v index x)
  (:documentation "Produce a sequence like V but with the INDEXth element replaced by the object X."))

(defgeneric subseq (v start &optional end)
  (:documentation "Produce a sequence like V containing elements from START (inclusive) to END (exclusive).

N.B., This function may produce a \"view\" into V."))

(defgeneric for-each (v f)
  (:documentation "Apply the unary function F to every element of V."))


;;; Index Structure and Tree Structure

(defun node-starting-index (index)
  "Given an index INDEX into a persistent vector, find the index of the first element of the node which contains INDEX."
  (deposit-field 0 (byte +coefficient-bits+ 0) index))

(defun offset (index)
  "Given an index INDEX into a persistent vector, calculate the node offset for that index. This is the offset into the resident node of this index, that is,

    (SVREF (OFFSET INDEX) (NODE-OF-INDEX INDEX))

will be the INDEXth element of the persistent vector. Alternatively,

    (+ (NODE-STARTING-INDEX INDEX) (OFFSET INDEX)) == INDEX."
  (mask-field (byte +coefficient-bits+ 0) index))

(defun offset-at-height (index height)
  "If we wish to traverse to the index INDEX and we are at its node at height HEIGHT, at what offset can we find the next node?"
  (offset (ash index (- (* +coefficient-bits+ height)))))

(defun tail-starting-index (v)
  "The index at which the tail of V starts."
  (let ((n (seq-count v)))
    (if (< n +branching-factor+)
        0
        (node-starting-index (1- n)))))

(defun make-node (&rest objects)
  "Make a tree node of OBJECTS objects, which shall not exceed +BRANCHING-FACTOR+ in count."
  (declare (dynamic-extent objects))
  (let ((node (make-array +branching-factor+ :element-type t :initial-element +vacant+)))
    (replace node objects)
    node))

;;; Persistent Vector Sequences

(defun make-empty-persistent-vector ()
  (load-time-value (make-seq) t))

(defun persistent-vector (&rest items)
  "Construct a persistent vector containing the objects ITEMS in order."
  (cl:reduce #'push-back items :initial-value (make-empty-persistent-vector)))

(defmethod length ((v persistent-vector-sequence))
  (seq-count v))

(defun find-node (root height index)
  "Find the node indexed by INDEX starting from the root node ROOT sitting at height HEIGHT."
  (if (zerop height)
      root
      (find-node (svref root (offset-at-height index height))
                 (1- height)
                 index)))

(defun copy-and-ensure-path (root height index)
  "Create a copy of the node ROOT (at height HEIGHT) along the path to the INDEXth element. If nodes along that path don't exist, create them. Return two values:

1. The new root (with the embedded path copied).

2. The leaf node of the embedded path (which is fresh and can be mutated).
"
  (let (greatest-grandchild)
    (labels ((walk (root-node height)
               (cond
                 ;; We are at a leaf node.
                 ((zerop height)
                  (setf greatest-grandchild root-node)
                  root-node)
                 ;; We've "reached" a node that doesn't exist, so we'll
                 ;; create it.
                 ((vacantp root-node)
                  (let* ((child-index (offset-at-height index height))
                         (new-root (make-node))
                         (new-child (make-node)))
                    (setf (svref new-root child-index)
                          (walk new-child (1- height)))
                    new-root))
                 ;; Continue descending, copying along the way.
                 (t
                  (let* ((child-index (offset-at-height index height))
                         (new-root (copy-vector root-node))
                         (child (svref root-node child-index)))
                    (setf (svref new-root child-index)
                          (walk child (1- height)))
                    new-root)))))
      (values (walk root height)
              greatest-grandchild))))

(defun node-of-index (v index)
  "Return the node of V which contains the INDEXth element."
  (unless (<= 0 index (1- (seq-count v)))
    (error 'index-error))
  (if (>= index (tail-starting-index v))
      (seq-tail v)
      (find-node (seq-root v) (seq-height v) index)))

(defmethod get ((v persistent-vector-sequence) index)
  (svref (node-of-index v index) (offset index)))

(defun adjoin-to-tail (v x)
  "Given a persistent vector V whose tail has space, adjoin the object X to the tail. If a tail is not present, make one."
  ;; The tail is the one exception to how we make nodes. Tails can
  ;; have less than +BRANCHING-FACTOR+ elements in them, which allows
  ;; small vectors to be efficient.
  (let* ((old-tail (seq-tail v))
         (new-tail (if (vacantp old-tail)
                       (vector x)
                       (copy-vector old-tail :adjoin x))))
    (make-seq :root (seq-root v)
              :tail new-tail
              :height (seq-height v)
              :count (1+ (seq-count v)))))

(defun make-path (num-levels node)
  "Build a path of NUM-LEVELS levels on top of the node NODE."
  ;; We build up from the leaf node.
  (cond
    ((zerop num-levels)
     node)
    (t
     (make-path (1- num-levels) (make-node node)))))

(defun root-filled-out-p (v)
  "Is the root of the persistent vector V \"filled out\"?

A persistent vector V is \"filled out\" if adding another node to V would increase its height.

N.B. This does *not* mean there isn't additional capacity for elements."
  (< (expt +branching-factor+ (seq-height v))
     (ash (seq-count v) (- +coefficient-bits+))))

(defun absorb-tail (v)
  "Absorb the (presumably full) tail TAIL of V as the rightmost leaf of V."
  ;; Invariant:
  (assert (= +branching-factor+ (cl:length (seq-tail v))))
  (let ((last-index (1- (seq-count v)))
        (tail (seq-tail v))
        (root (if (vacantp (seq-root v))
                  (make-node)
                  (seq-root v))))
    (labels ((absorb (height parent)
               (let* ((index (offset-at-height last-index height)) ;where is the trail currently?
                      (new-parent (copy-vector parent)) ;duplicate the path so far...
                      (new-child
                        ;; If we've reached the height just above the leaves...
                        (if (= height 1)
                            ;; ...then return the tail.
                            tail
                            ;; ...otherwise we need to see if we can
                            ;; continue to descend.
                            (let ((current-child (svref parent index)))
                              ;; If we have nothing to descend to...
                              (if (vacantp current-child)
                                  ;; ...then make a clean path to the new tail.
                                  (make-path (1- height) tail)
                                  ;; ...otherwise, we descend.
                                  (absorb (1- height) current-child))))))
                 ;; Assign the path (with the tail now embedded) as a child to the parent.
                 (setf (svref new-parent index) new-child)
                 ;; Return the parent.
                 new-parent)))
      ;; Neither height nor count change when we move the tail
      ;; elements into the larger structure.
      (make-seq
       :root (absorb (seq-height v) root)
       :tail +vacant+
       :height (seq-height v)
       :count (seq-count v)))))

(defun tail-full-p (v)
  "Is there room for additional elements in the tail?"
  ;; Instead of calculating the length of the tail (which may
  ;; inadvertently have vacancy markers), we use this method which
  ;; only relies on the data structure's invariants.
  (<= (+ +branching-factor+ (tail-starting-index v))
      (seq-count v)))

(defmethod push-back ((v persistent-vector-sequence) x)
  (cond
    ;; Does the tail have space?
    ((not (tail-full-p v))
     (adjoin-to-tail v x))
    ;; Does the root have space?
    ((not (root-filled-out-p v))
     (let ((new-root (absorb-tail v)))
       ;; N.B. Essentially a recursive call to PUSH-BACK.
       (adjoin-to-tail new-root x)))
    ;; The tail is full and the root is full.
    (t
     (let ((new-root
             ;; Grow the root out.
             (make-seq :root (make-node
                              ;; Old root.
                              (seq-root v)
                              ;; New child with the old tail.
                              (make-path (seq-height v) (seq-tail v)))
                       :tail +vacant+
                       :height (1+ (seq-height v))
                       :count (seq-count v))))
       ;; N.B. Essentially a recursive call to PUSH-BACK.
       (adjoin-to-tail new-root x)))))

(defmethod put ((v persistent-vector-sequence) index x)
  (let ((count (seq-count v))
        (height (seq-height v))
        (root (seq-root v))
        (tail (seq-tail v)))
    (unless (<= 0 index (1- count))
      (error 'index-error))
    (cond
      ;; If we can, moify the tail.
      ((<= 0 (- index (tail-starting-index v)) +branching-factor+) ; RSS: added <= 0 x case; just do zero?
       (let ((new-tail (copy-vector tail)))
         (setf (svref new-tail (offset index)) x)
         (make-seq :root root
                   :tail new-tail
                   :height height
                   :count count)))
      ;; Otherwise, modify an internal node.
      (t
       (multiple-value-bind (new-root leaf-node)
           (copy-and-ensure-path root height index)
         (setf (svref leaf-node (offset index)) x)
         (make-seq :root new-root
                   :tail tail
                   :height height
                   :count count))))))

(defun prune-path (root height index)
  "Assuming INDEX is an index into a non-tail node, prune all elements of ROOT (which is of height HEIGHT) that are above INDEX. (This function does not mutate ROOT.)

Return two values:

1. The new root.

2. The new height."
  (assert (not (vacantp root)))
  (assert (not (minusp index)))
  (labels ((walk (root-node height)
             (cond
               ;; We are at a leaf node.
               ((zerop height)
                (when (eq root root-node)
                  (setf root-node (copy-vector root)))
                (fill root-node +vacant+ :start (1+ (offset-at-height index height)))
                root-node)
               ;; We've "reached" a node that doesn't exist.
               ((vacantp root-node)
                (error "Trying to prune index that doesn't exist."))
               ;; Continue descending, copying along the way.
               (t
                (let* ((child-index (offset-at-height index height))
                       (new-node (copy-vector root-node))
                       (new-root (svref root-node child-index)))
                  ;; Set the Nth element.
                  (setf (svref new-node child-index)
                        (walk new-root (1- height)))
                  (fill new-node +vacant+ :start (1+ child-index))
                  new-node))))
           (collapse (root height)
             (cond
               ((vacantp root)
                (values root height))
               ((vacantp (svref root 1))
                (collapse (svref root 0) (1- height)))
               (t
                (values root height)))))
    (collapse (walk root height) height)))

(defmethod pop-back ((v persistent-vector-sequence))
  (values (prune v (1- (length v)))
          (get v (1- (length v)))))

(defmethod prune ((v persistent-vector-sequence) n)
  (let ((c (length v)))
    (unless (<= 0 n c)
      (error 'index-error))
    (cond
      ((zerop n)
       (make-empty-persistent-vector))
      ((= n c)
       v)
      (t
       (let ((tail-start (tail-starting-index v))
             (new-last-index (1- n))
             (root (seq-root v))
             (height (seq-height v)))
         (cond
           ;; We are pruning part of the tail.
           ((<= tail-start new-last-index)
            (make-seq :root root
                      :tail (shrink-vector (seq-tail v) (- c n))
                      :height (seq-height v)
                      :count n))
           ;; The existing tail is getting pruned off. We need to find
           ;; the node which becomes the new tail.
           (t
            (let* ((tail-node-first-index (node-starting-index new-last-index))
                   (new-tail (shrink-vector (find-node root height new-last-index)
                                            (- +branching-factor+ (offset new-last-index) 1))))
              (cond
                ;; We only have one node, which is being ejected as a tail node.
                ((zerop tail-node-first-index)
                 (make-seq :root +vacant+
                           :tail new-tail
                           :height 1
                           :count n))
                ;; We have more nodes. Eject the tail, prune the rest.
                (t
                 (let ((penultimate-node-last-index
                         (1- tail-node-first-index)))
                   (multiple-value-bind (pruned-root pruned-height)
                       (prune-path root height penultimate-node-last-index)
                     (make-seq :root pruned-root
                               :tail new-tail
                               :height pruned-height
                               :count n)))))))))))))

;;; Views
;;;
;;; Views are mostly useful for iterative purposes. The API makes them
;;; transparent to the user.

(defmethod subseq ((v persistent-vector-sequence) start &optional (end (length v)))
  (unless (<= 0 start end (length v))
    (error 'index-error))
  (cond
    ((= start (1- end))
     (persistent-vector (get v start)))
    ((= start end)
     (persistent-vector))
    (t
     (make-view :base v :start start :end end))))

(defmethod subseq ((v persistent-vector-view) start &optional (end (length v)))
  (unless (<= 0 start end (length v))
    (error 'index-error))
  (subseq (view-base v)
          (+ start (view-start v))
          (+ end (view-start v))))

(defmethod get ((v persistent-vector-view) index)
  (get (view-base v) (+ index (view-start v))))

(defmethod length ((v persistent-vector-view))
  (- (view-end v) (view-start v)))

(defmethod put ((v persistent-vector-view) index x)
  (unless (and (<= (view-start v) index)
               (<  index (view-end v)))
    (error 'index-error))
  (let ((base (view-base v)))
    (setf base (put base (+ index (view-start v)) x))
    (subseq base (view-start v) (view-end v))))

(defmethod push-back ((v persistent-vector-view) x)
  (let* ((base (view-base v))
         (base-count (length base))
         (new-index (view-end v)))
    (cond
      ((< new-index base-count)
       (setf base (put base new-index x)))
      (t
       (setf base (push-back base x))))
    (subseq base (view-start v) (1+ new-index))))

(defmethod pop-back ((v persistent-vector-view))
  (let ((start (view-start v))
        (end   (view-end v)))
    (cond
      ((> end start)
       (values
        (subseq (view-base v) start (1- end))
        (get v (1- (length v)))))
      (t
       (error 'index-error)))))

(defmethod prune ((v persistent-vector-view) n)
  (let ((start (view-start v))
        (count (length v)))
    (cond
      ((not (<= 0 n count))
       (error 'index-error))
      ((zerop n)
       (make-empty-persistent-vector))
      ((= n count)
       v)
      (t
       (subseq (view-base v) start (+ start n))))))

;;; Iteration

(defun bounded-for-each (v f from to)
  "Apply the function F to every element of V indexed between FROM (inclusive) and TO (exclusive)."
  (when (plusp (length v))
    ;; Iterate through the first partial block first.
    (unless (zerop (offset from))
      (let ((start-node (node-of-index v from))
            (remaining (min (- to from)
                            (- +branching-factor+ (offset from)))))
        (loop :repeat remaining
              :for i :from (offset from)
              :do (funcall f (svref start-node i))
                  (incf from))))
    ;; Iterate through entire blocks, except the last.
    (when (< from to)
      (assert (zerop (offset from)))
      (loop :with upper := (node-starting-index to)
            :for i :from from :below upper :by +branching-factor+
            :for node := (node-of-index v i)
            :do (cl:map nil f node)
                (incf from +branching-factor+)))
    ;; Iterate through the last block
    (when (< from to)
      (assert (<= (- to from) +branching-factor+))
      (assert (zerop (offset from)))
      (cl:map nil f (node-of-index v from)))
    nil))

(defmethod for-each ((v persistent-vector-sequence) f)
  (bounded-for-each v f 0 (seq-count v)))

(defmethod for-each ((v persistent-vector-view) f)
  (bounded-for-each (view-base v) f (view-start v) (view-end v)))


;;; Print Method

(defmethod print-object ((v persistent-vector) stream)
  (print-unreadable-object (v stream :type nil :identity nil)
    (write-string "Seq" stream)
    (block :PRINT-LOOP
      (format stream "[~D]" (length v))
      (when (plusp (length v))
        (let ((left (or *print-length* -1)))
          (for-each v (lambda (x)
                        (cond
                          ((not (zerop left))
                           (write-char #\Space stream)
                           (prin1 x stream)
                           (setf left (max -1 (1- left))))
                          (t
                           (write-string " ..." stream)
                           (return-from :PRINT-LOOP))))))))))

;;; Conversion Functions

(defun to-list (v)
  "Convert V to a Common Lisp list."
  (let* ((head (cons nil nil))
         (tail head))
    (flet ((next (x)
             (rplacd tail (cons x nil))
             (setf tail (cdr tail))))
      (declare (dynamic-extent #'next))
      (for-each v #'next)
      (cdr head))))

(defun to-vector (v)
  "Convert V to a Common Lisp vector."
  (let ((vector (make-array (length v)))
        (i 0))
    (flet ((next (x)
             (setf (svref vector i) x)
             (incf i)))
      (declare (dynamic-extent #'next))
      (for-each v #'next)
      vector)))

;;; Common Sequence Functions

(defun map (f v)
  "Apply F to every element of V, producing a new persisent vector."
  (let ((result (persistent-vector)))
    (flet ((next (x) (setf result (push-back result (funcall f x)))))
      (declare (dynamic-extent #'next))
      (for-each v #'next)
      result)))

(defun reduce (f v &key (initial-value (get v 0) init-supplied?))
  (loop
    :for i :from (if init-supplied? 0 1) :to (1- (length v))
    :do (setf initial-value (funcall f initial-value (get v i)))
    :finally (return initial-value)))

(defun range (n)
  (let ((vec (persistent-vector)))
    (dotimes (i n vec)
      (setf vec (push-back vec i)))))

;;; BENCHMARKING
;;;
;;; So far, it seems about 10x slower than adjustable vectors.

(defun range* (n)
  (let ((vec (make-array 1 :adjustable t :fill-pointer 0)))
     (dotimes (i n vec)
       (vector-push-extend i vec))))

(defun bench1 ()
  (time (reduce #'+ (range 10000000))))

(defun bench1* ()
  (time (cl:reduce #'+ (range* 10000000))))

(defun swap (v a b)
  (let ((x (get v a))
        (y (get v b)))
    (put (put v b x) a y)))

(defun bench2 ()
  (let* ((v (range 10000000))
         (c (length v)))
    (time (loop :repeat (ceiling c 2)
                :do (swap v (random c) (random c))))))

(defun bench2* ()
  (let* ((v (range* 10000000))
         (c (cl:length v)))
    (time (loop :repeat (ceiling c 2)
                :do (rotatef (aref v (random c)) (aref v (random c)))))))
