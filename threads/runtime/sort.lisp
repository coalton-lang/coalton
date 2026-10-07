;;;; sort.lisp
;;;;
;;;; Parallel stable merge sort.
;;;;
;;;; The two halves of a range are sorted in parallel into a scratch
;;;; vector, alternating between the data and scratch vectors at each
;;;; level, and then merged back in parallel. A merge is split by
;;;; cutting the longer run in the middle and binary-searching the cut
;;;; point in the other run, choosing the bound so that equal elements
;;;; of the first run stay ahead of those of the second.

(in-package #:coalton/threads/runtime)

(defconstant +insertion-sort-length+ 16
  "Ranges at most this long are sorted by insertion.")

(defconstant +sequential-sort-length+ 2048
  "Ranges at most this long are sorted sequentially.")

(defconstant +sequential-merge-length+ 4096
  "Merges of at most this many elements are done sequentially.")

(defmacro with-sort-types (&body body)
  `(locally (declare (type simple-vector a b)
                     (type fixnum lo hi)
                     (type function less))
     ,@body))

(defun %insertion-sort (a lo hi less)
  "Stably sort A[LO, HI) in place."
  (declare (type simple-vector a)
           (type fixnum lo hi)
           (type function less))
  (loop :for i :of-type fixnum :from (1+ lo) :below hi
        :do (let ((x (svref a i))
                  (j (1- i)))
              (declare (type fixnum j))
              (loop :while (and (>= j lo) (funcall less x (svref a j)))
                    :do (setf (svref a (1+ j)) (svref a j))
                        (decf j))
              (setf (svref a (1+ j)) x)))
  (values))

(defun %merge (src lo1 hi1 lo2 hi2 dst k less)
  "Stably merge the sorted runs SRC[LO1, HI1) and SRC[LO2, HI2) into DST
starting at index K. Among equal elements, those of the first run come
first."
  (declare (type simple-vector src dst)
           (type fixnum lo1 hi1 lo2 hi2 k)
           (type function less))
  (loop
    (cond ((>= lo1 hi1)
           (replace dst src :start1 k :start2 lo2 :end2 hi2)
           (return))
          ((>= lo2 hi2)
           (replace dst src :start1 k :start2 lo1 :end2 hi1)
           (return))
          ((funcall less (svref src lo2) (svref src lo1))
           (setf (svref dst k) (svref src lo2))
           (incf lo2)
           (incf k))
          (t
           (setf (svref dst k) (svref src lo1))
           (incf lo1)
           (incf k))))
  (values))

;;; In the following, a function named ...-SORT leaves the sorted
;;; elements of [LO, HI) in A, using B as scratch, and one named
;;; ...-SORT-INTO leaves them in B, using A as scratch.

(defun %sequential-sort (a b lo hi less)
  (with-sort-types
    (if (<= (- hi lo) +insertion-sort-length+)
        (%insertion-sort a lo hi less)
        (let ((mid (ash (+ lo hi) -1)))
          (%sequential-sort-into a b lo mid less)
          (%sequential-sort-into a b mid hi less)
          (%merge b lo mid mid hi a lo less))))
  (values))

(defun %sequential-sort-into (a b lo hi less)
  (with-sort-types
    (if (<= (- hi lo) +insertion-sort-length+)
        (progn
          (replace b a :start1 lo :start2 lo :end2 hi)
          (%insertion-sort b lo hi less))
        (let ((mid (ash (+ lo hi) -1)))
          (%sequential-sort a b lo mid less)
          (%sequential-sort a b mid hi less)
          (%merge a lo mid mid hi b lo less))))
  (values))

(defun %lower-bound (v lo hi x less)
  "The first index in [LO, HI) of the sorted V whose element is not less
than X, or HI."
  (declare (type simple-vector v)
           (type fixnum lo hi)
           (type function less))
  (loop :while (< lo hi)
        :do (let ((mid (ash (+ lo hi) -1)))
              (if (funcall less (svref v mid) x)
                  (setf lo (1+ mid))
                  (setf hi mid))))
  lo)

(defun %upper-bound (v lo hi x less)
  "The first index in [LO, HI) of the sorted V whose element is greater
than X, or HI."
  (declare (type simple-vector v)
           (type fixnum lo hi)
           (type function less))
  (loop :while (< lo hi)
        :do (let ((mid (ash (+ lo hi) -1)))
              (if (funcall less x (svref v mid))
                  (setf hi mid)
                  (setf lo (1+ mid)))))
  lo)

(defun %parallel-merge (src lo1 hi1 lo2 hi2 dst k less)
  (declare (type simple-vector src dst)
           (type fixnum lo1 hi1 lo2 hi2 k)
           (type function less))
  (let ((n1 (- hi1 lo1))
        (n2 (- hi2 lo2)))
    (if (<= (+ n1 n2) +sequential-merge-length+)
        (%merge src lo1 hi1 lo2 hi2 dst k less)
        (multiple-value-bind (m1 m2)
            (if (>= n1 n2)
                (let ((m1 (ash (+ lo1 hi1) -1)))
                  (values m1 (%lower-bound src lo2 hi2 (svref src m1) less)))
                (let ((m2 (ash (+ lo2 hi2) -1)))
                  (values (%upper-bound src lo1 hi1 (svref src m2) less) m2)))
          (let ((k2 (+ k (- m1 lo1) (- m2 lo2))))
            (join2-in-worker *worker*
                             (lambda ()
                               (%parallel-merge src lo1 m1 lo2 m2 dst k less)
                               nil)
                             (lambda ()
                               (%parallel-merge src m1 hi1 m2 hi2 dst k2 less)
                               nil))))))
  (values))

(defun %parallel-sort (a b lo hi less)
  (with-sort-types
    (if (<= (- hi lo) +sequential-sort-length+)
        (%sequential-sort a b lo hi less)
        (let ((mid (ash (+ lo hi) -1)))
          (join2-in-worker *worker*
                           (lambda () (%parallel-sort-into a b lo mid less) nil)
                           (lambda () (%parallel-sort-into a b mid hi less) nil))
          (%parallel-merge b lo mid mid hi a lo less))))
  (values))

(defun %parallel-sort-into (a b lo hi less)
  (with-sort-types
    (if (<= (- hi lo) +sequential-sort-length+)
        (%sequential-sort-into a b lo hi less)
        (let ((mid (ash (+ lo hi) -1)))
          (join2-in-worker *worker*
                           (lambda () (%parallel-sort a b lo mid less) nil)
                           (lambda () (%parallel-sort a b mid hi less) nil))
          (%parallel-merge a lo mid mid hi b lo less))))
  (values))

(defun parallel-sort (vector less)
  "Stably sort VECTOR in place, potentially in parallel, according to
LESS, a function of two arguments that returns true if its first
argument is strictly less than its second. Returns VECTOR. If LESS
signals a serious condition, VECTOR is left unchanged."
  (declare (type vector vector)
           (type function less))
  (let ((n (length vector)))
    (when (> n 1)
      (let ((a (make-array n))
            (b (make-array n)))
        (replace a vector)
        (cond ((<= n +sequential-sort-length+)
               (%sequential-sort a b 0 n less))
              (*worker*
               (%parallel-sort a b 0 n less))
              (t
               (call-in-pool (lambda () (%parallel-sort a b 0 n less) nil))))
        (replace vector a))))
  vector)
