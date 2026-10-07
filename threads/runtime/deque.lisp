;;;; deque.lisp
;;;;
;;;; A Chase-Lev work-stealing deque.
;;;;
;;;; D. Chase and Y. Lev, "Dynamic Circular Work-Stealing Deque", SPAA
;;;; 2005, with the memory orderings of N. M. Lê, A. Pop, A. Cohen and
;;;; F. Zappa Nardelli, "Correct and Efficient Work-Stealing for Weak
;;;; Memory Models", PPoPP 2013.
;;;;
;;;; The owning worker pushes and pops at the bottom; any other thread
;;;; may steal from the top. Only the owner writes BOTTOM and the
;;;; buffer; thieves (and the owner, when racing for the last item)
;;;; advance TOP with compare-and-swap. Because Lisp is garbage
;;;; collected, a buffer that is replaced while growing may still be
;;;; read by thieves; no memory reclamation scheme is needed.

(in-package #:coalton/threads/runtime)

(defconstant +deque-initial-capacity+ 64
  "Initial number of slots in a deque buffer. Must be a power of two.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun padding-slots (prefix count)
    "Slot specifications for COUNT unused slots, used to keep fields
written by different threads on different cache lines."
    (loop :for i :below count
          :collect `(,(intern (format nil "~A-~D" prefix i)) nil :read-only t))))

(defmacro define-deque-structure ()
  `(defstruct (deque (:constructor make-deque ())
                     (:copier nil)
                     (:predicate nil))
     ;; Written only by the owner; read by thieves.
     (bottom 0 :type fixnum)
     (buffer (make-array +deque-initial-capacity+ :initial-element nil)
      :type simple-vector)
     ;; Used only by the owner: the slots of the items below this index
     ;; that were stolen have been cleared. See DEQUE-CLEAR-STOLEN.
     (cleared 0 :type fixnum)
     ;; Keep TOP 128 bytes away from BOTTOM, a distance that also
     ;; defeats adjacent-line prefetching on x86-64.
     ,@(padding-slots "%PAD-A" 13)
     ;; Advanced by thieves with compare-and-swap.
     (top 0 :type fixnum)
     ,@(padding-slots "%PAD-B" 15)))

(define-deque-structure)

(backend:freeze-type deque)

(declaim (inline deque-push deque-pop deque-steal deque-maybe-non-empty-p
                 deque-peek-top deque-peek-bottom))

(defun deque-clear-stolen (deque top)
  "Clear the slots of the items that thieves took from DEQUE, whose top
index is TOP, so that the garbage collector can reclaim those items.
Thieves never clear slots themselves, because the owner may already
have reused them. Only the owner may call this, and only when DEQUE is
empty: then no thief can take an item until the owner pushes again."
  (declare (type deque deque)
           (type fixnum top))
  (let* ((buffer (deque-buffer deque))
         (capacity (length buffer))
         (mask (1- capacity)))
    ;; Slots of indices more than CAPACITY below TOP have been reused.
    (loop :for i :of-type fixnum :from (max (deque-cleared deque) (- top capacity)) :below top
          :do (setf (svref buffer (logand i mask)) nil))
    (setf (deque-cleared deque) top))
  (values))

(defun deque-release-stolen (deque)
  "If DEQUE is empty, clear the slots of the items that thieves took from
it, as DEQUE-POP does, so that the garbage collector can reclaim them.
Only the owner may call this."
  (declare (type deque deque))
  (let ((top (deque-top deque)))
    (when (and (<= (deque-bottom deque) top)
               (< (deque-cleared deque) top))
      (deque-clear-stolen deque top)))
  (values))

(defun deque-grow (deque bottom top)
  "Replace the buffer of DEQUE with one twice as large, holding the
items in [TOP, BOTTOM). Only the owner may call this. Returns the new
buffer."
  (declare (type deque deque)
           (type fixnum bottom top))
  (let* ((old (deque-buffer deque))
         (old-mask (1- (length old)))
         (new (make-array (* 2 (length old)) :initial-element nil))
         (new-mask (1- (length new))))
    (loop :for i :of-type fixnum :from top :below bottom
          :do (setf (svref new (logand i new-mask))
                    (svref old (logand i old-mask))))
    ;; Thieves that see the new buffer must see its contents.
    (backend:release-fence)
    (setf (deque-buffer deque) new)
    new))

(defun deque-push (deque item)
  "Push ITEM onto the bottom of DEQUE. Only the owner may call this.
Returns true if DEQUE appeared to be empty before the push."
  (declare (type deque deque)
           (optimize speed))
  (let* ((bottom (deque-bottom deque))
         (top (deque-top deque))
         (buffer (deque-buffer deque)))
    (backend:acquire-fence)
    (when (>= (- bottom top) (length buffer))
      (setf buffer (deque-grow deque bottom top)))
    ;; Make the contents of ITEM visible before ITEM itself. Thieves and
    ;; scanners may read a slot after its index has left the window
    ;; [TOP, BOTTOM) that they read, and find ITEM there before this
    ;; push publishes it with the new bottom (see DEQUE-STEAL, whose
    ;; predicate runs before its compare-and-swap, and DEQUE-FIND-IF).
    (backend:release-fence)
    (setf (svref buffer (logand bottom (1- (length buffer)))) item)
    ;; Publish the item before the new bottom.
    (backend:release-fence)
    (setf (deque-bottom deque) (1+ bottom))
    (= bottom top)))

(defun deque-pop (deque)
  "Pop the most recently pushed item from DEQUE, or return NIL if it is
empty. Only the owner may call this."
  (declare (type deque deque)
           (optimize speed))
  (let* ((bottom (1- (deque-bottom deque)))
         (buffer (deque-buffer deque)))
    (setf (deque-bottom deque) bottom)
    ;; The store to BOTTOM must be visible before we read TOP.
    (backend:full-fence)
    (let ((top (deque-top deque)))
      (cond
        ((< bottom top)
         ;; Empty. Restore BOTTOM.
         (setf (deque-bottom deque) (1+ bottom))
         (when (< (deque-cleared deque) top)
           (deque-clear-stolen deque top))
         nil)
        (t
         (let* ((index (logand bottom (1- (length buffer))))
                (item (svref buffer index)))
           (cond
             ((< top bottom)
              ;; More than one item: no thief can take this one.
              ;; Clear the slot so that the item can be collected.
              (setf (svref buffer index) nil)
              item)
             (t
              ;; The last item: race with thieves for it.
              (let ((won (eql top (backend:compare-and-swap
                                   (deque-top deque) top (1+ top)))))
                (setf (deque-bottom deque) (1+ bottom))
                (setf (svref buffer index) nil)
                (and won item))))))))))

(defun deque-steal (deque &optional acceptable-p)
  "Steal the least recently pushed item from DEQUE. Returns the item,
NIL if DEQUE is empty, :RETRY if the attempt lost a race and might
succeed if repeated, or :REFUSED if ACCEPTABLE-P, a predicate, is given
and rejects the item."
  (declare (type deque deque)
           (type (or null function) acceptable-p)
           (optimize speed))
  (let ((top (deque-top deque)))
    (backend:full-fence)
    (let ((bottom (deque-bottom deque)))
      (backend:acquire-fence)
      (if (< top bottom)
          (let* ((buffer (deque-buffer deque))
                 (item (svref buffer (logand top (1- (length buffer))))))
            (cond ((null item)
                   ;; The owner took the item and cleared its slot.
                   :retry)
                  ((and acceptable-p (not (funcall acceptable-p item)))
                   :refused)
                  ((eql top (backend:compare-and-swap (deque-top deque) top (1+ top)))
                   item)
                  (t
                   :retry)))
          nil))))

(defun deque-peek-top (deque)
  "The item of DEQUE that a thief would take next, or NIL. The answer
may be stale."
  (declare (type deque deque))
  (let ((top (deque-top deque))
        (bottom (deque-bottom deque)))
    (when (< top bottom)
      (let ((buffer (deque-buffer deque)))
        (svref buffer (logand top (1- (length buffer))))))))

(defun deque-peek-bottom (deque)
  "The item of DEQUE that its owner would pop next, or NIL. Only the
owner may call this."
  (declare (type deque deque))
  (let ((top (deque-top deque))
        (bottom (deque-bottom deque)))
    (when (< top bottom)
      (let ((buffer (deque-buffer deque)))
        (svref buffer (logand (1- bottom) (1- (length buffer))))))))

(defun deque-maybe-non-empty-p (deque)
  "True if DEQUE appeared to contain items. The answer may be stale."
  (declare (type deque deque))
  (< (deque-top deque) (deque-bottom deque)))

(defun deque-find-if (predicate deque)
  "An item of DEQUE that satisfies PREDICATE, looking from the top, or
NIL. Any thread may call this, but the answer may be stale: the item
may have been taken, or even pushed again, by the time it is returned,
and may never have been in DEQUE at all if its owner grew the buffer
meanwhile. Callers must therefore claim the item atomically before
acting on it. The contents of the item are visible, though: DEQUE-PUSH
makes them visible before the item."
  (declare (type function predicate)
           (type deque deque))
  (let ((top (deque-top deque))
        (bottom (deque-bottom deque)))
    (backend:acquire-fence)
    (let* ((buffer (deque-buffer deque))
           (mask (1- (length buffer))))
      (loop :for i :of-type fixnum :from top :below (min bottom (+ top (length buffer)))
            :for item := (svref buffer (logand i mask))
            :when (and item (funcall predicate item))
              :return item))))
