(in-package #:coalton-library)
 
(cl:declaim (cl:inline make-cell-internal))

(cl:defstruct cell-internal
  (inner (cl:error "") :type cl:t))

(cl:defmethod cl:print-object ((self cell-internal) stream)
  (cl:format stream "~A" (cell-internal-inner self))
  self)

#+(and sbcl coalton-release)
(cl:declaim (sb-ext:freeze-type cell-internal))

(coalton-toplevel
  (define-type (Cell :a)
    "Internally mutable cell"
    (%Cell Lisp-Object))

  (declare make-cell (:a -> (Cell :a)))
  (define (make-cell data)
    "Create a new mutable cell"
    (lisp (Cell :a) (data)
      (%Cell (make-cell-internal :inner data))))

  (declare cell-read ((Cell :a) -> :a))
  (define (cell-read c)
    "Read the value of a mutable cell"
    (match c
      ((%Cell c)
       (lisp :a (c)
         (cell-internal-inner c)))))

  (declare cell-swap! ((Cell :a) -> :a -> :a))
  (define (cell-swap! cel data)
    "Replace the value of a mutable cell with a new value, then return the old value"
    (match cel
      ((%Cell c)
       (lisp :a (data c)
         (cl:let* ((old (cell-internal-inner c)))
           (cl:setf (cell-internal-inner c) data)
           old)))))

  (declare cell-write! ((Cell :a) -> :a -> :a))
  (define (cell-write! cel data)
    "Set the value of a mutable cell, returning the new value"
    (match cel
      ((%Cell c)
       (lisp :a (data c)
         (cl:setf (cell-internal-inner c) data)))))

  (declare cell-update! ((:a -> :a) -> (Cell :a) -> :a))
  (define (cell-update! f cel)
    "Apply F to the contents of CEL, storing and returning the result"
    (cell-write! cel (f (cell-read cel))))

  (declare cell-update-swap! ((:a -> :a) -> (Cell :a) -> :a))
  (define (cell-update-swap! f cel)
    "Apply F to the contents of CEL, swapping the result for the old value"
    (cell-swap! cel (f (cell-read cel))))

  ;;; operators on cells of lists
  (declare cell-push! ((Cell (List :elt)) -> :elt -> (List :elt)))
  (define (cell-push! cel new-elt)
    (cell-update! (Cons new-elt) cel))

  (declare cell-pop! ((Cell (List :elt)) -> (Optional :elt)))
  (define (cell-pop! cel)
    (match (cell-read cel)
      ((Cons fst rst)
       (progn (cell-write! cel rst)
              (Some fst)))
      ((Nil) None)))

  ;;; operators on cells of numbers
  (declare cell-increment! ((Num :counter) => (Cell :counter) -> :counter))
  (define (cell-increment! cel)
    "Add one to the contents of CEL, storing and returning the new value"
    (cell-update! (+ (fromInt 1)) cel))

  (declare cell-decrement! ((Num :counter) => (Cell :counter) -> :counter))
  (define (cell-decrement! cel)
    "Add one to the contents of CEL, storing and returning the new value"
    (cell-update! (- (fromInt 1)) cel))

  ;; i am very skeptical of these instances
  (define-instance (Eq :a => (Eq (Cell :a)))
    (define (== c1 c2)
      (== (cell-read c1) (cell-read c2))))

  (define-instance (Num :a => (Num (Cell :a)))
    (define (+ c1 c2)
      (make-cell (+ (cell-read c1) (cell-read c2))))
    (define (- c1 c2)
      (make-cell (- (cell-read c1) (cell-read c2))))
    (define (* c1 c2)
      (make-cell (* (cell-read c1) (cell-read c2))))
    (define (fromInt i)
      (make-cell (fromInt i))))

  (define-instance (Semigroup :a => (Semigroup (Cell :a)))
    (define (<> a b)
      (make-cell (<> (cell-read a) (cell-read b)))))

  (define-instance (Functor Cell)
    (define (map f c)
      (make-cell (f (cell-read c)))))

  (define-instance (Applicative Cell)
    (define (pure x) (make-cell x))
    (define (liftA2 f c1 c2)
      (make-cell (f (cell-read c1) (cell-read c2)))))

  (define-instance (Into :a (Cell :a))
    (define into make-cell))

  (define-instance (Into (Cell :a) :a)
    (define into cell-read)))
