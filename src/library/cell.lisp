(in-package #:coalton-library)
 
(cl:declaim (cl:inline make-cell-internal))

(cl:defstruct cell-internal
  (inner (cl:error "") :type cl:t))

(cl:defmethod cl:print-object ((self cell-internal) stream)
  (cl:format stream "~A" (cell-internal-inner self))
  self)

#+sbcl
(cl:declaim (sb-ext:freeze-type cell-internal))

(coalton-toplevel
  (define-type (Cell :a)
    "Internally mutable cell"
    (Cell Lisp-Object))

  (declare make-cell (:a -> (Cell :a)))
  (define (make-cell data)
    "Create a new mutable cell"
    (lisp (Cell :a) (data)
      (Cell (make-cell-internal :inner data))))

  (declare cell-read ((Cell :a) -> :a))
  (define (cell-read c)
    "Read the value of a mutable cell"
    (match c
      ((Cell c)
       (lisp :a (c)
         (cell-internal-inner c)))))

  (declare cell-swap (:a -> (Cell :a) -> :a))
  (define (cell-swap data c)
    "Replace the value of a mutable cell with a new value, then return the old value"
    (match c
      ((Cell c)
       (lisp :a (data c)
         (cl:let* ((old (cell-internal-inner c)))
           (cl:setf (cell-internal-inner c) data)
           old)))))

  (declare cell-write (:a -> (Cell :a) -> Unit))
  (define (cell-write data cell)
    "Set the value of a mutable cell"
    (progn
      (cell-swap data cell)
      Unit))

  (declare cell-update ((:a -> :a) -> (Cell :a) -> Unit))
  (define (cell-update f cell)
    (cell-write (f (cell-read cell)) cell))

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
