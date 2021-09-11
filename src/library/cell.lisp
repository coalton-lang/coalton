(in-package #:coalton-library)

(cl:defstruct cell-internal
  (inner (cl:error "") :type cl:t))

(coalton-toplevel
  (define-type (Cell :a)
    "Internally mutable cell"
    (Cell Lisp-Object))

  (declare make-cell (:a -> (Cell :a)))
  (define (make-cell data)
    "Create a new mutable cell"
    (lisp (Cell :a) (data)
      (Cell (veil (make-cell-internal :inner data)))))

  (declare cell-read ((Cell :a) -> :a))
  (define (cell-read cell)
    "Read the value of a mutable cell"
    (lisp :a (cell)
      (cl:slot-value (unveil (cl:slot-value cell '_0)) 'inner)))

  (declare cell-swap (:a -> (Cell :a) -> :a))
  (define (cell-swap data cell)
    "Replace the value of a mutable cell with a new value, then return the old value"
    (lisp :a (data cell)
      (cl:let* ((inner (unveil (cl:slot-value cell '_0)))
               (old (cell-internal-inner inner)))
        (cl:setf (cell-internal-inner inner) data)
        old)))

  (declare cell-write (:a -> (Cell :a) -> Unit))
  (define (cell-write data cell)
    "Set the value of a mutable cell"
    (progn
      (cell-swap data cell)
      Unit))

  (declare cell-update ((:a -> :a) -> (Cell :a) -> Unit))
  (define (cell-update f cell)
    (cell-write (f (cell-read cell)) cell))

  (define-instance (Show :a => (Show (Cell :a)))
    (define (show x)
      (concat-string (concat-string "Cell<" (show (cell-read x))) ">")))

  (define-instance (Eq :a => (Eq (Cell :a)))
    (define (== c1 c2)
      (== (cell-read c1) (cell-read c2)))
    (define (/= c1 c2)
      (not (== c1 c2))))

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
      (make-cell (f (cell-read c1) (cell-read c2))))))
