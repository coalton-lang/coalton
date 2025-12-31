(coalton-library/utils:defstdlib-package #:coalton-library/cell
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes)
  (:local-nicknames
   (#:compat #:coalton-compatibility))
  (:export
   #:Cell
   #:new
   #:read
   #:swap!
   #:write!
   #:update!
   #:update-swap!
   #:push!
   #:pop!
   #:increment!
   #:decrement!))

(in-package #:coalton-library/cell)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(cl:declaim (cl:inline make-cell-internal))

(cl:defstruct cell-internal
  (inner (cl:error "") :type cl:t))

(cl:defmethod cl:print-object ((self cell-internal) stream)
  (cl:format stream "#.(CELL ~A)" (cell-internal-inner self))
  self)

(compat:try-freeze-type cell-internal)

(coalton-toplevel

  (repr :native cell-internal)
  (define-type (Cell :a)
    "Internally mutable cell")

  (inline)
  (declare new (:a -> Cell :a))
  (define (new data)
    "Create a new mutable cell containing `data`."
    (lisp (Cell :a) (data)
      (make-cell-internal :inner data)))

  (inline)
  (declare read (Cell :a -> :a))
  (define (read cel)
    "Read the value of a mutable cell `cel`."
    (lisp :a (cel)
      (cell-internal-inner cel)))

  (declare swap! (Cell :a -> :a -> :a))
  (define (swap! cel data)
    "Replace the value of a mutable cell `cel` with a new value `data`,
then return the old value."
    (lisp :a (data cel)
      (cl:let* ((old (cell-internal-inner cel)))
        (cl:setf (cell-internal-inner cel) data)
        old)))

  (inline)
  (declare write! (Cell :a -> :a -> :a))
  (define (write! cel data)
    "Set the value of a mutable cell `cel` to `data`, returning the new
value."
    (lisp :a (data cel)
      (cl:setf (cell-internal-inner cel) data)))

  (inline)
  (declare update! ((:a -> :a) -> Cell :a -> :a))
  (define (update! f cel)
    "Apply `f` to the contents of `cel`, storing and returning the result."
    (write! cel (f (read cel))))

  (declare update-swap! ((:a -> :a) -> Cell :a -> :a))
  (define (update-swap! f cel)
    "Apply `f` to the contents of `cel`, swapping the result for the old
value."
    (swap! cel (f (read cel))))

;;; operators on cells of lists
  (declare push! (Cell (List :elt) -> :elt -> List :elt))
  (define (push! cel new-elt)
    "Push `new-elt` onto the start of the list in `cel`."
    (update! (Cons new-elt) cel))

  (declare pop! (Cell (List :elt) -> Optional :elt))
  (define (pop! cel)
    "Remove and return the first element of the list in `cel`."
    (match (read cel)
      ((Cons fst rst)
       (write! cel rst)
       (Some fst))
      ((Nil) None)))

;;; operators on cells of numbers
  (inline)
  (declare increment! (Num :counter => Cell :counter -> :counter))
  (define (increment! cel)
    "Add one to the contents of `cel`, storing and returning the new value."
    (update! (+ 1) cel))

  (inline)
  (declare decrement! (Num :counter => (Cell :counter) -> :counter))
  (define (decrement! cel)
    "Subtract one from the contents of `cel`, storing and returning the new
value."
    (update! (+ -1) cel))

  ;; i am very skeptical of these instances
  (define-instance (Eq :a => Eq (Cell :a))
    (inline)
    (define (== c1 c2)
      (== (read c1) (read c2))))

  (define-instance (Ord :a => Ord (Cell :a))
    (inline)
    (define (<=> c1 c2)
      (<=> (read c1) (read c2))))

  (define-instance (Num :a => Num (Cell :a))
    (define (+ c1 c2)
      (new (+ (read c1) (read c2))))
    (define (- c1 c2)
      (new (- (read c1) (read c2))))
    (define (* c1 c2)
      (new (* (read c1) (read c2))))
    (define (fromInt i)
      (new (fromInt i))))

  (define-instance (Semigroup :a => Semigroup (Cell :a))
    (inline)
    (define (<> a b)
      (new (<> (read a) (read b)))))

  (define-instance (Functor Cell)
    (define (map f c)
      (new (f (read c)))))

  (define-instance (Applicative Cell)
    (inline)
    (define pure new)
    (define (liftA2 f c1 c2)
      (new (f (read c1) (read c2)))))

  (define-instance (Into :a (Cell :a))
    (inline)
    (define into new))

  (define-instance (Into (Cell :a) :a)
    (inline)
    (define into read))

  (define-instance (Into :a String => Into (Cell :a) String)
    (inline)
    (define (into c)
      (into (read c))))

  (define-instance (Default :a => Default (Cell :a))
    (inline)
    (define (default) (new (default)))))

(compat:try-lock-package "COALTON-LIBRARY/CELL")
