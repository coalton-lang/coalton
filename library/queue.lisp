(coalton/utils:defstdlib-package #:coalton/queue
  (:use
   #:coalton
   #:coalton/builtin
   #:coalton/functions
   #:coalton/classes)
  (:local-nicknames
   (#:cell #:coalton/cell)
   (#:iter #:coalton/iterator))
  (:export
   #:Queue
   #:new
   #:length
   #:empty?
   #:copy
   #:clear!
   #:push!
   #:pop!
   #:pop-unsafe!
   #:peek
   #:peek-unsafe
   #:index
   #:index-unsafe
   #:append
   #:extend!
   #:items!))

(in-package #:coalton/queue)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(cl:defstruct queue-internal
  (elements cl:nil :type cl:list)
  (last     cl:nil :type (cl:or cl:null (cl:cons cl:t cl:null)))
  (length        0 :type (cl:and cl:fixnum cl:unsigned-byte)))

(cl:defmethod cl:print-object ((self queue-internal) stream)
  (cl:format stream "#.(QUEUE~{ ~A~})" (queue-internal-elements self))
  self)

#+sbcl
(cl:declaim (sb-ext:freeze-type queue-internal))

(coalton-toplevel

  ;;
  ;; Queue
  ;;

  (repr :native queue-internal)
  (define-type (Queue :a)
    "Unbounded FIFO queue implemented with a linked list.")

  (declare new (Unit -> Queue :a))
  (define (new)
    "Create a new empty queue."
    (lisp (Queue :a) ()
      (make-queue-internal)))

  (declare length (Queue :a -> UFix))
  (define (length q)
    "Returns the length of `q`."
    (lisp UFix (q)
      (queue-internal-length q)))

  (declare empty? (Queue :a -> Boolean))
  (define (empty? q)
    "Is `q` empty?"
    (lisp Boolean (q)
      (cl:null (queue-internal-elements q))))

  (declare copy (Queue :a -> Queue :a))
  (define (copy q)
    "Return a new queue containing the same elements as `q`."
    (lisp (Queue :a) (q)
      (cl:multiple-value-bind (elements last)
          ;; Copy the list, holding on to a reference to the last cons
          (cl:loop
             :with head := (cl:cons (cl:car (queue-internal-elements q)) cl:nil)
             :with tail := head
             :for e :in (cl:cdr (queue-internal-elements q))
             :for new-cons := (cl:cons e nil)
             :do (cl:setf (cl:cdr tail) new-cons
                          tail new-cons)
             :finally (cl:return (cl:values head tail)))
        (make-queue-internal
         :elements elements
         :last last
         :length (queue-internal-length q)))))

  (declare clear! (Queue :a -> Unit))
  (define (clear! q)
    "Clear all elements from `q`."
    (lisp Unit (q)
      (cl:setf (queue-internal-elements q) cl:nil
               (queue-internal-last q) cl:nil
               (queue-internal-length q) 0)
      Unit))

  (declare push! (:a -> Queue :a -> Unit))
  (define (push! item q)
    "Push `item` onto the end of `q`."
    (lisp Unit (item q)
      (cl:let ((last (cl:cons item cl:nil)))
        (cl:if (cl:null (queue-internal-elements q))
               ;; Set up the queue with the first element. Note that the same
               ;; reference to the singleton list is shared by both
               ;; QUEUE-INTERNAL-ELEMENTS and QUEUE-INTERNAL-LAST.
               (cl:setf (queue-internal-elements q) last
                        (queue-internal-last q) last)

               ;; We can now append elements to QUEUE-INTERNAL-ELEMENTS simply by
               ;; modifying QUEUE-INTERNAL-LAST, whose reference is shared by
               ;; QUEUE-INTERNAL-ELEMENTS,
               ;;
               ;; We do this instead of a single SETF for type safety of
               ;; QUEUE-INTERNAL-LAST.
               (cl:let ((old (queue-internal-last q)))
                 (cl:setf (queue-internal-last q) last
                          (cl:cdr old) last)))

        (cl:incf (queue-internal-length q)))

      Unit))

  (declare pop! (Queue :a -> Optional :a))
  (define (pop! q)
    "Remove and return the first item of `q`."
    (if (empty? q)
        None
        (Some (pop-unsafe! q))))

  (declare pop-unsafe! (Queue :a -> :a))
  (define (pop-unsafe! q)
    "Remove and return the first item of `q` without checking if the queue is empty."
    (lisp :a (q)
      (cl:prog1
          (cl:or (cl:pop (queue-internal-elements q))
                 (cl:error "pop-unsafe! called on an empty Queue."))
        (cl:decf (queue-internal-length q)))))

  (declare peek (Queue :a -> Optional :a))
  (define (peek q)
    "Peek at the first item of `q`."
    (if (empty? q)
        None
        (Some (peek-unsafe q))))

  (declare peek-unsafe (Queue :a -> :a))
  (define (peek-unsafe q)
    "Peek at the first item of `q` without checking if the queue is empty."
    (lisp :a (q)
      (cl:or (cl:car (queue-internal-elements q))
             (cl:error "peek-unsafe called on an empty Queue."))))

  (declare index (UFix -> Queue :a -> Optional :a))
  (define (index index q)
    "Return the `index`th element of `q`."
    (if (>= index (length q))
        None
        (Some (index-unsafe index q))))

  (declare index-unsafe (UFix -> Queue :a -> :a))
  (define (index-unsafe index q)
    "Return the `index`th element of `q` without checking if the element exists."
    (lisp :a (index q)
      (cl:nth index (queue-internal-elements q))))

  (declare append (Queue :a -> Queue :a -> Queue :a))
  (define (append q1 q2)
    "Create a new queue containing the elements of `q1` followed by the elements of `q2`."
    (let out = (new))
    (extend! out q1)
    (extend! out q2)
    out)

  (declare extend! (iter:IntoIterator :container :elt => Queue :elt -> :container -> Unit))
  (define (extend! q iter)
    "Push every element in `iter` to the end of `q`."
    (let iter = (iter:into-iter iter))

    (iter:for-each!
     (fn (x)
       (push! x q)
       Unit)
     iter))

  (declare items! (Queue :a -> iter:Iterator :a))
  (define (items! q)
    "Returns an iterator over the items of `q`, removing items as they are returned."
    (iter:with-size
        (fn ()
          (pop! q))
      (length q)))

  ;;
  ;; Instances
  ;;

  (define-instance (Eq :a => Eq (Queue :a))
    (define (== q1 q2)
      (if (/= (length q1) (length q2))
          False
          (iter:every! id (iter:zip-with! == (iter:into-iter q1) (iter:into-iter q2))))))

  (define-instance (Semigroup (Queue :a))
    (define <> append))

  (define-instance (Functor Queue)
    (define (map f q)
      (let out = (new))
      (iter:for-each!
       (fn (x)
         (push! (f x) out)
         Unit)
       (iter:into-iter q))
      out))

  (define-instance (Foldable Queue)
    (define (fold f init q)
      (lisp :a (f init q)
        (cl:reduce
         (cl:lambda (b a)
           (call-coalton-function f b a))
         (queue-internal-elements q)
         :initial-value init)))
    (define (foldr f init q)
      (lisp :a (f init q)
        (cl:reduce
         (cl:lambda (a b)
           (call-coalton-function f a b))
         (queue-internal-elements q)
         :initial-value init
         :from-end cl:t))))

  (define-instance (iter:IntoIterator (Queue :a) :a)
    (define (iter:into-iter q)
      (let idx = (cell:new 0))
      (iter:with-size
          (fn ()
            (let res = (index (cell:read idx) q))
            (cell:increment! idx)
            res)
        (length q))))

  (define-instance (iter:FromIterator (Queue :a) :a)
    (define (iter:collect! iter)
      (let out = (new))
      (extend! out iter)
      out))

  (define-instance (Default (Queue :a))
    (define default new)))

#+sb-package-locks
(sb-ext:lock-package "COALTON/QUEUE")
