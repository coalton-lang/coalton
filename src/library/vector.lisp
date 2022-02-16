(coalton-library/utils:defstdlib-package #:coalton-library/vector
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/functions
   #:coalton-library/classes)
  (:local-nicknames
   (#:list #:coalton-library/list)
   (#:cell #:coalton-library/cell))
  (:export
   #:Vector
   #:new
   #:with-capacity
   #:length
   #:capacity
   #:empty?
   #:copy
   #:push!
   #:pop!
   #:pop-unsafe!
   #:index
   #:index-unsafe
   #:set!
   #:head
   #:head-unsafe
   #:last
   #:last-unsafe
   #:find-elem
   #:foreach
   #:foreach-index
   #:foreach2
   #:append
   #:swap-remove!
   #:swap-remove-unsafe!
   #:sort!
   #:sort-by!))

(cl:in-package #:coalton-library/vector)

(coalton-toplevel

  ;;
  ;; Vector
  ;;

  (define-type (Vector :a)
    (%Vector Lisp-Object))

  (declare new (Unit -> (Vector :a)))
  (define (new _)
    "Create a new empty vector"
    (with-capacity 0))

  (declare with-capacity (Integer -> (Vector :a)))
  (define (with-capacity n)
    "Create a new vector with N elements preallocated"
    (lisp (Vector :a) (n)
      (%Vector (cl:make-array n :fill-pointer 0 :adjustable cl:t))))

  (declare length ((Vector :a) -> Integer))
  (define (length v)
    "Returns the length of V"
    (match v
      ((%Vector v)
       (lisp Integer (v)
         (cl:fill-pointer v)))))

  (declare capacity ((Vector :a) -> Integer))
  (define (capacity v)
    "Returns the number of elements that V can store without resizing"
    (match v
      ((%Vector v)
       (lisp Integer (v)
         (cl:array-dimension v 0)))))

  (declare empty? ((Vector :a) -> Boolean))
  (define (empty? v)
    "Returns TRUE if V is empty"
    (== 0 (length v)))

  (declare copy ((Vector :a) -> (Vector :a)))
  (define (copy v)
    "Return a new vector containing the same elements as V"
    (match v
      ((%Vector v)
       (lisp (Vector :a) (v)
         (%Vector (alexandria:copy-array v))))))

  (declare push! (:a -> (Vector :a) -> Integer))
  (define (push! item v)
    "Append ITEM to V and resize V if necessary"
    (match v
      ((%Vector v)
       (lisp Integer (item v)
         (cl:progn
	   (cl:vector-push-extend item v)
	   (cl:1- (cl:fill-pointer v)))))))

  (declare pop! ((Vector :a) -> (Optional :a)))
  (define (pop! v)
    "Remove and return the first item of V"
    (if (== 0 (length v))
        None
        (Some (pop-unsafe! v))))

  (declare pop-unsafe! ((Vector :a) -> :a))
  (define (pop-unsafe! v)
    "Remove and return the first item of V without checking if the vector is empty"
    (match v
      ((%Vector v)
       (lisp :a (v)
         (cl:vector-pop v)))))

  (declare index (Integer -> (Vector :a) -> (Optional :a)))
  (define (index index v)
    "Return the INDEXth element of V"
    (if (>= index (length v))
        None
        (Some (index-unsafe index v))))

  (declare index-unsafe (Integer -> (Vector :a) -> :a))
  (define (index-unsafe index v)
    "Return the INDEXth element of V without checking if the element exists"
    (match v
      ((%Vector v)
       (lisp :a (index v)
         (cl:aref v index)))))

  (declare set! (Integer -> :a -> (Vector :a) -> Unit))
  (define (set! index item v)
    "Set the INDEXth element of V to ITEM. This function left intentionally unsafe because it does not have a return value to check."
    (match v
      ((%Vector v)
       (lisp Unit (index item v)
         (cl:progn
           (cl:setf (cl:aref v index) item)
           Unit)))))

  (declare head ((Vector :a) -> (Optional :a)))
  (define (head v)
    "Return the first item of V"
    (index 0 v))

  (declare head-unsafe ((Vector :a) -> :a))
  (define (head-unsafe v)
    "Return the first item of V without first checking if V is empty"
    (index-unsafe 0 v))

  (declare last ((Vector :a) -> (Optional :a)))
  (define (last v)
    "Return the last element of V"
    (index (- (length v) 1) v))

  (declare last-unsafe ((Vector :a) -> :a))
  (define (last-unsafe v)
    "Return the last element of V without first checking if V is empty"
    (index-unsafe (- (length v) 1) v))

  (declare find-elem (Eq :a => (:a -> (Vector :a) -> (Optional Integer))))
  (define (find-elem e v)
    "Find the index of element E in V"
    (match v
      ((%Vector v)
       (let ((test (fn (elem)
                     (== elem e))))

         (progn
           (lisp (Optional Integer) (v test)
             (cl:let ((pos (cl:position-if
                            #'(cl:lambda (x)
                                (cl:equalp True (coalton-impl/codegen::A1 test x)))
                            v)))
               (cl:if pos
                      (Some pos)
                      None))))))))

  (declare foreach ((:a -> :b) -> (Vector :a) -> Unit))
  (define (foreach f v)
    "Call the function F once for each item in V"
    (match v
      ((%Vector v)
       (lisp Unit (f v)
         (cl:progn
           (cl:loop :for elem :across v
              :do (coalton-impl/codegen::A1 f elem))
           Unit)))))

  (declare foreach-index ((Integer -> :a -> :b) -> (Vector :a) -> Unit))
  (define (foreach-index f v)
    "Call the function F once for each item in V with its index"
    (match v
      ((%Vector v)
       (lisp Unit (f v)
         (cl:progn
           (cl:loop
              :for elem :across v
              :for i :from 0
              :do (coalton-impl/codegen::A2 f i elem))
           Unit)))))

  (declare foreach2 ((:a -> :b -> :c) -> (Vector :a) -> (Vector :b) -> Unit))
  (define (foreach2 f v1 v2)
    "Like vector-foreach but twice as good"
    (match (Tuple v1 v2)
      ((Tuple (%Vector v1) (%Vector v2))
       (lisp Unit (f v1 v2)
         (cl:progn
           (cl:loop
              :for e1 :across v1
              :for e2 :across v2
              :do (coalton-impl/codegen::A2 f e1 e2))
           Unit)))))

  (declare append ((Vector :a) -> (Vector :a) -> (Vector :a)))
  (define (append v1 v2)
    "Create a new VECTOR containing the elements of v1 followed by the elements of v2"
    (progn
      (let out = (with-capacity (+ (length v1) (length v2))))
      (let f =
        (fn (item)
          (push! item out)))

      (foreach f v1)
      (foreach f v2)
      out))

  (declare swap-remove! (Integer -> (Vector :a) -> (Optional :a)))
  (define (swap-remove! idx vec)
    "Remove the element IDX from VEC and replace it with the last element in VEC. Then return the removed element."
    (if (>= idx (length vec))
        None
        (Some (swap-remove-unsafe! idx vec))))

  (declare swap-remove-unsafe! (Integer -> (Vector :a) -> :a))
  (define (swap-remove-unsafe! idx vec)
    "Remove the element IDX from VEC and replace it with the last element in VEC without bounds checking. Then return the removed element."
    (if (== (+ 1 idx) (length vec))
        (pop-unsafe! vec)
        (progn
          (let out = (index-unsafe idx vec))
          (set! idx (pop-unsafe! vec) vec)
          out)))

  (declare sort-by! ((:a -> :a -> Boolean) -> (Vector :a) -> Unit))
  (define (sort-by! f v)
    "Sort a vector inplace with predicate function F"
    (match v
      ((%Vector v)
       (progn
         (lisp :a (v f)
           (cl:sort
            v
            (cl:lambda (a b)
              (coalton-impl/codegen::A2 f a b))))
         Unit))))

  (declare sort! (Ord :a => ((Vector :a) -> Unit)))
  (define (sort! v)
    "Sort a vector inplace"
    (sort-by! < v))


  ;;
  ;; Vector Instances
  ;;

  (define-instance (Eq :a => (Eq (Vector :a)))
    (define (== v1 v2)
      (if (/= (length v1) (length v2))
          False
          (progn
            (let out = (cell:new True))
            (foreach2
             (fn (e1 e2)
               (unless (== e1 e2)
                 (cell:write! out False)))
             v1 v2)
            (cell:read out)))))

  (define-instance (Semigroup (Vector :a))
    (define <> append))

  (define-instance (Functor Vector)
    (define (map f v)
      (progn
        (let out = (with-capacity (length v)))
        (foreach
         (fn (item)
           (push! (f item) out))
         v)
        out)))

  (define-instance (Into (List :a) (Vector :a))
    (define (into lst)
      (progn
        (let out = (with-capacity (list:length lst)))
        (let inner =
          (fn (lst)
            (match lst
              ((Cons x xs)
               (progn
                 (push! x out)
                 (inner xs)))
              ((Nil) Unit))))
        (inner lst)
        out)))

  (define-instance (Into (Vector :a) (List :a))
    (define (into v)
      (let ((inner
              (fn (v index)
                (if (>= index (length v))
                    Nil
                    (Cons (index-unsafe index v) (inner v (+ 1 index)))))))
        (inner v 0))))

  (define-instance (Iso (Vector :a) (List :a))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/VECTOR")
