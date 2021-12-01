(in-package #:coalton-library)

(coalton-toplevel

  ;;
  ;; Vector
  ;;

  (define-type (Vector :a)
    (Vector Lisp-Object))

  (declare make-vector (Unit -> (Vector :a)))
  (define (make-vector _)
    "Create a new empty vector"
    (make-vector-capacity 0))

  (declare make-vector-capacity (Integer -> (Vector :a)))
  (define (make-vector-capacity n)
    "Create a new vector with N elements preallocated"
    (lisp (Vector :a) (n)
      (Vector (cl:make-array n :fill-pointer 0 :adjustable cl:t))))

  (declare vector-length ((Vector :a) -> Integer))
  (define (vector-length v)
    "Returns the length of V"
    (match v
      ((Vector v)
       (lisp Integer (v)
         (cl:fill-pointer v)))))

  (declare vector-capacity ((Vector :a) -> Integer))
  (define (vector-capacity v)
    "Returns the number of elements that V can store without resizing"
    (match v
      ((Vector v)
       (lisp Integer (v)
         (cl:array-dimension v 0)))))

  (declare vector-empty ((Vector :a) -> Boolean))
  (define (vector-empty v)
    "Returns TRUE if V is empty"
    (== 0 (vector-length v)))

  (declare vector-copy ((Vector :a) -> (Vector :a)))
  (define (vector-copy v)
    "Return a new vector containing the same elements as V"
    (match v
      ((Vector v)
        (lisp (Vector :a) (v)
          (Vector (alexandria:copy-array v))))))

  (declare vector-push (:a -> (Vector :a) -> Integer))
  (define (vector-push item v)
    "Append ITEM to V and resize V if necessary"
    (match v
      ((Vector v)
       (lisp Integer (item v)
         (cl:progn
	   (cl:vector-push-extend item v)
	   (cl:1- (cl:fill-pointer v)))))))

  (declare vector-pop ((Vector :a) -> (Optional :a)))
  (define (vector-pop v)
    "Remove and return the first item of V"
    (if (== 0 (vector-length v))
        None
        (Some (vector-pop-unsafe v))))

  (declare vector-pop-unsafe ((Vector :a) -> :a))
  (define (vector-pop-unsafe v)
    "Remove and return the first item of V without checking if the vector is empty"
    (match v
      ((Vector v)
       (lisp :a (v)
         (cl:vector-pop v)))))

  (declare vector-index (Integer -> (Vector :a) -> (Optional :a)))
  (define (vector-index index v)
    "Return the INDEXth element of V"
    (if (>= index (vector-length v))
        None
        (Some (vector-index-unsafe index v))))

  (declare vector-index-unsafe (Integer -> (Vector :a) -> :a))
  (define (vector-index-unsafe index v)
    "Return the INDEXth element of V without checking if the element exists"
    (match v
      ((Vector v)
       (lisp :a (index v)
         (cl:aref v index)))))

  (declare vector-set (Integer -> :a -> (Vector :a) -> Unit))
  (define (vector-set index item v)
    "Set the INDEXth element of V to ITEM. This function left intentionally unsafe because it does not have a return value to check."
    (match v
      ((Vector v)
       (lisp Unit (index item v)
         (cl:progn
           (cl:setf (cl:aref v index) item)
           Unit)))))

  (declare vector-head ((Vector :a) -> (Optional :a)))
  (define (vector-head v)
    "Return the first item of V"
    (vector-index 0 v))

  (declare vector-head-unsafe ((Vector :a) -> :a))
  (define (vector-head-unsafe v)
    "Return the first item of V without first checking if V is empty"
    (vector-index-unsafe 0 v))

  (declare vector-last ((Vector :a) -> (Optional :a)))
  (define (vector-last v)
    "Return the last element of V"
    (vector-index (- (vector-length v) 1) v))

  (declare vector-last-unsafe ((Vector :a) -> :a))
  (define (vector-last-unsafe v)
    "Return the last element of V without first checking if V is empty"
    (vector-index-unsafe (- (vector-length v) 1) v))

  (declare vector-find-elem (Eq :a => (:a -> (Vector :a) -> (Optional Integer))))
  (define (vector-find-elem e v)
    "Find the index of element E in V"
    (match v
      ((Vector v)
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

  (declare vector-foreach ((:a -> :b) -> (Vector :a) -> Unit))
  (define (vector-foreach f v)
    "Call the function F once for each item in V"
    (match v
      ((Vector v)
       (lisp Unit (f v)
         (cl:progn
           (cl:loop :for elem :across v
              :do (coalton-impl/codegen::A1 f elem))
           Unit)))))

  (declare vector-foreach-index ((Integer -> :a -> :b) -> (Vector :a) -> Unit))
  (define (vector-foreach-index f v)
    "Call the function F once for each item in V with its index"
    (match v
      ((Vector v)
       (lisp Unit (f v)
         (cl:progn
           (cl:loop
              :for elem :across v
              :for i :from 0
              :do (coalton-impl/codegen::A2 f i elem))
           Unit)))))

  (declare vector-foreach2 ((:a -> :b -> :c) -> (Vector :a) -> (Vector :b) -> Unit))
  (define (vector-foreach2 f v1 v2)
    "Like vector-foreach but twice as good"
    (match (Tuple v1 v2)
      ((Tuple (Vector v1) (Vector v2))
       (lisp Unit (f v1 v2)
         (cl:progn
           (cl:loop
              :for e1 :across v1
              :for e2 :across v2
              :do (coalton-impl/codegen::A2 f e1 e2))
           Unit)))))

  (declare vector-append ((Vector :a) -> (Vector :a) -> (Vector :a)))
  (define (vector-append v1 v2)
    "Create a new VECTOR containing the elements of v1 followed by the elements of v2"
    (progn
      (let out = (make-vector-capacity (+ (vector-length v1) (vector-length v2))))
      (let f =
        (fn (item)
          (vector-push item out)))

      (vector-foreach f v1)
      (vector-foreach f v2)
      out))

  (declare vector-swap-remove (Integer -> (Vector :a) -> (Optional :a)))
  (define (vector-swap-remove idx vec)
    "Remove the element IDX from VEC and replace it with the last element in VEC. Then return the removed element."
    (if (>= idx (vector-length vec))
        None
        (Some (vector-swap-remove-unsafe idx vec))))

  (declare vector-swap-remove-unsafe (Integer -> (Vector :a) -> :a))
  (define (vector-swap-remove-unsafe idx vec)
    "Remove the element IDX from VEC and replace it with the last element in VEC without bounds checking. Then return the removed element."
    (if (== (+ 1 idx) (vector-length vec))
        (vector-pop-unsafe vec)
        (progn
          (let out = (vector-index-unsafe idx vec))
          (vector-set idx (vector-pop-unsafe vec) vec)
          out)))

  (declare vector-sort-by ((:a -> :a -> Boolean) -> (Vector :a) -> Unit))
  (define (vector-sort-by f v)
    "Sort a vector with predicate function F"
    (match v
      ((Vector v)
       (progn
         (lisp :a (v f)
           (cl:sort
            v
            (cl:lambda (a b)
              (coalton-impl/codegen::A2 f a b))))
         Unit))))

  (declare vector-sort (Ord :a => ((Vector :a) -> Unit)))
  (define (vector-sort v)
    "Sort a vector inplace"
    (vector-sort-by < v))


  ;;
  ;; Vector Instances
  ;;

  (define-instance (Eq :a => (Eq (Vector :a)))
    (define (== v1 v2)
      (if (/= (vector-length v1) (vector-length v2))
          False
          (progn
            (let out = (make-cell True))
            (vector-foreach2
             (fn (e1 e2)
               (unless (== e1 e2)
                 (cell-write False out)))
             v1 v2)
            (cell-read out))))
    (define (/= v1 v2)
      (not (== v1 v2))))

  (define-instance (Semigroup (Vector :a))
    (define (<> v1 v2)
      (vector-append v1 v2)))

  (define-instance (Functor Vector)
    (define (map f v)
      (progn
        (let out = (make-vector-capacity (vector-length v)))
        (vector-foreach
         (fn (item)
           (vector-push (f item) out))
         v)
        out)))

  (define-instance (Into (List :a) (Vector :a))
    (define (into lst)
      (progn
        (let out = (make-vector-capacity (length lst)))
        (let inner =
          (fn (lst)
            (match lst
              ((Cons x xs)
               (progn
                 (vector-push x out)
                 (inner xs)))
              ((Nil) Unit))))
        (inner lst)
        out)))

  (define-instance (Into (Vector :a) (List :a))
    (define (into v)
      (let ((inner
            (fn (v index)
              (if (>= index (vector-length v))
                  Nil
                  (Cons (vector-index-unsafe index v) (inner v (+ 1 index)))))))
      (inner v 0))))

  (define-instance (Iso (Vector :a) (List :a))))
