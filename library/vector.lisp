(coalton-library/utils:defstdlib-package #:coalton-library/vector
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/functions
   #:coalton-library/classes)
  (:local-nicknames
   (#:types #:coalton-library/types)
   (#:list #:coalton-library/list)
   (#:cell #:coalton-library/cell))
  (:export
   #:Vector
   #:new
   #:with-capacity
   #:length
   #:capacity
   #:element-type
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
   #:sort-by!
   #:make))

(in-package #:coalton-library/vector)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel

  ;;
  ;; Vector
  ;;

  (repr :native (cl:vector cl:*))
  (define-type (Vector :a))

  (declare new (types:RuntimeRepr :a => Unit -> Vector :a))
  (define (new _)
    "Create a new empty vector"
    (with-capacity 0))

  (declare %with-capacity-specialized (types:LispType -> UFix -> Vector :a))
  (define (%with-capacity-specialized t n)
    "Create a new vector with N elements preallocated"
    (lisp (Vector :a) (n t)
      (cl:make-array n :fill-pointer 0 :adjustable cl:t :element-type t)))

  (declare with-capacity (types:RuntimeRepr :a => UFix -> Vector :a))
  (define (with-capacity n)
    "Create a new vector with N elements preallocated"
    (let p = types:Proxy)
    (let ((declare %proxy-helper (types:Proxy (Vector :a) -> types:Proxy :a))
          (%proxy-helper (fn (_) types:Proxy)))
      (let p_ = (%proxy-helper p))
      (let t = (types:runtime-repr p_))
      (types:as-proxy-of (%with-capacity-specialized t n) p)))

  (declare length (Vector :a -> UFix))
  (define (length v)
    "Returns the length of V"
    (lisp UFix (v)
      (cl:length v)))

  (declare capacity (Vector :a -> UFix))
  (define (capacity v)
    "Returns the number of elements that V can store without resizing"
    (lisp UFix (v)
      (cl:array-dimension v 0)))

  (declare element-type (Vector :a -> types:LispType))
  (define (element-type v)
    "Returns the element type of V as a LispType"
    (lisp types:LispType (v)
      (cl:array-element-type v)))

  (declare empty? (Vector :a -> Boolean))
  (define (empty? v)
    "Returns TRUE if V is empty"
    (== 0 (length v)))

  (declare copy (Vector :a -> Vector :a))
  (define (copy v)
    "Return a new vector containing the same elements as V"
    (lisp (Vector :a) (v)
      (alexandria:copy-array v)))

  (declare push! (:a -> Vector :a -> UFix))
  (define (push! item v)
    "Append ITEM to V and resize V if necessary, returning the index of the new item."
    (lisp UFix (item v)
      (cl:vector-push-extend item v)))

  (declare pop! (Vector :a -> Optional :a))
  (define (pop! v)
    "Remove and return the last item of V"
    (if (== 0 (length v))
        None
        (Some (pop-unsafe! v))))

  (declare pop-unsafe! (Vector :a -> :a))
  (define (pop-unsafe! v)
    "Remove and return the last item of V without checking if the vector is empty"
    (lisp :a (v)
      (cl:vector-pop v)))

  (declare index (UFix -> Vector :a -> Optional :a))
  (define (index index v)
    "Return the INDEXth element of V"
    (if (>= index (length v))
        None
        (Some (index-unsafe index v))))

  (declare index-unsafe (UFix -> Vector :a -> :a))
  (define (index-unsafe index v)
    "Return the INDEXth element of V without checking if the element exists"
    (lisp :a (index v)
      (cl:aref v index)))

  (declare set! (UFix -> :a -> Vector :a -> Unit))
  (define (set! index item v)
    "Set the INDEXth element of V to ITEM. This function left intentionally unsafe because it does not have a return value to check."
    (lisp Void (index item v)
      (cl:setf (cl:aref v index) item))
    Unit)

  (declare head (Vector :a -> Optional :a))
  (define (head v)
    "Return the first item of V"
    (index 0 v))

  (declare head-unsafe (Vector :a -> :a))
  (define (head-unsafe v)
    "Return the first item of V without first checking if V is empty"
    (index-unsafe 0 v))

  (declare last (Vector :a -> Optional :a))
  (define (last v)
    "Return the last element of V"
    (index (- (length v) 1) v))

  (declare last-unsafe (Vector :a -> :a))
  (define (last-unsafe v)
    "Return the last element of V without first checking if V is empty"
    (index-unsafe (- (length v) 1) v))

  (declare find-elem (Eq :a => :a -> Vector :a -> Optional UFix))
  (define (find-elem e v)
    "Find the index of element E in V"
    (let ((test (fn (elem)
                  (== elem e))))

      (lisp (Optional UFix) (v test)
        (cl:let ((pos (cl:position-if
                       (cl:lambda (x)
                         (cl:eq cl:t (call-coalton-function test x)))
                       v)))
          (cl:if pos
                 (Some pos)
                 None)))))

  (declare foreach ((:a -> :b) -> Vector :a -> Unit))
  (define (foreach f v)
    "Call the function F once for each item in V"
    (lisp Void (f v)
      (cl:loop :for elem :across v
         :do (call-coalton-function f elem)))
    Unit)

  (declare foreach-index ((UFix -> :a -> :b) -> Vector :a -> Unit))
  (define (foreach-index f v)
    "Call the function F once for each item in V with its index"
    (lisp Void (f v)
      (cl:loop
         :for elem :across v
         :for i :from 0
         :do (call-coalton-function f i elem)))
    Unit)

  (declare foreach2 ((:a -> :b -> :c) -> Vector :a -> Vector :b -> Unit))
  (define (foreach2 f v1 v2)
    "Iterate in parallel over V1 and V2 calling F once for each pair of elements. Iteration stops when the shorter vector runs out of elements."
    (lisp Void (f v1 v2)
      (cl:loop
         :for e1 :across v1
         :for e2 :across v2
         :do (call-coalton-function f e1 e2)))
    Unit)

  (declare append (types:RuntimeRepr :a => Vector :a -> Vector :a -> Vector :a))
  (define (append v1 v2)
    "Create a new VECTOR containing the elements of v1 followed by the elements of v2"
    (let out = (with-capacity (+ (length v1) (length v2))))
    (let f =
      (fn (item)
        (push! item out)))

    (foreach f v1)
    (foreach f v2)
    out)

  (declare swap-remove! (UFix -> Vector :a -> Optional :a))
  (define (swap-remove! idx vec)
    "Remove the element IDX from VEC and replace it with the last element in VEC. Then return the removed element."
    (if (>= idx (length vec))
        None
        (Some (swap-remove-unsafe! idx vec))))

  (declare swap-remove-unsafe! (UFix -> Vector :a -> :a))
  (define (swap-remove-unsafe! idx vec)
    "Remove the element IDX from VEC and replace it with the last element in VEC without bounds checking. Then return the removed element."
    (if (== (+ 1 idx) (length vec))
        (pop-unsafe! vec)
        (progn
          (let out = (index-unsafe idx vec))
          (set! idx (pop-unsafe! vec) vec)
          out)))

  (declare sort-by! ((:a -> :a -> Boolean) -> Vector :a -> Unit))
  (define (sort-by! f v)
    "Sort a vector inplace with predicate function F"
    (lisp Void (v f)
      (cl:sort
       v
       (cl:lambda (a b)
         (call-coalton-function f a b))))
    Unit)

  (declare sort! (Ord :a => Vector :a -> Unit))
  (define (sort! v)
    "Sort a vector inplace"
    (sort-by! < v))


  ;;
  ;; Vector Instances
  ;;

  (define-instance (Eq :a => Eq (Vector :a))
    (define (== v1 v2)
      (if (/= (length v1) (length v2))
          False
          (progn
            (let out = (cell:new True))
            (foreach2
             (fn (e1 e2)
               (unless (== e1 e2)
                 (cell:write! out False)
                 Unit))
             v1 v2)
            (cell:read out)))))

  (define-instance (types:RuntimeRepr :a => Semigroup (Vector :a))
    (define <> append))

  (define-instance (Functor Vector)
    (define (map f v)
      (let out = (%with-capacity-specialized
                  (lisp types:LispType (v)
                    (cl:array-element-type v))
                  (length v)))
      (foreach
       (fn (item)
         (push! (f item) out))
       v)
      out))

  (define-instance (Foldable Vector)
    (define (fold f init vec)
      (lisp :a (f init vec)
        (cl:reduce
         (cl:lambda (b a)
           (call-coalton-function f b a))
         vec
         :initial-value init)))
    (define (foldr f init vec)
      (lisp :a (f init vec)
        (cl:reduce
         (cl:lambda (a b)
           (call-coalton-function f a b))
         vec
         :initial-value init
         :from-end cl:t))))


  (define-instance (types:RuntimeRepr :a => Into (List :a) (Vector :a))
    (define (into lst)
      (let ((out (with-capacity (list:length lst)))
            (inner
              (fn (lst)
                (match lst
                  ((Cons x xs)
                   (progn
                     (push! x out)
                     (inner xs)))
                  ((Nil) Unit)))))
        (progn
          (inner lst)
          out))))

  (define-instance (types:RuntimeRepr :a => Into (Vector :a) (List :a))
    (define (into v)
      (let ((inner
              (fn (v index)
                (if (>= index (length v))
                    Nil
                    (Cons (index-unsafe index v) (inner v (+ 1 index)))))))
        (inner v 0))))

  (define-instance (types:RuntimeRepr :a => Iso (Vector :a) (List :a)))

  )

(cl:defmacro make (cl:&rest elements)
  "Construct a `Vector' containing the ELEMENTS, in the order listed."
  (cl:let* ((length (cl:length elements))
            (vec (cl:gensym "VEC-")))
    `(progn
       (let ,vec = (with-capacity ,length))
       ,@(cl:loop :for elt :in elements
            :collect `(push! ,elt ,vec))
       ,vec)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/VECTOR")
