(coalton-library/utils:defstdlib-package #:coalton-library/experimental/do-control-loops
  (:use
   #:coalton
   #:coalton-library/classes)
  (:local-nicknames
   (:l #:coalton-library/list)
   (:it #:coalton-library/iterator))
  (:import-from #:coalton-library/experimental/do-control-core
   #:Terminator
   #:ended?
   #:Yielder
   #:yield)
  (:export
   #:loop-while
   #:loop-while-valM
   #:loop-times
   #:collect-val
   #:foreach

   #:do-loop-while
   #:do-loop-while-valM
   #:do-loop-times
   #:do-collect-val
   #:do-foreach))

(in-package #:coalton-library/experimental/do-control-loops)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (declare loop-while ((Monad :m) (Terminator :t) => :m :t -> :m Unit))
  (define (loop-while m-operation)
    "Repeat M-OPERATION until it returns a terminated value. Returns Unit."
    (do
     (res <- m-operation)
     (if (ended? res)
         (pure Unit)
         (loop-while m-operation))))

  (declare loop-while-valM ((Monad :m) (Yielder :y) => :m (:y :a) -> (:a -> :m :b) -> :m Unit))
  (define (loop-while-valM m-operation f)
    "Repeat M-OPERATION while it yields a value, running the yielded value applied to F.
Returns Unit."
    (do
     (res <- m-operation)
     (match (yield res)
       ((Some x)
        (do
         (f x)
         (loop-while-valM m-operation f)))
       ((None)
        (pure Unit)))))

  (declare loop-times (Monad :m => UFix -> (UFix -> :m :a) -> :m Unit))
  (define (loop-times n m-operation)
    "Repeat M-OPERATION N times. Passes the current index (starting at 0) to
M-OPERATION. Returns Unit."
    (rec % ((i 0))
      (if (== i n)
          (pure Unit)
          (do
           (m-operation i)
           (% (+ 1 i))))))

  (inline)
  (declare collect-val ((Monad :m) (Yielder :y) => :m (:y :a) -> :m (List :a)))
  (define (collect-val m-operation)
    "Repeatedly run M-OPERATION, collecting each yielded value into a list until
no value is yielded."
    (rec % ((result mempty))
      (do
       (val? <- m-operation)
       (match (yield val?)
         ((Some x)
          (% (Cons x result)))
         ((None)
          (pure (l:reverse result)))))))

  (declare foreach ((Monad :m) (it:IntoIterator :i :a) => :i -> (:a -> :m :z) -> :m Unit))
  (define (foreach into-itr fa->m)
    "Apply FA->M to each element produced by INTO-ITR and run the resulting monadic action.
Discards the return values and returns Unit."
    (rec % ((itr (it:into-iter into-itr)))
      (match (it:next! itr)
        ((None) (pure Unit))
        ((Some a)
         (do
          (fa->m a)
          (% itr)))))))

;;
;; Loops
;;

(cl:defmacro do-loop-while (cl:&body body)
  "Run BODY repeatedly (in a 'do' block) until it returns a terminator that has ended."
  `(loop-while
    (do
     ,@body)))

(cl:defmacro do-loop-while-valM ((sym m-operation) cl:&body body)
    "Run M-OPERATION, bind its result to SYM, and run BODY in a 'do' block. Repeats while
M-OPERATION yields a value. Returns Unit."
  `(loop-while-valM ,m-operation
    (fn (,sym)
      (do
       ,@body))))

(cl:defmacro do-loop-times ((sym n) cl:&body body)
    "Run BODY (in a 'do' block) N times. Binds the current index (starting at 0) to SYM.
Returns Unit."
  `(loop-times ,n
    (fn (,sym)
      (do
       ,@body))))

(cl:defmacro do-collect-val (cl:&body body)
  "Run BODY repeatedly (in a 'do' block) collecting each yielded value into a list."
  `(collect-val
    (do
     ,@body)))

(cl:defmacro do-foreach ((sym into-itr) cl:&body body)
  "For each element of INTO-ITR, bind it to SYM and run BODY in a 'do' block.
Returns Unit."
  `(foreach ,into-itr
    (fn (,sym)
      (do
       ,@body))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/EXPERIMENTAL/DO-CONTROL-LOOPS")
