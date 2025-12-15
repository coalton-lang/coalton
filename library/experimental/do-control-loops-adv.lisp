(coalton-library/utils:defstdlib-package #:coalton-library/experimental/do-control-loops-adv
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/functions)
  (:local-nicknames
   (:l #:coalton-library/list)
   (:ct #:coalton-library/experimental/do-control-core)
   (#:compat #:coalton-compatibility))
  (:import-from #:coalton-library/monad/environment
   #:MonadEnvironment
   #:ask
   #:local
   #:asks)
  (:import-from #:coalton-library/monad/statet
   #:MonadState
   #:get
   #:put
   #:modify)
  (:export
   ;;
   ;; LoopT
   ;;
   #:LoopT

   ;;
   ;; Loop Commands
   ;;
   #:unwrap-loop
   #:break-loop
   #:continue-loop

   ;;
   ;; Looping Control Flow
   ;;
   #:loop_
   #:do-loop

   #:loop-while
   #:do-loop-while

   #:loop-do-while
   #:do-loop-do-while

   #:loop-times
   #:do-loop-times

   #:collect
   #:do-collect

   #:collect-val
   #:do-collect-val

   #:foreach
   #:do-foreach

   #:once
   #:do-once))

(in-package  #:coalton-library/experimental/do-control-loops-adv)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;;
;;; LoopT Monad Transformer
;;;

(coalton-toplevel

  (define-type (Step :a)
    Continue%
    Break%
    (Value% :a))

  (repr :transparent)
  (define-type (LoopT :m :a)
    (LoopT (:m (Step :a))))

  (inline)
  (declare unwrap-loop (LoopT :m :a -> :m (Step :a)))
  (define (unwrap-loop (LoopT m-stp))
    "Advance a LoopT computation by one step, returning whether it asked to continue,
break, or produced a value."
    m-stp)

  (inline)
  (declare break-loop (Monad :m => LoopT :m :a))
  (define break-loop
    "Signal that the loop should terminate immediately."
    (LoopT (pure Break%)))

  (inline)
  (declare continue-loop (Monad :m => LoopT :m :a))
  (define continue-loop
    "Signal that the current iteration should be skipped and the loop should continue."
    (LoopT (pure Continue%)))

  (define-instance (Functor :m => Functor (LoopT :m))
    (inline)
    (define (map fa->b (LoopT m-stp-a))
      (LoopT
       (map
        (fn (stp)
          (match stp
            ((Continue%) Continue%)
            ((Break%) Break%)
            ((Value% a) (Value% (fa->b a)))))
        m-stp-a))))

  (define-instance (Monad :m => Applicative (LoopT :m))
    (inline)
    (define (pure a)
      (LoopT (pure (Value% a))))
    (inline)
    (define (liftA2 fa->b->c (LoopT m-stp-a) (LoopT m-stp-b))
      (LoopT
       (do
        (stp-a <- m-stp-a)
        (match stp-a
          ((Continue%) (pure Continue%))
          ((Break%) (pure Break%))
          ((Value% a)
           (do
            (stp-b <- m-stp-b)
            (match stp-b
              ((Continue%) (pure Continue%))
              ((Break%) (pure Break%))
              ((Value% b)
               (pure (Value% (fa->b->c a b))))))))))))

  (define-instance (Monad :m => Monad (LoopT :m))
    (inline)
    (define (>>= (LoopT m-stp-a) fa->lpt-m-stp-a)
      (LoopT
       (do
        (stp-a <- m-stp-a)
        (match stp-a
          ((Break%) (pure Break%))
          ((Continue%) (pure Continue%))
          ((Value% a)
           (unwrap-loop (fa->lpt-m-stp-a a))))))))

  (inline)
  (declare map-loopT ((:m (Step :a) -> :n (Step :b)) -> LoopT :m :a -> LoopT :n :b))
  (define (map-loopT fm-stp-a->n-stp-b (LoopT m-stp-a))
    (LoopT
     (fm-stp-a->n-stp-b m-stp-a)))

  (inline)
  (declare lift-loopT (Functor :m => :m :a -> LoopT :m :a))
  (define (lift-loopT ma)
    (LoopT (map Value% ma)))

  (define-instance (MonadTransformer LoopT)
    (define lift lift-loopT))

  ;;
  ;; Standard Library Instances
  ;;

  (define-instance (MonadEnvironment :e :m => MonadEnvironment :e (LoopT :m))
    (define ask (lift ask))
    (define asks (compose lift asks))
    (define local (compose map-loopT local)))

  (define-instance (MonadState :s :m => MonadState :s (LoopT :m))
    (define get (lift get))
    (define put (compose lift put))
    (define modify (compose lift modify))))

;;;
;;; Loop Controls
;;;

(coalton-toplevel
  (declare loop_ (Monad :m => LoopT :m :a -> :m Unit))
  (define (loop_ body)
    "Run BODY forever, until it signals a break. Any produced values are ignored. Returns Unit."
    (do
     (r <- (unwrap-loop body))
     (match r
       ((Break%) (pure Unit))
       (_ (loop_ body)))))

  (declare loop-while ((Monad :m) (ct::Terminator :t) => LoopT :m :t -> :m Unit))
  (define (loop-while body)
    "Run BODY repeatedly until it returns a terminated value. Returns Unit."
    (do
     (r <- (unwrap-loop body))
     (match r
       ((Break%) (pure Unit))
       ((Continue%) (loop-while body))
       ((Value% t)
        (if (ct::ended? t)
            (pure Unit)
            (loop-while body))))))

  (declare loop-do-while ((Monad :m) (ct::Terminator :t) => :m :t -> LoopT :m :a -> :m Unit))
  (define (loop-do-while m-term? body)
    "Before each iteration, evaluate M-TERM?. If it indicates completion, stop; otherwise run BODY.
Respects break and continue within BODY. Returns Unit."
    (do
     (term? <- m-term?)
     (if (ct::ended? term?)
         (pure Unit)
         (do
          (r <- (unwrap-loop body))
          (match r
            ((Break%) (pure Unit))
            (_ (loop-do-while m-term? body)))))))

  (declare loop-times (Monad :m => UFix -> (UFix -> LoopT :m :a) -> :m Unit))
  (define (loop-times n body)
    "Repeat BODY N times. Passes the current index (starting at 0) to BODY.
Returns Unit."
    (rec % ((i 0))
      (if (== i n)
          (pure Unit)
          (do
           (step <- (unwrap-loop (body i)))
           (match step
             ((Break%) (pure Unit))
             ((Continue%) (% (+ 1 i)))
             ((Value% _) (% (+ 1 i))))))))

  (declare collect (Monad :m => LoopT :m :a -> :m (List :a)))
  (define (collect body)
    "Run BODY in a loop, collecting each value it produces into a list in encounter order.
Stops when BODY breaks. Continues skip the rest of the iteration. Returns the collected list."
    (rec % ((result mempty))
      (do
       (r <- (unwrap-loop body))
       (match r
         ((Break%) (pure (l:reverse result)))
         ((Continue%) (% result))
         ((Value% val)
          (% (Cons val result)))))))

  (declare collect-val ((Monad :m) (ct::Yielder :y) => LoopT :m (:y :a) -> :m (List :a)))
  (define (collect-val body)
    "Run BODY in a loop, adding each available value it yields to a list.
Stops when BODY yields no value or breaks. Continue skips the rest of the iteration.
Returns the collected list."
    (rec % ((result mempty))
      (do
       (r <- (unwrap-loop body))
       (match r
         ((Break%) (pure (l:reverse result)))
         ((Continue%) (% result))
         ((Value% val?)
          (match (ct::yield val?)
            ((Some x)
             (% (Cons x result)))
            ((None)
             (pure (l:reverse result)))))))))

  (declare foreach (Monad :m => List :a -> (:a -> LoopT :m :z) -> :m Unit))
  (define (foreach lst fa->lpt-m)
    "For each element of LST, run FA->LPT-M on it. Break stops the iteration.
Continue skips to the next element. Discards return values and returns Unit."
    (rec % ((rem lst))
      (match rem
        ((Nil) (pure Unit))
        ((Cons a rst)
         (do
          (r <- (unwrap-loop (fa->lpt-m a)))
          (match r
            ((Break%) (pure Unit))
            (_ (% rst))))))))

  (inline)
  (declare once (Monad :m => LoopT :m :a -> :m Unit))
  (define (once lp-m)
    "Run an operation exactly once. Continue or break will both immediately end
execution in the operation. Returns Unit."
    (do
     (unwrap-loop lp-m)
     (pure Unit))))

(cl:defmacro do-loop (cl:&body body)
  "Run BODY (in a 'do' block) forever until it signals a break. Any produced
values are ignored. Returns Unit."
  `(loop_
    (do
     ,@body)))

(cl:defmacro do-loop-while (cl:&body body)
  "Run BODY (in a 'do' block) repeatedly until it returns a terminated value.
Returns Unit."
  `(loop-while
    (do
     ,@body)))

(cl:defmacro do-loop-do-while (test cl:&body body)
  "Before each iteration, evaluate TEST. If it indicates completion, stop.
Otherwise run BODY (in a 'do' block). Respects break and continue within BODY.
Returns Unit."
  `(loop-do-while ,test
    (do
     ,@body)))

(cl:defmacro do-loop-times ((sym n) cl:&body body)
    "Run BODY (in a 'do' block) N times. Binds the current index (starting at 0) to SYM.
Respects break and continue in BODY. Returns Unit."
  `(loop-times ,n
    (fn (,sym)
      (do
       ,@body))))

(cl:defmacro do-collect (cl:&body body)
  "Run BODY in a loop and collect each produced value into a list."
  `(collect
     (do
       ,@body)))

(cl:defmacro do-collect-val (cl:&body body)
    "Run BODY (in a 'do' block) in a loop, adding each available value it
yields to a list. Stops when BODY yields no value or breaks. Continus
skips the rest of the iteration. Returns the collected list."
  `(collect-val
    (do
     ,@body)))

(cl:defmacro do-foreach ((sym lst) cl:&body body)
  "For each element of LST, bind it to SYM and run BODY (in a 'do' block) on it.
Break stops the iteration. Continue skips to the next element. Discards return
values and returns Unit."
  `(foreach ,lst
    (fn (,sym)
      (do
       ,@body))))

(cl:defmacro do-once (cl:&body body)
    "Run BODY (in a 'do' block) exactly once. Continue or break will both
immediately end execution in the operation. Returns Unit."
  `(once
    (do
     ,@body)))

(compat:try-lock-package "COALTON-LIBRARY/EXPERIMENTAL/DO-CONTROL-LOOPS-ADV")
