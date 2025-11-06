(coalton-library/utils:defstdlib-package #:coalton-library/experimental/do-control-core
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/functions)
  (:local-nicknames
   (:l #:coalton-library/list)
   (:opt #:coalton-library/optional)
   (:rst #:coalton-library/result))
  (:export
   #:when_
   #:whenM
   #:when-val
   #:when-valM
   #:if*
   #:if-val
   #:if-val_
   #:if-valM
   #:map-success
   #:map-successM
   #:flatmap-success
   #:flatmap-successM

   #:do-when
   #:do-whenM
   #:do-when-val
   #:do-when-valM
   #:do-if
   #:do-if-val
   #:do-if-not-val
   #:do-if-val_
   #:do-if-not-val_
   #:do-if-valM
   #:do-if-not-valM
   #:do-map-success
   #:do-map-successM
   #:do-flatmap-success
   #:do-flatmap-successM

   #:do-match
   #:matchM
   #:do-matchM
   #:do-when-match
   #:do-if-match))

(in-package #:coalton-library/experimental/do-control-core)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (define-class (Terminator :a)
    "Represents a value that terminates a control flow."
    (ended? (:a -> Boolean)))

  (define-instance (Terminator Boolean)
    (inline)
    (define ended? id))

  (define-instance (Terminator (Optional :a))
    (inline)
    (define ended? opt:none?))

  (define-instance (Terminator (Result :e :a))
    (inline)
    (define ended? rst:err?))

  (define-instance (Terminator (List :a))
    (inline)
    (define ended? l:null?))

  (define-class (Traversable :y => Yielder :y)
    "A data type that can terminate or yield a value into control flow."
    (yield (:y :a -> Optional :a))
    (concat-mapA (Applicative :m => :y :a -> (:a -> :m (:y :b)) -> :m (:y :b)))
    (wrap-success (:a -> :y :a)))

  (define-instance (Yielder Optional)
    (define yield id)
    (inline)
    (define (concat-mapA opt fa->mopt-b)
      (match opt
        ((None) (pure None))
        ((Some a) (fa->mopt-b a))))
    (define wrap-success Some))

  (define-instance (Yielder (Result :e))
    (inline)
    (define (yield res)
      (match res
        ((Ok a) (Some a))
        ((Err _) None)))
    (inline)
    (define (concat-mapA res fa->mres-b)
      (match res
        ((Err e) (pure (Err e)))
        ((Ok a) (fa->mres-b a))))
    (define wrap-success Ok))

  (define-instance (Yielder List)
    (define yield l:head)
    (inline)
    (define (concat-mapA lst fa->mlst-b)
      (map (fn (x) (>>= x id)) (traverse fa->mlst-b lst)))
    (inline)
    (define (wrap-success x)
      (make-list x))))

(coalton-toplevel
  ;;
  ;; Single Execution
  ;;

  (inline)
  (declare when_ ((Monad :m) (Terminator :t) => :t -> :m :z -> :m Unit))
  (define (when_ term? m)
    "Run the monadic operation M when the terminator TERM? indicates completion,
or do nothing."
    (if (ended? term?)
        (do m (pure Unit))
        (pure Unit)))

  (inline)
  (declare whenM ((Monad :m) (Terminator :t) => :m :t -> :m :z -> :m Unit))
  (define (whenM mterm? mop)
    "Evaluate MTERM?, and if it indicates completion, run MOP, or do nothing."
    (do
     (term? <- mterm?)
     (when_ term? mop)))

  (inline)
  (declare when-val ((Monad :m) (Yielder :y) => :y :a -> (:a -> :m :z) -> :m Unit))
  (define (when-val val? f->m)
    "If VAL? yields a value, apply F->M to it. If not, do nothing. Always returns Unit."
    (match (yield val?)
      ((None)
       (pure Unit))
      ((Some x)
       (do
        (f->m x)
        (pure Unit)))))

  (inline)
  (declare when-valM ((Monad :m) (Yielder :y) => :m (:y :a) -> (:a -> :m :z) -> :m Unit))
  (define (when-valM mval? f->m)
    "Evaluate MVAL?, and if it yields, run F->M on the value. Otherwise, do nothing."
    (do
     (val? <- mval?)
     (when-val val? f->m)))

  (inline)
  (declare if* ((Monad :m) (Terminator :t) => :t -> :m :b -> :m :b -> :m :b))
  (define (if* val? m-true m-false)
    "Choose between M-TRUE and M-FALSE based on VAL?. If (ended? VAL?) is true, run M-TRUE,
else run M-FALSE."
    (if (ended? val?)
        m-true
        m-false))

  (inline)
  (declare if-val ((Monad :m) (Yielder :y) => :y :a -> (:a -> :m :b) -> :m :b -> :m :b))
  (define (if-val val? f-mval m-none)
    "If VAL? yields a value, apply F-MVAL to it. Otherwise, run M-NONE."
    (match (yield val?)
      ((None)
       m-none)
      ((Some x)
       (f-mval x))))

  (inline)
  (declare if-val_ ((Monad :m) (Yielder :y) => :y :a -> (:a -> :m :b) -> :m :c -> :m Unit))
  (define (if-val_ val? f-mval m-none)
    "Like if-val, but discards the branch result and returns Unit."
    (match (yield val?)
      ((None)
       (do m-none (pure Unit)))
      ((Some x)
       (do (f-mval x) (pure Unit)))))

  (inline)
  (declare if-valM ((Monad :m) (Yielder :y) => :m (:y :a) -> (:a -> :m :b) -> :m :b -> :m :b))
  (define (if-valM mval? f-mval m-none)
    "Evaluate MVAL? and dispatch to F-MVAL if the result yields a value.
Otherwise evaluate M-NONE."
    (do
     (val? <- mval?)
     (if-val val? f-mval m-none)))

  (inline)
  (declare map-success ((Monad :m) (Yielder :y) => :y :a -> (:a -> :m :b) -> :m (:y :b)))
  (define (map-success val? f->mb)
    "Map F->MB over the successful/available value(s) of VAL? within the monad."
    (traverse f->mb val?))

  (inline)
  (declare map-successM ((Monad :m) (Yielder :y) => :m (:y :a) -> (:a -> :m :b) -> :m (:y :b)))
  (define (map-successM mval? f->mb)
    "Evaluate MVAL? and map F->MB over the successful value(s) from inside the monad."
    (do
     (val? <- mval?)
     (map-success val? f->mb)))

  (inline)
  (declare flatmap-success ((Monad :m) (Yielder :y) => :y :a -> (:a -> :m (:y :b)) -> :m (:y :b)))
  (define flatmap-success concat-mapA)

  (inline)
  (declare flatmap-successM ((Monad :m) (Yielder :y) => :m (:y :a) -> (:a -> :m (:y :b)) -> :m (:y :b)))
  (define (flatmap-successM mval? f->mval?b)
    "Evaluate MVAL?, and if the result yields a value, then flatmap F->MVAL?B
over the value."
    (do
     (val? <- mval?)
     (flatmap-success val? f->mval?b))))

(cl:defmacro do-when (b cl:&body body)
  "Run BODY (as a 'do' block) only when B indicates completion per Terminator semantics."
  `(when_ ,b
    (do
     ,@body)))

(cl:defmacro do-whenM (m-term cl:&body body)
  "Evaluate M-TERM and, when it indicates completion, run BODY as a 'do' block."
  `(whenM ,m-term
    (do
     ,@body)))

(cl:defmacro do-when-val ((sym opt) cl:&body body)
  "If OPT yields a value, bind it to SYM and run BODY as a 'do' block. Otherwise do nothing."
  `(when-val ,opt
    (fn (,sym)
      (do
       ,@body))))

(cl:defmacro do-when-valM ((sym opt) cl:&body body)
  "Evaluate OPT, bind the yielded value to SYM, and run BODY in a 'do' block."
  `(when-valM ,opt
    (fn (,sym)
      (do
       ,@body))))

(cl:defmacro do-if (term true-body cl:&body none-body)
  "If TERM indicates completion, run TRUE-BODY. Otherwise run NONE-BODY
in a 'do' block."
  `(if* ,term
    ,true-body
    (do
     ,@none-body)))

(cl:defmacro do-if-val ((sym opt) some-body cl:&body none-body)
  "If OPT yields a value, bind it to SYM and run SOME-BODY. Otherwise run
NONE-BODY in a 'do' block."
  `(if-val ,opt
    (fn (,sym)
      ,some-body)
    (do
     ,@none-body)))

(cl:defmacro do-if-not-val ((sym opt) none-body cl:&body some-body)
  "If OPT yields a value, bind it to SYM and run SOME-BODY in a 'do' block.
Otherwise, run NONE-BODY."
  `(if-val ,opt
     (fn (,sym)
       (do
        ,@some-body))
     ,none-body))

(cl:defmacro do-if-val_ ((sym opt) some-body cl:&body none-body)
  "If OPT yields a value, bind it to SYM and run SOME-BODY. Otherwise run NONE-BODY in a 'do' block.
Returns Unit."
  `(if-val_ ,opt
    (fn (,sym)
      ,some-body)
    (do
     ,@none-body)))

(cl:defmacro do-if-not-val_ ((sym opt) none-body cl:&body some-body)
  "If OPT yields a value, bind it to SYM and run SOME-BODY in a 'do' block.
Otherwise, run NONE-BODY. Returns Unit."
  `(if-val_ ,opt
    (fn (,sym)
      (do
       ,@some-body))
    ,none-body))

(cl:defmacro do-if-valM ((sym opt) some-body cl:&body none-body)
  "Evaluate OPT, then choose between SOME-BODY and NONE-BODY. Wraps NONE-BODY
in a 'do' block."
  `(if-valM ,opt
    (fn (,sym)
      ,some-body)
    (do
     ,@none-body)))

(cl:defmacro do-if-not-valM ((sym opt) none-body cl:&body some-body)
  "Evaluate OPT. If it yields a value, bind it to SYM and run SOME-BODY in a 'do' block.
Otherwise, run NONE-BODY."
  `(if-valM ,opt
    (fn (,sym)
      (do
       ,@some-body))
    ,none-body))

(cl:defmacro do-map-success ((sym val?) cl:&body body)
  "Apply BODY (in a 'do' block) to the successful/available value(s) of VAL?"
  `(map-success
     ,val?
     (fn (,sym)
       (do
        ,@body))))

(cl:defmacro do-map-successM ((sym val?) cl:&body body)
  "Evaluate VAL?. If the value(s) are successful, run BODY wrapped in a 'do' block
with the successful value(s) bound to SYM."
  `(map-successM
     ,val?
     (fn (,sym)
       (do
        ,@body))))

(cl:defmacro do-flatmap-success ((sym val?) cl:&body body)
  "Apply BODY (in a 'do' block) producing a Yielder and flatten the result."
  `(flatmap-success
    ,val?
    (fn (,sym)
      (do
       ,@body))))

(cl:defmacro do-flatmap-successM ((sym val?) cl:&body body)
  "Evaluate MVAL?, and if the result yields a value, then flatmap over BODY,
wrapped in a 'do' block, with the yielded value bound to SYM."
  `(flatmap-successM
    ,val?
    (fn (,sym)
      (do
       ,@body))))

;;
;; Loops
;;

(cl:defmacro do-collect-val (cl:&body body)
  "Run BODY repeatedly (in a 'do' block) collecting each yielded value into a list."
  `(collect-val
    (do
     ,@body)))

(cl:defmacro do-foreach ((sym lst) cl:&body body)
  "For each element of LST, bind it to SYM and run BODY in a 'do' block."
  `(foreach ,lst
    (fn (,sym)
      (do
       ,@body))))

(cl:defmacro do-when-match (scrut match-form cl:&body match-body)
  "If SCRUT matches MATCH-FORM, run MATCH-BODY in a 'do' block. Otherwise, do nothing."
  `(match ,scrut
     (,match-form
      (do
       ,@match-body
       (pure Unit)))
     (_ (pure Unit))))

(cl:defmacro do-if-match (scrut match-form match-body cl:&body rest-body)
  "If SCRUT matches MATCH-FORM, run MATCH-BODY. Otherwise, run REST-BODY."
  `(match ,scrut
     (,match-form ,match-body)
     (_ ,@rest-body)))

(cl:defmacro matchM (m cl:&body body)
  "Evaluate M and match on its result."
  (cl:let ((sym (cl:gensym "match-scrut")))
    `(do
      (,sym <- ,m)
      (match ,sym
        ,@body))))

(cl:defmacro do-match (scrutinee cl:&rest forms)
  "Converts:

(do-match x
  ((A a1 a2)
   (func1 a1)
   (func2 a2))
  ((B b1 b2)
   (func1 b1)
   (func2 b2)))

=>

(match x
  ((A a1 a2)
   (do
     (func1 a1)
     (func2 a2)))
  ((B b1 b2)
   (do
     (func1 b1)
     (func2 b2))))
"
  `(match ,scrutinee
     ,@(cl:mapcar
        (cl:lambda (form)
          (cl:destructuring-bind (pattern-form cl:&rest do-body) form
            `(,pattern-form
              (do
               ,@do-body))))
        forms)))

(cl:defmacro do-matchM (scrutinee-m cl:&body body)
  "Monadic variant of do-match. Evaluate SCRUTINEE-M and then perform do-match on the result."
  (cl:let ((sym (cl:gensym "match-scrut")))
    `(do
      (,sym <- ,scrutinee-m)
      (do-match ,sym
        ,@body))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/EXPERIMENTAL/DO-CONTROL-CORE")
