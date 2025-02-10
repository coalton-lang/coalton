(coalton-library/utils:defstdlib-package #:coalton-library/result
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/optional) 
  (:local-nicknames
   (#:cell #:coalton-library/cell)
   (#:iter #:coalton-library/iterator))
  (:export
   #:ok?
   #:err?
   #:map-err
   #:flatten
   #:ok-or-error))

(in-package #:coalton-library/result)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;;
  ;; Result
  ;;

  (inline)
  (declare ok? (Result :a :b -> Boolean))
  (define (ok? x)
    "Returns TRUE if X is OK"
    (match x
      ((Ok _) True)
      ((Err _) False)))

  (inline)
  (declare err? (Result :a :b -> Boolean))
  (define (err? x)
    "Returns TRUE if X is ERR"
    (match x
      ((Err _) True)
      ((Ok _) False)))

  (inline)
  (declare map-err ((:a -> :b) -> Result :a :c -> Result :b :c))
  (define (map-err f x)
    "Map over the ERR case"
    (match x
      ((Err x) (Err (f x)))
      ((Ok x) (Ok x))))

  (inline)
  (declare flatten (Result :a :a -> :a))
  (define (flatten x)
    (match x
      ((Ok x) x)
      ((Err x) x)))

  (inline)
  (declare ok-or-error ((Signalable :err) => (Result :err :a) -> :a))
  (define (ok-or-error res)
    (match res
      ((Ok elt) elt)
      ((Err r) (error r))))
  
  ;;
  ;; Instances
  ;;

  (define-instance ((Eq :a) (Eq :b) => Eq (Result :a :b))
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Ok a) (Ok b)) (== a b))
        ((Tuple (Err a) (Err b)) (== a b))
        (_ False))))

  (define-instance ((Ord :a) (Ord :b) => Ord (Result :a :b))
    (define (<=> a b)
      (match (Tuple a b)
        ((Tuple (Ok a) (Ok b)) (<=> a b))
        ((Tuple (Err a) (Err b)) (<=> a b))
        ((Tuple (Err _) (Ok _)) LT)
        ((Tuple (Ok _) (Err _)) GT))))

  (define-instance (Semigroup :b => Semigroup (Result :a :b))
    (define (<> a b)
      (match (Tuple a b)
        ((Tuple (Ok x) (Ok y))
         (Ok (<> x y)))
        ((Tuple (Err _) _) a)
        (_ b))))

  (define-instance (Monoid :b => Monoid (Result :a :b))
    (define mempty (Ok mempty)))

  (define-instance (Functor (Result :a))
    (inline)
    (define (map f x)
      (match x
        ((Ok x) (Ok (f x)))
        ((Err e) (Err e)))))

  (define-instance (Applicative (Result :a))
    (define (pure x) (Ok x))
    (define (liftA2 f a b)
      (match (Tuple a b)
        ((Tuple (Ok a) (Ok b))
         (Ok (f a b)))
        ((Tuple (Err e) _) (Err e))
        ((Tuple _ (Err e)) (Err e)))))

  (define-instance (Monad (Result :a))
    (inline)
    (define (>>= m f)
      (match m
        ((Ok x) (f x))
        ((Err e) (Err e)))))

  (define-instance (Bifunctor Result)
    (define (bimap f g res)
      (match res
        ((Ok x) (Ok (g x)))
        ((Err e) (Err (f e))))))

  (define-instance (Into (Result :a :b) (Optional :b))
    (inline)
    (define (into res)
      (match res
        ((Ok x) (Some x))
        ((Err _) None))))

  (define-instance (Into (Optional :b) (Result Unit :b))
    (inline)
    (define (into opt)
      (match opt
        ((Some x) (Ok x))
        ((None) (Err Unit)))))

  (define-instance (Iso (Result Unit :a) (Optional :a)))

  (define-instance (Unwrappable (Result :a))
    (inline)
    (define (unwrap-or-else succeed fail res)
      (match res
        ((Ok elt) (succeed elt))
        ((Err _) (fail)))))

  (define-instance (iter:IntoIterator (Result :err :elt) :elt)
    (define (iter:into-iter result)
      (match result
        ((Ok x)
         (let cell = (cell:new True))
         (iter:with-size
             (fn ()
               (unless (cell:read cell)
                 (return None))

               (cell:write! cell False)
               (Some x))
           1))
        ((Err _)
         iter:empty))))

  (define-instance (iter:FromIterator :container :elt => iter:FromIterator (Result :err :container) (Result :err :elt))
    (define (iter:collect! iter)
      (let error = (cell:new None))
      (let out =
        (iter:collect!
         (iter:map-while! (fn (x)
                            (match x
                              ((Ok x) (Some x))
                              ((Err e)
                               (cell:write! error (Some e))
                               None)))
                          iter)))
      (match (cell:read error)
        ((None) (Ok out))
        ((Some e) (Err e))))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/RESULT")
