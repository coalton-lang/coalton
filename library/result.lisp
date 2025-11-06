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
   #:okM
   #:map-err
   #:ok-or-def
   #:flatten
   #:opt->result
   #:err-if
   #:ok-or-error))

(in-package #:coalton-library/result)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;;
  ;; Result
  ;;

  (declare err-if (Boolean -> :err -> Result :err Unit))
  (define (err-if failed? failure)
    "Fail with FAILURE value if FAILED? is True."
    (if failed?
        (Err failure)
        (Ok Unit)))

  (declare opt->result (:err -> Optional :a -> Result :err :a))
  (define (opt->result failure opt)
    "Convert OPT to a Result, using FAILURE value if None."
    (match opt
      ((None) (Err failure))
      ((Some a) (Ok a))))

  (declare ok? (Result :a :b -> Boolean))
  (define (ok? x)
    "Returns TRUE if X is OK"
    (match x
      ((Ok _) True)
      ((Err _) False)))

  (declare err? (Result :a :b -> Boolean))
  (define (err? x)
    "Returns TRUE if X is ERR"
    (match x
      ((Err _) True)
      ((Ok _) False)))

  (inline)
  (declare okM (Functor :f => :f :a -> :f (Result :e :a)))
  (define (okM f-a)
    "Wrap a value inside F-A inside of 'Ok'."
    (map Ok f-a))

  (declare map-err ((:a -> :b) -> Result :a :c -> Result :b :c))
  (define (map-err f x)
    "Map over the ERR case"
    (match x
      ((Err x) (Err (f x)))
      ((Ok x) (Ok x))))

  (declare ok-or-def (:a -> Result :err :a -> :a))
  (define (ok-or-def def res)
    "Take value in RES if it is OK, or DEF if it is ERR."
    (match res
      ((Err _) def)
      ((Ok x) x)))

  (declare flatten (Result :a :a -> :a))
  (define (flatten x)
    (match x
      ((Ok x) x)
      ((Err x) x)))

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
    (define (>>= m f)
      (match m
        ((Ok x) (f x))
        ((Err e) (Err e)))))

  (define-instance (Traversable (Result :a))
    (define (traverse f m)
      (match m
        ((Err x) (pure (Err x)))
        ((Ok y)  (map Ok (f y))))))

  (define-instance (Bifunctor Result)
    (define (bimap f g res)
      (match res
        ((Ok x) (Ok (g x)))
        ((Err e) (Err (f e))))))

  (define-instance (Into (Result :a :b) (Optional :b))
    (define (into res)
      (match res
        ((Ok x) (Some x))
        ((Err _) None))))

  (define-instance (Into (Optional :b) (Result Unit :b))
    (define (into opt)
      (match opt
        ((Some x) (Ok x))
        ((None) (Err Unit)))))

  (define-instance (Iso (Result Unit :a) (Optional :a)))

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
