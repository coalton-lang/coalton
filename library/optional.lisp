(coalton-library/utils:defstdlib-package #:coalton-library/optional
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes)
  (:local-nicknames
   (#:cell #:coalton-library/cell)
   (#:iter #:coalton-library/iterator))
  (:export
   #:from-some
   #:some?
   #:none?))

(in-package #:coalton-library/optional)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;;
  ;; Optional
  ;;

  (declare from-some (String -> (Optional :a) -> :a))
  (define (from-some str opt)
    "Get the value of OPT, erroring with the provided string if it is None."
    (match opt
      ((Some x) x)
      ((None) (error str))))

  (declare some? ((Optional :a) -> Boolean))
  (define (some? x)
    "Is X Some?"
    (match x
      ((Some _) True)
      ((None) False)))

  (declare none? ((Optional :a) -> Boolean))
  (define (none? x)
    "Is X None?"
    (match x
      ((None) True)
      ((Some _) False)))

  ;;
  ;; Instances
  ;;

  (define-instance (Eq :a => Eq (Optional :a))
    (define (== x y)
      (match (Tuple x y)
        ((Tuple (Some x) (Some y)) (== x y))
        ((Tuple (None) (None)) True)
        (_ False))))

  (define-instance (Ord :a => Ord (Optional :a))
    (define (<=> x y)
      (match (Tuple x y)
        ((Tuple (Some x) (Some y))
         (<=> x y))
        ((Tuple (Some _) (None))
         GT)
        ((Tuple (None) (Some _))
         LT)
        ((Tuple (None) (None))
         Eq))))

  (define-instance (Num :a => Num (Optional :a))
    (define (+ a b) (liftA2 + a b))
    (define (- a b) (liftA2 - a b))
    (define (* a b) (liftA2 * a b))
    (define (/ a b) (liftA2 / a b))
    (define (fromInt x) (pure (fromInt x))))

  (define-instance (Semigroup :a => Semigroup (Optional :a))
    (define (<> a b)
      (match (Tuple a b)
        ((Tuple (Some a) (Some b)) (Some (<> a b)))
        (_ None))))

  (define-instance (Monoid :a => Monoid (Optional :a))
    (define mempty (Some mempty)))

  (define-instance (Functor Optional)
    (define (map f x)
      (match x
        ((Some a) (Some (f a)))
        ((None) None))))

  (define-instance (Applicative Optional)
    (define (pure x)
      (Some x))
    (define (liftA2 f x y)
      (match (Tuple x y)
        ((Tuple (Some x) (Some y))
         (Some (f x y)))
        (_ None))))

  (define-instance (Monad Optional)
    (define (>>= x f)
      (match x
        ((Some x) (f x))
        ((None) None))))

  (define-instance (MonadFail Optional)
    (define (fail _)
      None))

  (define-instance (Alternative Optional)
    (define empty None)
    (define (alt x y)
      (match x
        ((Some _) x)
        (_ y))))

  (define-instance (Unwrappable Optional)
    (define (unwrap-or-else succeed fail opt)
      (match opt
        ((Some elt) (succeed elt))
        ((None) (fail)))))

  (define-instance (iter:IntoIterator (Optional :a) :a)
    (define (iter:into-iter opt)
      (match opt
        ((Some x)
         (let cell = (cell:new True))
         (iter:with-size
             (fn ()
               (unless (cell:read cell)
                 (return None))

               (cell:write! cell False)
               opt)
           1))
        ((None)
         iter:empty))))

  (define-instance (iter:FromIterator :container :elt => iter:FromIterator (Optional :container) (Optional :elt))
    (define (iter:collect! iter)
      (let error = (cell:new False))
      (let out =
        (iter:collect!
         (iter:map-while! (fn (x)
                             (match x
                               ((Some _) x)
                               ((None)
                                (cell:write! error True)
                                None)))
                           iter)))
      (if (cell:read error)
          None
          (Some out)))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/OPTIONAL")
