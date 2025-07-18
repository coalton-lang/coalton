(coalton-library/utils::defstdlib-package #:coalton-library/monad/free
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/functions)
  (:export
   #:MonadFree
   #:wrap
   #:Free
   #:val
   #:liftF
   #:foldFree))

(in-package #:coalton-library/monad/free)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;
;; MonadFree Typeclass
;;

(coalton-toplevel
  (define-class (Monad :m => MonadFree :f :m)
    "A free monad is a monad, :m, which is capable of 'wrapping'
around functors, and then 'unwrapping' them later using `>>=`."
    (wrap (:f (:m :a) -> :m :a)))

  (declare liftF ((Functor :f) (MonadFree :f :m) => :f :a -> :m :a))
  (define (liftF f)
    "Lift a Functor into the Free Monad."
    (wrap (map pure f))))

(coalton-toplevel

  ;;
  ;; Free Type
  ;; 

  (define-type (Free :f :a)
    "`Free :f` gives you a Monad instance for any `Functor :f`.

References: [here](https://serokell.io/blog/introduction-to-free-monads) and [here](https://www.tweag.io/blog/2018-02-05-free-monads/)"    
    (Free (:f (Free :f :a)))
    (Val :a))

  (declare foldFree (Monad :c =>
                           (:a (Free :a :b) -> :c (Free :a :b)) ->
                           (Free :a :b) ->
                           (:c :b)))
  (define (foldFree nat fr)
    "Given a natural transformation, induce a Monad homomorphism from a
free monad to a target monad."
    (match fr
      ((Val a) (pure a))
      ((Free fa) (>>= (nat fa) (foldFree nat)))))

  ;;
  ;; Instances
  ;;

  (define-instance (Functor :f => Functor (Free :f))
    (define (map f freef)
      (match freef
        ((Val a) (Val (f a)))
        ((Free g) (Free (map (map f) g))))))

  (declare free-apply (Functor :f => Free :f (:a -> :b) -> Free :f :a -> Free :f :b))
  (define (free-apply free-f-func free-fa)
    "This is <*> implemented for Free :f"
    (match free-f-func
      ((Val func)
       (map func free-fa))
      ((Free funky-func)
       (Free
        (map
         (fn (func) (free-apply func free-fa))
         funky-func)))))

  (declare free-lifta2 (Functor :f => (:a -> :b -> :c) -> Free :f :a -> Free :f :b -> Free :f :c))
  (define (free-lifta2 op fa fb)
    (free-apply (map op fa) fb))

  (define-instance (Functor :f => Applicative (Free :f))
    (define pure Val)
    (define lifta2 free-lifta2))

  (define-instance (Functor :f => Monad (Free :f))
    (define (>>= fa a->fb)
      (match fa
        ((Val a) (a->fb a))
        ((Free ga)
         (Free (map
                ((flip >>=) a->fb)
                ga))))))

  (define-instance (Functor :f => MonadFree :f (Free :f))
    (define wrap Free))

  (define-instance (Traversable :t => Traversable (Free :t))
    ;; implementation taken from
    ;; https://hackage.haskell.org/package/free-5.2/docs/src/Control.Monad.Free.html
    (define (traverse f freea)
      (let ((g
              (fn (x)
                (match x
                  ((Val a) (map Val (f a)))
                  ((Free fa) (map Free (traverse g fa)))))))
        (g freea))))

  (define-instance (Foldable :f => Foldable (Free :f))
    (define (foldr f)
      (let ((g
              (fn (r fr)
                (match fr
                  ((Val a) (f a r))
                  ((Free fa) (foldr (flip g) r fa))))))
        g))

    (define (fold f)
      (let ((g
              (fn (r fr)
                (match fr
                  ((Val a) (f r a))
                  ((Free fa) (fold g r fa))))))
        g))))  

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MONAD/FREE")
