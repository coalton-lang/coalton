(in-package #:coalton-library)

(cl:defmacro lazy (expr)
  `(%Lazy (make-cell (Thunk (fn (_) ,expr)))))

(coalton-toplevel
  (define-type (LazyState :a)
    "Internal state for the lazy computation. THUNK contains the computation to
run, VALUE is the result of the computation."
    (Thunk (Unit -> :a))
    (Value :a))

  (define-type (Lazy :a)
    "Lazily evaluated computation. Construct by using the LAZY macro, compute the
value by calling LAZY-FORCE."
    (%Lazy (Cell (LazyState :a))))

  (define-instance (Functor Lazy)
    (define (map f l)
      (lazy (f (lazy-force l)))))

  (define-instance (Applicative Lazy)
    (define (pure v)
      (%Lazy (make-cell (Value v))))
    (define (liftA2 f fa fb)
      (lazy (f (into fa) (into fb)))))

  (define-instance (Monad Lazy)
    (define (>>= ma f)
      (lazy (into (f (into ma))))))

  (define-instance (Into :a (Lazy :a))
    (define into pure))

  (define-instance (Into (Lazy :a) :a)
    (define into lazy-force))

  (declare lazy-force ((Lazy :a) -> :a))
  (define (lazy-force l)
    "Return result of lazy computation, computing it if necessary.

The computation is only evaluated once, and subsequent calls to LAZY-FORCE will
just return the result.

The function is not thread-safe, so trying to access from several threads might
run the computation more than once, and if the computation isn't idempotent,
different values might be produced."
    (match l
      ((%Lazy c)
       (match (into c)
         ((Value v) v)
         ((Thunk f)
          (progn
            (let v = (f))
            (cell-write (Value v) c)
            v)))))))
