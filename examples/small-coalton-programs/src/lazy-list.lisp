(cl:in-package #:small-coalton-programs)

(coalton-toplevel
  (define-type (LazyList :a)
    (LazyCons (Lazy :a) (LazyList :a))
    (LazyNil))

  (declare lazy-cons ((Lazy :a) -> (LazyList :a) -> (LazyList :a)))
  (define (lazy-cons x l)
    (LazyCons x l))

  (declare lazy-head ((LazyList :a) -> (Lazy :a)))
  (define (lazy-head l)
    (match l
      ((LazyCons x _) x)
      ((LazyNil) (error "Empty list"))))

  (declare lazy-tail ((LazyList :a) -> (LazyList :a)))
  (define (lazy-tail l)
    (match l
      ((LazyCons _ t) t)
      ((LazyNil) (error "Empty list"))))

  ;; The first item would fail when looked evaluated,
  ;; but not on constructing the list.
  ;; The second has a side-effect
  (define lst (LazyCons (lazy (error "1 fails"))
                        (LazyCons (lazy (lisp Integer ()
                                          (cl:progn (cl:format cl:*standard-output* "Evaluating second item")
                                                    2)))
                                  LazyNil)))
  ;; We can fetch the computation, but don't have to evaluate it
  (define a (lazy-head lst))
  ;; We can skip over and fetch the next item
  (define b (lazy-head (lazy-tail lst)))
  ;; Evaluating the second item works, and will also run the side-effects
  ;; Printing "Evaluating second item"
  (define bvalue (lazy-force b))
  ;; Evaluating a second time will just return the value, not run the computation again
  ;; Nothing is printed
  (define bvalue (lazy-force b))
  ;; Evaluating the first item will fail
  ;; (define avalue (lazy-force a))
  )
