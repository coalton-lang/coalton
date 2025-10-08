(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

(define-test test-run-envT ()
  (let (Tuple _ result) =
    (st:run
     (m-env:run-envT
      (do
       (m-env:EnvT (fn (_) (st:modify (+ 2))))
       (m-env:EnvT (fn (_) st:get)))
      0)
     0))
  (is (== 2 result)))

(coalton-toplevel
  (declare add-context (m-env:EnvT Integer (st:ST Integer) Unit))
  (define add-context
    (do
     (x <- m-env:ask)
     (lift (st:modify (+ x))))))

(define-test test-ask-envT ()
  (let (Tuple _ result) =
    (st:run
     (m-env:run-envT
      (do
       add-context
       add-context
       (m-env:lift-envT st:get))
      2)
     0))
  (is (== 4 result)))

(coalton-toplevel
  (define-struct Config
    (x Integer)))

(define-test test-asks-envT ()
  (let (Tuple _ result) =
    (st:run
     (m-env:run-envT
      (do
       (x <- (the (m-env:EnvT :env :m Integer) (m-env:asks-envT .x)))
       (m-env:lift-envT (st:modify (+ x)))
       (m-env:lift-envT st:get))
      (Config 10))
     0))
  (is (== 10 result)))

(define-test test-local-envT ()
  (let (Tuple _ result) =
    (st:run
     (m-env:run-envT
      (do
       add-context
       (m-env:local-envT (* 3) add-context)
       (m-env:lift-envT st:get))
      2)
     0))
  (is (== 8 result)))

(define-test test-ask-env ()
  (let result =
    (m-env:run-env
     (do
      (x <- m-env:ask)
      (pure (+ 10 x)))
     15))
  (is (== 25 result)))

(define-test test-asks-env ()
  (let result =
    (m-env:run-env
     (do
      (x? <- (m-env:asks head))
      (pure
       (match x?
         ((None) 0)
         ((Some x) (+ 10 x)))))
     (make-list 10 20 30 40)))
  (is (== 20 result)))

(define-test test-local-env ()
  (let result =
    (m-env:run-env
     (m-env:local
      (+ 100)
      (do
       (x <- m-env:ask)
       (pure (+ x 10))))
     15))
  (is (== 125 result)))
