(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare positive-or-fail (st:ST Integer (Result Integer Integer)))
  (define positive-or-fail
    (do
     (x <- st:get)
     (if (>= x 0)
         (pure (Ok x))
         (pure (Err x))))))

(define-test test-run-resultT ()
  (let (Tuple _ result) =
    (st:run
     (m-res:run-resultT
      (do
       (lift (st:modify (+ 2)))
       (m-res:ResultT positive-or-fail)
       (lift (st:modify (+ -10)))
       (m-res:ResultT positive-or-fail)
       (lift (st:modify (+ 20)))
       (m-res:ResultT positive-or-fail)))
     0))
  (is (== (Err -8) result)))

(coalton-toplevel
  (declare double-success-in-st
           (st:ST Integer (Result Integer Integer)
            -> st:ST Integer (Result Integer Integer)))
  (define (double-success-in-st comp)
    (do
     (res <- comp)
     (pure (match res
             ((Err e) (Err e))
             ((Ok  v) (Ok (* 2 v))))))))

(define-test test-map-resultT ()
  (let (Tuple _ ok-res) =
    (st:run
     (m-res:run-resultT
      (m-res:map-resultT double-success-in-st
                         (m-res:ResultT positive-or-fail)))
     4))
  (is (== (Ok 8) ok-res))
  (let (Tuple _ err-res) =
    (st:run
     (m-res:run-resultT
      (m-res:map-resultT double-success-in-st
                         (m-res:ResultT positive-or-fail)))
     -2))
  (is (== (Err -2) err-res)))

(define-test test-map-errT ()
  (let (Tuple _ err-res) =
    (st:run
     (m-res:run-resultT
      (m-res:map-errT abs
                      (m-res:ResultT positive-or-fail)))
     -6))
  (is (== (Err 6) err-res))
  (let (Tuple _ ok-res) =
    (st:run
     (m-res:run-resultT
      (m-res:map-errT (* -1)
                      (m-res:ResultT positive-or-fail)))
     5))
  (is (== (Ok 5) ok-res)))

(define-test test-err-ifT ()
  (let (Tuple _ pass-res) =
    (st:run
     (m-res:run-resultT (m-res:err-ifT false "fail"))
     0))
  (is (== (Ok Unit) pass-res))
  (let (Tuple _ fail-res) =
    (st:run
     (m-res:run-resultT (m-res:err-ifT true "fail"))
     0))
  (is (== (Err "fail") fail-res)))
