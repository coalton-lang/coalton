(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-type-alias TestM (m-env:EnvT Integer
                                       (m-stt:StateT (List Integer)
                                                     m-id:Identity)))

  (define (runM env init-state m)
    (m-id:run-identity
     (m-stt:run-stateT
      (m-env:run-envT m env)
      init-state)))

  (declare just-get (TestM Integer))
  (define just-get
    (do
     (ints <- m-stt:get)
     (match ints
       ((Nil) (pure 0))
       ((Cons x _) (pure x)))))

  (declare just-put (TestM Unit))
  (define just-put
    (do
     (x <- m-env:ask)
     (m-stt:put (make-list x))))

  (declare just-modify (TestM Unit))
  (define just-modify
    (do
     (x <- m-env:ask)
     (m-stt:modify (map (+ x))))))

(define-test test-statet-get ()
  (let (Tuple state result) = (runM 100 (make-list 1 2 3) just-get))
  (is (== (make-list 1 2 3) state))
  (is (== 1 result)))

(define-test test-statet-put ()
  (let (Tuple state result) = (runM 100 (make-list 1 2 3) just-put))
  (is (== (make-list 100) state))
  (is (== Unit result)))

(define-test test-statet-modify ()
  (let (Tuple state result) = (runM 100 (make-list 1 2 3) just-modify))
  (is (== (make-list 101 102 103) state))
  (is (== Unit result)))
