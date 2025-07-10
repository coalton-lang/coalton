(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare assert-positive (m-opt:OptionalT (st:ST Integer) Integer))
  (define assert-positive
    (do
     (val <- (lift st:get))
     (if (> val 0)
         (pure val)
         empty))))

(define-test test-run-optionalT ()
  (let (Tuple _ result) =
    (st:run (m-opt:run-optionalT assert-positive)
            0))
  (is (none? result))
  (let (Tuple _ result2) =
    (st:run (m-opt:run-optionalT
             (do
              (lift (st:modify (+ 2)))
              assert-positive
              (lift (st:modify (+ 3)))
              (lift st:get)))
            0))
  (is (== (Some 5) result2)))

(coalton-toplevel
  (declare split-positive-state (st:ST (Optional Integer) :a
                                 -> Result (Optional Integer) (Optional Integer)))
  (define (split-positive-state stateful-calculation)
    (let (Tuple state _) = (st:run stateful-calculation (Some 0)))
    (match state
      ((None) (Err None))
      ((Some x)
       (if (> x 0)
          (Ok state)
          (Err state))))))

(define-test test-map-optionalT ()
  (let result1 =
    (m-opt:run-optionalT
     (m-opt:map-optionalT
      split-positive-state
      (lift (st:modify (map (+ 3)))))))
  (is (== (Ok (Some 3)) result1))
  (let result2 =
    (m-opt:run-optionalT
     (m-opt:map-optionalT
      split-positive-state
      (lift (st:modify (map (+ -2)))))))
  (is (== (Err (Some -2)) result2))
  (let result3 =
    (m-opt:run-optionalT
     (m-opt:map-optionalT
      split-positive-state
      (lift (do
       (st:modify (map (+ -2)))
       (st:modify (fn (opt) (>>= opt (fn (x) (if (> x 0) (Some x) None))))))))))
  (is (== (Err None) result3)))

