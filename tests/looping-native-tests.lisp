(in-package #:coalton-native-tests)

(define-test test-while-loop ()
  (let ((countdown (cell:new 10))
        (sum (cell:new 0)))
    (while (< 0 (cell:decrement! countdown))
      (cell:update! (+ (cell:read countdown)) sum))
    (is (== 0 (cell:read countdown)))
    (is (== 45 (cell:read sum)))))


(define-test test-while-loop-break ()
  (let countdown = (cell:new 10))
  (let sum = (cell:new 0))

  (while (< 0 (cell:decrement! countdown))
    (cell:update! (+ (cell:read countdown)) sum)
    (when (== (cell:read countdown) 5) (break)))

  (is (== 5 (cell:read countdown)))
  (is (== 35 (cell:read sum)))

  ;; again but now we break with labels
  (cell:swap! countdown 10)
  (cell:swap! sum 0)

  (while :aloop (< 0 (cell:decrement! countdown))
    (cell:update! (+ (cell:read countdown)) sum)
    (when (== (cell:read countdown) 5) (break :aloop)))

  (is (== 5 (cell:read countdown)))
  (is (== 35 (cell:read sum)))

  ;; just evens
  (cell:swap! countdown 10)
  (cell:swap! sum 0)

  (while :aloop (< 0 (cell:decrement! countdown))
    (when (odd? (cell:read countdown)) (continue :aloop))
    (cell:update! (+ (cell:read countdown)) sum))

  (is (== 20 (cell:read sum))))


(define-test test-while-let ()
  (let ((iter (iter:up-to 10))
        (sum (cell:new 0)))
    (while-let
     (Some wtf) = (iter:next! iter)
     (cell:update! (+ wtf) sum))
    (is (== 45 (cell:read sum)))))


(define-test test-for ()
  (let ((sum (cell:new 0)))

    (for x in (iter:up-to 10) (cell:update! (+ x) sum))
    (is (== 45 (cell:read sum)))

    (cell:swap! sum 0)
    (for x in (iter:up-to 20)
         (cell:update! (+ x) sum)
         (when (== 9 x) (break)))
    (is (== 45 (cell:read sum )))

    (cell:swap! sum 0)
    (for x in (iter:up-to 20)
         (when (even? x) (continue))
         (cell:update! (+ x) sum))
    (is (== 100 (cell:read sum)))


    (cell:swap! sum 0)
    (for :aloop x in (iter:up-to 20)
         (cell:update! (+ x) sum)
         (when (== 9 x) (break :aloop)))
    (is (== 45 (cell:read sum )))


    (cell:swap! sum 0)
    (for :aloop x in (iter:up-to 20)
         (when (even? x) (continue :aloop))
         (cell:update! (+ x) sum))
    (is (== 100 (cell:read sum)))))

(define-test test-loop-control ()
  ;; These first few just test that escape works at all. Tests will
  ;; hang if it doesn't.
  (loop (break))
  (is True)

  (loop :outer (break))
  (is True)

  (loop :outer (break :outer))
  (is True)

  (loop :outer (while True (break :outer)))
  (is True)

  ;; test breaking from an outer loop in an inner loop.
  (let counter = (cell:new 0))
  (let acc = (cell:new Nil))
  (loop :outer
        (while (< (cell:increment! counter) 10)
          (let x = (fold + (cell:read counter) (cell:read acc)))
          (when (< 500 x)
            (break :outer))
          (when (/= 0 (mod (cell:read counter) 3)) 
            (cell:push! acc x)
            Unit))
        
        (when (< (length (cell:read acc)) 500)
          (cell:swap! counter 0)
          Unit))
  (is (== 279 (list:car (cell:read acc))))


  ;; the same thing but using continue in the inner loop  
  (cell:swap! counter 0)
  (cell:swap! acc Nil)
  (loop :outer
        (while (< (cell:increment! counter) 10)
          (let x = (fold + (cell:read counter) (cell:read acc)))
          (when (< 500 x)
            (break :outer))
          (when (== 0 (mod (cell:read counter) 3)) 
            (continue))
          (cell:push! acc x))

        (when (< (length (cell:read acc)) 500)
          (cell:swap! counter 0)
          Unit))
  (is (== 279 (list:car (cell:read acc))))

  ;; the same thing again, but just using loop
  (cell:swap! counter 0)
  (cell:swap! acc Nil)
  (loop :outer
        (loop
          (unless (< (cell:increment! counter) 10)
            (break))                ; break inner
          (let x = (fold + (cell:read counter) (cell:read acc)))
          (when (< 500 x)
            (break :outer))         ; break outer
          (when (== 0 (mod (cell:read counter) 3))
            (continue))             ; continue inner
          (cell:push! acc x)
          (unless (< (length (cell:read acc)) 500)
            (continue :outer)))     ; we can continue from outer in inner
        (cell:swap! counter 0))
  (is (== 279 (list:car (cell:read acc)))))




