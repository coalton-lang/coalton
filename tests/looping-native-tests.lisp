(in-package #:coalton-native-tests)

(define-test test-for-while-clause ()
  (let sum = (cell:new (the UFix 0)))
  (let countdown =
    (for ((declare countdown UFix)
          (countdown 9 (1- countdown)))
      :returns countdown
      :while (< 0 countdown)
      (cell:update! (fn (acc) (+ countdown acc)) sum)))
  (is (== 0 countdown))
  (is (== 45 (cell:read sum))))

(define-test test-for-init-bindings-follow-let ()
  (let result =
    (for ((declare a UFix)
          (declare b UFix)
          (a b a)
          (b 1 (+ b 1)))
      :returns (Tuple a b)
      :while (< b 10)
      Unit))
  (match result
    ((Tuple a b)
     (is (== 1 a))
     (is (== 10 b)))))


(define-test test-for-while-clause-control ()
  (let sum = (cell:new (the UFix 0)))
  (let countdown =
    (for ((declare countdown UFix)
          (countdown 9 (1- countdown)))
      :returns countdown
      :while (< 0 countdown)
      (cell:update! (fn (acc) (+ countdown acc)) sum)
      (when (== countdown 5) (break))))

  (is (== 5 countdown))
  (is (== 35 (cell:read sum)))

  ;; again but now we break with labels
  (cell:swap! sum (the UFix 0))
  (let countdown =
    (for :aloop ((declare countdown UFix)
                 (countdown 9 (1- countdown)))
      :returns countdown
      :while (< 0 countdown)
      (cell:update! (fn (acc) (+ countdown acc)) sum)
      (when (== countdown 5) (break :aloop))))

  (is (== 5 countdown))
  (is (== 35 (cell:read sum)))

  ;; just evens
  (cell:swap! sum (the UFix 0))
  (for :aloop ((declare countdown UFix)
               (countdown 9 (1- countdown)))
    :while (< 0 countdown)
    (when (odd? countdown) (continue :aloop))
    (cell:update! (fn (acc) (+ countdown acc)) sum))

  (is (== 20 (cell:read sum))))


(define-test test-for-match-break ()
  (let xs = (vector:make 0 1 2 3 4 5 6 7 8 9))
  (let count =
    (for ((declare count UFix)
          (count 0 (1+ count)))
      :returns count
      (when (== (vector:pop! xs) None)
        (break))))
  (is (== 10 count)))


(define-test test-counted-for ()
  (let sum = (cell:new (the UFix 0)))

  (for ((declare i UFix)
        (i 0 (1+ i)))
    :repeat 10
    (cell:update! (fn (acc) (+ i acc)) sum))
  (is (== 45 (cell:read sum)))

  (cell:swap! sum (the UFix 0))
  (for ((declare i UFix)
        (i 0 (1+ i)))
    :repeat 20
    (cell:update! (fn (acc) (+ i acc)) sum)
    (when (== 9 i) (break)))
  (is (== 45 (cell:read sum )))

  (cell:swap! sum (the UFix 0))
  (for ((declare i UFix)
        (i 0 (1+ i)))
    :repeat 20
    (when (even? i) (continue))
    (cell:update! (fn (acc) (+ i acc)) sum))
  (is (== 100 (cell:read sum)))

  (cell:swap! sum (the UFix 0))
  (for :aloop ((declare i UFix)
               (i 0 (1+ i)))
    :repeat 20
    (cell:update! (fn (acc) (+ i acc)) sum)
    (when (== 9 i) (break :aloop)))
  (is (== 45 (cell:read sum)))

  (cell:swap! sum (the UFix 0))
  (for :aloop ((declare i UFix)
               (i 0 (1+ i)))
    :repeat 20
    (when (even? i) (continue :aloop))
    (cell:update! (fn (acc) (+ i acc)) sum))
  (is (== 100 (cell:read sum))))

(define-test test-for-fibonacci-bindings ()
  (let fibs = (vector:new))
  (for ((declare a UFix)
        (declare b UFix)
        (a b b)
        (b 1 (+ a b)))
    :repeat 20
    (vector:push! a fibs))
  (is (== (vector:make 1 1 2 3 5 8 13 21 34 55
                        89 144 233 377 610 987 1597 2584 4181 6765)
          fibs)))

(define-test test-for*-sequential-bindings ()
  (let result =
    (for* ((declare a UFix)
           (declare b UFix)
           (a 1 b)
           (b 1 (+ a b)))
      :returns (Tuple a b)
      :repeat 5
      Unit))
  (match result
    ((Tuple a b)
     (is (== 16 a))
     (is (== 32 b)))))

(define-test test-for*-init-bindings-are-non-recursive ()
  (let x = (the UFix 1))
  (let result =
    (for* ((declare x UFix)
           (x (1+ x) (1+ x)))
      :returns x
      :repeat 3
      Unit))
  (is (== 1 x))
  (is (== 5 result)))

(define-test test-for-control ()
  ;; These first few just test that escape works at all. Tests will
  ;; hang if it doesn't.
  (for () (break))
  (is True)

  (for :outer ()
    (break))
  (is True)

  (for :outer ()
    (break :outer))
  (is True)

  (for :outer ()
    (for ()
      :while True
      (break :outer)))
  (is True)

  ;; test breaking from an outer loop in an inner loop.
  (let acc = (cell:new Nil))
  (for :outer ()
    (for ((declare counter UFix)
          (counter 1 (1+ counter)))
      :repeat 9
      (let x = (fold + counter (cell:read acc)))
      (when (< 500 x)
        (break :outer))
      (when (/= 0 (mod counter 3))
        (cell:push! acc x))))
  (is (== 279 (list:car (cell:read acc))))


  ;; the same thing but using continue in the inner for
  (cell:swap! acc Nil)
  (for :outer ()
    (for ((declare counter UFix)
          (counter 1 (1+ counter)))
      :repeat 9
      (let x = (fold + counter (cell:read acc)))
      (when (< 500 x)
        (break :outer))
      (when (== 0 (mod counter 3))
        (continue))
      (cell:push! acc x)))
  (is (== 279 (list:car (cell:read acc))))

  ;; the same thing again, but just using for
  (cell:swap! acc Nil)
  (for :outer ()
    (for ((declare counter UFix)
          (counter 1 (1+ counter)))
      (unless (< counter 10)
        (break))                ; break inner
      (let x = (fold + counter (cell:read acc)))
      (when (< 500 x)
        (break :outer))         ; break outer
      (when (== 0 (mod counter 3))
        (continue))             ; continue inner
      (cell:push! acc x)
      (unless (< (length (cell:read acc)) 500)
        (continue :outer)))     ; we can continue from outer in inner
    )
  (is (== 279 (list:car (cell:read acc)))))

(define-test test-imperative-for-bindings ()
  (let hits = (cell:new 0))
  (for ((x 10 x))
    :repeat x
    (cell:increment! hits))
  (is (== 10 (cell:read hits))))
