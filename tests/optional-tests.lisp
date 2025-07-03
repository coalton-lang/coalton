(cl:in-package #:coalton-native-tests)

(define-test test-optional ()

  (let ((x 5)
        (n (the (Optional Integer) None))
        (s (fn (x) (Some x)))
        (u (fn (x) (unwrap x))))
    (is (== x
            (nest u u u
                  s s s
                  x)))
    (is (== x
            (nest u u u u u u u u u u u u u u u u u
                  s s s s s s s s s s s s s s s s s
                  x)))
    (is (== (nest s s s s x)
            (nest u u u u u u u u u u u u u
                  s s s s s s s s s s s s s s s s s
                  x)))
    (is (== n
            (nest u u u
                  s s s
                  n)))
    (is (== n
            (nest u u u u u u u u u u u u u u u u u
                  s s s s s s s s s s s s s s s s s
                  n)))
    (is (== (nest s s s s n)
            (nest u u u u u u u u u u u u u
                  s s s s s s s s s s s s s s s s s
                  n)))))
