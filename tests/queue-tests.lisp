(in-package #:coalton-native-tests)

(define-test queue-show ()
  (let q = (queue:new))
  (queue:push! 1 q)
  (queue:push! 2 q)
  (queue:push! 3 q)
  (is (== "#<Queue [1 2 3]>"
          (show-as-string q))))
