(cl:in-package #:coalton-native-tests)

(coalton-toplevel
  (define (recursive-predicate x inc goal)
    (if (== x goal)
        goal
        (recursive-predicate (+ x inc) inc goal))))

;; Test that functions with typeclass constraints call themselfs recursively
;; This was bugged in the past #216 #147 #125
(define-test test-recursive-predicate ()
  (is (== (recursive-predicate 0 1 5) 5)))

(coalton-toplevel
  (define (gh-295-f a)
    (let ((g (fn (x)
               (== x (make-list a)))))
      g)))

;; See gh #295
(define-test test-deferred-predicate-removal ()
  (progn
    (is (== (gh-295-f 1 (make-list 2 3 4)) False))
    (is (== (gh-295-f 1 (make-list 1)) True))))


;; Test that functions can be given explicit predicates in any order
;; See gh #377
(coalton-toplevel
  (declare gh-377-a ((Num :a) (Ord :a) => :a -> :a -> :a -> (List :a)))
  (define (gh-377-a step start end)
    (let ((inner
            (fn (current acc)
              (if (> current end)
                  acc
                  (inner (+ current step)
                         (Cons current acc))))))
      (inner start Nil)))

 (declare gh-377-b ((Ord :a) (Num :a) => :a -> :a -> :a -> (List :a)))
  (define (gh-377-b step start end)
    (let ((inner
            (fn (current acc)
              (if (> current end)
                  acc
                  (inner (+ current step)
                         (Cons current acc))))))
      (inner start Nil))))

(define-test test-explicit-predicate-order ()
  (progn
    (is (== (gh-377-a 1 0 5)
            (make-list 5 4 3 2 1 0)))
    (is (== (gh-377-b 1 0 5)
            (make-list 5 4 3 2 1 0)))))
