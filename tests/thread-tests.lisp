(in-package #:coalton-native-tests)

(coalton-toplevel 
  (define (sleep n)
    (lisp Unit (n) (cl:sleep n) Unit)))

;; Threads

(define-test thread-spawn-and-join ()
  (let ((thread (threads:spawn (sleep 1))))
    (is (threads:alive? thread))
    (threads:join thread)
    (is (not (threads:alive? thread)))))

(define-test thread-all-threads ()
  (is (some?
       (find (fn (thread) (== thread (threads:current-thread)))
             (threads:all-threads)))))

(define-test thread-all-threads-count ()
  (let ((count (length (threads:all-threads)))
        (thread (threads:spawn (sleep 60))))
    (is (== (length (threads:all-threads)) (1+ count)))
    (threads:destroy thread)
    (is (== (length (threads:all-threads)) count))))

;; Locks

(define-test lock-acquire-release ()
  (let ((lock (threads:make-lock)))
    (is (threads:acquire-lock lock))
    (threads:release-lock lock)
    (is (threads:acquire-lock-no-wait lock))
    (threads:release-lock lock)))

(define-test lock-fail-acquire ()
  (let ((lock (threads:make-lock)))
    (threads:spawn
      (threads:with-lock-held (lock)
        (sleep 60)))
    (sleep 1)
    (is (not (threads:acquire-lock-no-wait lock)))))

;; TODO Recursive Locks

;; Semaphores

(define-test semaphore-signal-await ()
  (let ((sem (threads:make-semaphore)))
    (threads:spawn
      (sleep 0.5)
      (threads:signal-semaphore sem 2))
    (threads:await-semaphore sem)
    (threads:await-semaphore sem)
    (is True)))

(define-test semaphore-n-of-m ()
  (let ((sem (threads:make-semaphore))
        (count (threads:make-atomic 0)))
    (threads:spawn
      (sleep 1)
      (threads:signal-semaphore sem 4))
    ;; idk dotimes
    (for x in (range 1 5)
      (threads:spawn 
        (threads:await-semaphore sem)
        (threads:incf-atomic count)))
    (sleep 1)
    (is (== 4 (threads:atomic-value count)))
    (threads:signal-semaphore sem 1)
    (sleep 1)
    (is (== 5 (threads:atomic-value count)))))

;; TODO Condition Variables

;; Atomics

(define-test atomic-incf-decf ()
  (let ((atomic (threads:make-atomic 3)))
    (threads:incf-atomic 10)
    (threads:decf-atomic 4)
    (is (== 9 (threads:atomic-value atomic)))))

(define-test atomic-cmp-and-swap ()
  (let ((atomic (threads:make-atomic 4)))
    (is (not (threads:atomic-cmp-and-swap atomic 0 100)))
    (is (threads:atomic-cmp-and-swap atomic 4 3))
    (is (== 3 (threads:atomic-value atomic)))))

(define-test atomic-concurrency ()
  (let ((atomic (threads:make-atomic 4000))
        (incer (threads:spawn
                 (for x in (range 1 1000)
                   (threads:incf-atomic atomic))))
        (decer (threads:spawn
                 (for x in (range 1 1000)
                   (threads:decf-atomic atomic)))))
    (threads:join incer)
    (threads:join decer)
    (is (== 4000 (threads:atomic-value atomic)))))
