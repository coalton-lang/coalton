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

;; TODO Semaphores

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
    (for x in (make-list 1 2 3 4 5)
      (threads:spawn 
        (threads:await-semaphore sem)
        (threads:incf-atomic count)))
    (sleep 1)
    (is (== 4 (threads:atomic-value count)))
    (threads:signal-semaphore sem 1)
    (sleep 1)
    (is (== 5 (threads:atomic-value count)))))

;; TODO Condition Variables

;; TODO Atomics


