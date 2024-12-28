(in-package #:coalton-native-tests)

;;---------;;
;; Threads ;;
;;---------;;

(define-test thread-spawn-and-join ()
  (let ((thread (threads:spawn (sys:sleep (the Integer 1)) (make-list 1))))
    (is (threads:alive? thread))
    (is (== (make-list 1) (unwrap (threads:join thread))))
    (is (not (threads:alive? thread)))))

(define-test thread-lisp-typed-join ()
  (let ((thread
          (lisp (threads:Thread String) ()
            (bt2:make-thread (cl:lambda () "string"))))
        (thread-inferred
          (lisp (threads:Thread :a) ()
            (bt2:make-thread (cl:lambda () 1.01)))))
    (is (== "string" (unwrap (threads:join thread))))
    (is (== 1.01 (unwrap (threads:join thread-inferred))))))

(define-test thread-destroy-and-join ()
  (let ((thread (threads:spawn (sys:sleep (the Integer 10)) 10)))
    (threads:destroy thread)
    (is (result:err? (threads:join thread)))))

(define-test thread-all-threads ()
  (is (some?
       (find (== (threads:current-thread))
             (threads:all-threads)))))

(define-test thread-all-threads-contains-new ()
  (let ((old-threads (threads:all-threads))
        (thread (threads:spawn (sys:sleep (the Integer 40)))))
    (sys:sleep (the Integer 1))
    (is (some? (find (== thread) (threads:all-threads))))
    (is (none? (find (== thread) old-threads)))
    (threads:destroy thread)))

;;-------;;
;; Locks ;;
;;-------;;

(define-test lock-acquire-release ()
  (let ((lock (threads:make-lock)))
    (is (threads:acquire-lock lock))
    (threads:release-lock lock)
    (is (threads:acquire-lock-no-wait lock))
    (threads:release-lock lock)))

(define-test lock-fail-acquire ()
  (let ((lock (threads:make-lock))
        (thread 
          (threads:spawn
            (threads:with-lock-held lock (fn () (sys:sleep (the Integer 60)))))))
    (sys:sleep (the Integer 1))
    (is (not (threads:acquire-lock-no-wait lock)))
    (threads:destroy thread)))

;;------------;;
;; Semaphores ;;
;;------------;;

(define-test semaphore-signal-await ()
  (let ((sem (threads:make-semaphore)))
    (threads:spawn
      (sys:sleep (the Single-Float 0.5))
      (threads:signal-semaphore sem 2))
    (threads:await-semaphore sem)
    (threads:await-semaphore sem)
    (is True)))

(define-test semaphore-n-of-m ()
  (let ((sem (threads:make-semaphore))
        (count (threads:make-atomic 0)))
    (threads:spawn
      (sys:sleep (the Integer 1))
      (threads:signal-semaphore sem 4))
    (for x in (range 1 5)
      (threads:spawn 
        (threads:await-semaphore sem)
        (threads:incf-atomic count 1)))
    (sys:sleep (the Integer 1))
    (is (== 4 (threads:atomic-value count)))
    (threads:signal-semaphore sem 1)
    (sys:sleep (the Integer 1))
    (is (== 5 (threads:atomic-value count)))))

;;---------------------;;
;; Condition Variables ;;
;;---------------------;;

(define-test condition-variable-concurrency ()
  (let ((atomic (threads:make-atomic 0))
        (target 30)
        (cv (threads:make-cv))
        (lock (threads:make-lock)))
    (let ((worker (fn (i)
                    (threads:with-lock-held lock
                      (fn () 
                        (while (not (== i (threads:atomic-value atomic)))
                          (threads:await-cv cv lock)
                          (sys:sleep (the Single-Float 0.1)))
                        (threads:incf-atomic atomic 1)))
                    (threads:broadcast-cv cv)))) 
      (for x in (range target 1)
        (threads:spawn
          (sys:sleep (the Single-Float 0.1))
          (worker (- target x)))) 
      (threads:with-lock-held lock 
        (fn () 
          (while (not (== target (threads:atomic-value atomic)))
            (threads:await-cv cv lock))))
      (is (== target (threads:atomic-value atomic))))))

;;---------;;
;; Atomics ;;
;;---------;;

(define-test atomic-incf-decf ()
  (let ((atomic (threads:make-atomic 3)))
    (is (== 13 (threads:incf-atomic atomic 10)))
    (is (== 9 (threads:decf-atomic atomic 4)))))

(define-test atomic-cmp-and-swap ()
  (let ((atomic (threads:make-atomic 4)))
    (is (not (threads:atomic-cmp-and-swap atomic 0 100)))
    (is (threads:atomic-cmp-and-swap atomic 4 3))
    (is (== 3 (threads:atomic-value atomic)))))

(define-test atomic-concurrency ()
  (let ((atomic (threads:make-atomic 4000))
        (incer (threads:spawn
                 (for x in (range 1 1000)
                   (threads:incf-atomic atomic 1))))
        (decer (threads:spawn
                 (for x in (range 1 1000)
                   (threads:decf-atomic atomic 1)))))
    (is (result:ok? (threads:join incer)))
    (is (result:ok? (threads:join decer)))
    (is (== 4000 (threads:atomic-value atomic)))))
