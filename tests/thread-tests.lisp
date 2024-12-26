(in-package #:coalton-native-tests)

(define-test thread-spawn-and-join ()
  (let ((thread (threads:spawn)))
    (is (threads:alive? thread))
    (threads:join thread)
    (is (not (threads:alive? thread)))))

(define-test thread-current-thread ()
  (is (== (threads:current-thread)
          (lisp threads:Thread () bt2::*current-thread*))))

(define-test thread-all-threads ()
  (is (some?
       (find (fn (thread) (== thread (threads:current-thread)))
             (threads:all-threads)))))

(define-test thread-all-threads-count ()
  (let ((count (length (threads:all-threads)))
        (thread (threads:spawn (lisp Unit () (cl:sleep 60) Unit))))
    (is (== (length (threads:all-threads)) (1+ count)))
    (threads:destroy thread)
    (is (== (length (threads:all-threads)) count))))
