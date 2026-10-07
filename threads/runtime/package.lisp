;;;; package.lisp
;;;;
;;;; The runtime underlying coalton/threads: a work-stealing scheduler
;;;; for fork-join task parallelism, written in Common Lisp and used by
;;;; the Coalton packages of this system. It depends on the Lisp
;;;; implementation only through the package COALTON/THREADS/BACKEND.

(defpackage #:coalton/threads/runtime
  (:documentation "The work-stealing scheduler underlying coalton/threads.

This package is an implementation detail of the Coalton packages in
coalton/threads. Its interface may change without notice.")
  (:use #:cl)
  (:local-nicknames
   (#:backend #:coalton/threads/backend))
  (:export
   ;; Configuration
   #:*debug-tasks*

   ;; Conditions
   #:task-aborted
   #:thread-aborted

   ;; Plain threads
   #:thread-handle
   #:spawn-thread
   #:join-thread
   #:thread-alive-p
   #:thread-handle-name

   ;; Pool management
   #:worker-count
   #:set-worker-count
   #:shutdown
   #:current-worker-index

   ;; Tasks
   #:join2
   #:call-in-pool
   #:job
   #:spawn-future
   #:await-future
   #:future-done-p
   #:scope
   #:call-with-scope
   #:scope-spawn

   ;; Data parallelism
   #:parallel-for
   #:parallel-for-chunks
   #:parallel-reduce
   #:parallel-reduce-chunks
   #:parallel-map-vector
   #:parallel-map-list
   #:parallel-sort))
