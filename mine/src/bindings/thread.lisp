;;;; thread.lisp -- SBCL-specific threading bindings.
;;;;
;;;; Thin wrappers around sb-thread for use from Coalton
;;;; via the `lisp` escape hatch.

(in-package #:mine/bindings/thread)

;;; Thread creation

(defun make-thread (name function)
  "Create and start a new thread named NAME that executes FUNCTION.
FUNCTION is a thunk (zero-argument function).
Returns the thread object."
  (declare (type string name)
           (type function function))
  (sb-thread:make-thread function :name name))

;;; Mutexes

(defun make-mutex (&optional name)
  "Create a new mutex, optionally named NAME."
  (if name
      (sb-thread:make-mutex :name name)
      (sb-thread:make-mutex)))

(defmacro with-mutex ((mutex) &body body)
  "Execute BODY while holding MUTEX.
The mutex is acquired before BODY and released when BODY exits,
whether normally or via a non-local exit."
  `(sb-thread:with-mutex (,mutex)
     ,@body))

;;; Condition variables (waitqueues)

(defun make-waitqueue (&optional name)
  "Create a new waitqueue (condition variable), optionally named NAME."
  (if name
      (sb-thread:make-waitqueue :name name)
      (sb-thread:make-waitqueue)))

(defun condition-wait (waitqueue mutex)
  "Release MUTEX, wait on WAITQUEUE until signaled, then reacquire MUTEX.
Must be called while holding MUTEX."
  (sb-thread:condition-wait waitqueue mutex)
  (values))

(defun condition-notify (waitqueue)
  "Wake one thread waiting on WAITQUEUE."
  (sb-thread:condition-notify waitqueue)
  (values))

;;; Thread queries

(defun thread-alive-p (thread)
  "Return T if THREAD is still running."
  (sb-thread:thread-alive-p thread))

(defun current-thread ()
  "Return the current thread object."
  sb-thread:*current-thread*)

(defun thread-name (thread)
  "Return the name string of THREAD, or NIL if unnamed."
  (sb-thread:thread-name thread))
