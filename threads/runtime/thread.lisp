;;;; thread.lisp
;;;;
;;;; Plain threads whose value, or the condition that ended them, is
;;;; delivered to the thread that joins them.

(in-package #:coalton/threads/runtime)

(define-condition thread-aborted (error)
  ((name :initarg :name :reader thread-aborted-name))
  (:report (lambda (condition stream)
             (format stream "The thread ~S ended without returning a value."
                     (thread-aborted-name condition))))
  (:documentation "Signaled by JOIN-THREAD for a thread that ended
without returning a value or signaling a serious condition, for example
because it was terminated. Coalton code catches it as the exception
ThreadAborted of coalton/threads/thread."))

(defstruct (thread-handle (:constructor %make-thread-handle ())
                          (:copier nil)
                          (:predicate nil))
  (thread nil)
  (result nil)
  (condition nil)
  (finished nil :type boolean))

(backend:freeze-type thread-handle)

(defun spawn-thread (fn name)
  "Start a thread named NAME that calls FN, a function of no arguments
returning one value. Output from the thread goes to the current
thread's output streams."
  (declare (type function fn)
           (type string name))
  (let ((handle (%make-thread-handle))
        (bindings (capture-standard-bindings)))
    (setf (thread-handle-thread handle)
          (backend:make-thread
           (lambda ()
             (with-bindings (bindings)
               (with-condition-capture (condition)
                   (setf (thread-handle-result handle) (funcall fn))
                 (setf (thread-handle-condition handle) condition))
               (setf (thread-handle-finished handle) t))
             nil)
           :name name))
    handle))

(defun join-thread (handle)
  "Wait for the thread of HANDLE to end and return its value. If the
thread signaled a serious condition, signal it again in the current
thread."
  (declare (type thread-handle handle))
  (let ((thread (thread-handle-thread handle)))
    (backend:join-thread thread)
    (cond ((thread-handle-condition handle)
           (error (thread-handle-condition handle)))
          ((thread-handle-finished handle)
           (thread-handle-result handle))
          (t
           (error 'thread-aborted :name (backend:thread-name thread))))))

(defun thread-alive-p (handle)
  (declare (type thread-handle handle))
  (backend:thread-alive-p (thread-handle-thread handle)))

(defun thread-handle-name (handle)
  (declare (type thread-handle handle))
  (or (backend:thread-name (thread-handle-thread handle)) ""))
