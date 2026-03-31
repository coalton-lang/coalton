;;;; debug.lisp -- Debugger integration for the runtime server.
;;;;
;;;; Captures conditions, lists restarts, and invokes them by index.

(in-package #:mine/runtime/debug)

;;; State

(defvar *active-restarts* nil
  "The list of restarts captured by the most recent condition handler.
Each entry is (index restart-object name-string report-string).")

(defvar *active-condition* nil
  "The condition currently being handled, or NIL.")

;;; Public API

(defmacro with-debugger ((&key on-condition) &body body)
  "Execute BODY with a debugger hook that intercepts all conditions.
ON-CONDITION is called with (condition restarts-alist) when a condition
is signaled that enters the debugger.  Restarts-alist is a list of
 (index name-string report-string) entries."
  (let ((callback (gensym "CALLBACK")))
    `(let ((,callback ,on-condition))
       (let ((*debugger-hook*
               (lambda (condition hook)
                 (declare (ignore hook))
                 (setf *active-condition* condition)
                 (let ((restarts (compute-restarts condition)))
                   (setf *active-restarts*
                         (loop :for r :in restarts
                               :for i :from 0
                               :collect (list i r
                                             (princ-to-string
                                              (restart-name r))
                                             (handler-case
                                                 (with-output-to-string (s)
                                                   (princ r s))
                                               (error () "")))))
                   (when ,callback
                     (funcall ,callback
                              condition
                              (mapcar (lambda (entry)
                                        (list (first entry)
                                              (third entry)
                                              (fourth entry)))
                                      *active-restarts*)))))))
         ,@body))))

(defun list-restarts ()
  "Return a list of (index name-string description-string) for active restarts.
Returns NIL if no debugger session is active."
  (mapcar (lambda (entry)
            (list (first entry) (third entry) (fourth entry)))
          *active-restarts*))

(defun invoke-restart-by-index (index)
  "Invoke the restart at INDEX in the current restart list.
Returns NIL if the index is out of range or no restarts are active."
  (let ((entry (find index *active-restarts* :key #'first)))
    (when entry
      (let ((restart-obj (second entry)))
        (invoke-restart restart-obj)))))
