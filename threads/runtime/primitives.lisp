;;;; primitives.lisp
;;;;
;;;; Building blocks for the scheduler: the number of workers, a
;;;; park/unpark primitive for sleeping threads, and the special
;;;; bindings of worker threads.

(in-package #:coalton/threads/runtime)

;;; Number of workers

(defun %parse-positive-integer (string)
  (let ((n (and string (ignore-errors (parse-integer string)))))
    (and (integerp n) (plusp n) n)))

(defun default-worker-count ()
  "The pool size used when none was configured: the value of the
environment variable COALTON_NUM_THREADS if it is a positive integer,
or else the number of available processors."
  (or (%parse-positive-integer (uiop:getenv "COALTON_NUM_THREADS"))
      (backend:available-cpu-count)))

;;; Parking
;;;
;;; A parker lets one thread sleep until another wakes it. UNPARK
;;; grants a permit; PARK consumes the permit, sleeping until one is
;;; available. A permit granted while nobody is parked makes the next
;;; PARK return immediately, so wakeups cannot be lost. Callers must
;;; tolerate spurious returns from PARK.

(defstruct (parker (:constructor make-parker ())
                   (:copier nil)
                   (:predicate nil))
  (mutex (backend:make-mutex :name "Coalton parker")
   :type backend:mutex :read-only t)
  (condition-variable (backend:make-condition-variable :name "Coalton parker")
   :type backend:condition-variable :read-only t)
  (permit nil :type boolean)
  ;; True while the owner of the parker is asleep or about to be, for
  ;; wakers that only need to wake it then. Set and cleared by its owner.
  (asleep nil :type boolean))

(backend:freeze-type parker)

(defun unpark (parker)
  "Grant PARKER a permit, waking its thread if it is parked."
  (declare (type parker parker))
  (backend:uninterruptibly
    (backend:with-mutex ((parker-mutex parker))
      (setf (parker-permit parker) t)
      (backend:condition-notify (parker-condition-variable parker))))
  (values))

(defun park (parker)
  "Sleep until PARKER has a permit, then consume it."
  (declare (type parker parker))
  (let ((mutex (parker-mutex parker)))
    (backend:with-mutex (mutex)
      ;; CONDITION-WAIT can return spuriously.
      (loop :until (parker-permit parker)
            :do (backend:condition-wait (parker-condition-variable parker) mutex))
      (setf (parker-permit parker) nil)))
  (values))

;;; Dynamic environment of worker threads

(defun capture-standard-bindings ()
  "An alist of special bindings, taken from the current thread, to be
established in threads that run Coalton code on its behalf, so that
output from those threads goes where the current thread's output goes."
  (list (cons '*standard-output* *standard-output*)
        (cons '*error-output* *error-output*)
        (cons '*trace-output* *trace-output*)))

(defmacro with-bindings ((bindings) &body body)
  "Execute BODY with the special bindings in the alist BINDINGS."
  (let ((b (gensym "BINDINGS")))
    `(let ((,b ,bindings))
       (progv (mapcar #'car ,b) (mapcar #'cdr ,b)
         ,@body))))
