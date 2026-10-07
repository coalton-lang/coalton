;;;; sbcl.lisp
;;;;
;;;; The backend of coalton/threads for SBCL. See package.lisp for what
;;;; each definition must do.

(in-package #:coalton/threads/backend)

(unless (member :sb-thread *features*)
  (error "coalton/threads requires an SBCL built with thread support (:SB-THREAD)."))

;;; Atomic operations and memory ordering
;;;
;;; The lock-free structures of the runtime follow the C11 formulation
;;; of the Chase-Lev deque by Lê, Pop, Cohen and Zappa Nardelli,
;;; "Correct and Efficient Work-Stealing for Weak Memory Models"
;;; (PPoPP 2013). The orderings are expressed with SBCL's barriers. On
;;; x86-64 only FULL-FENCE emits an instruction; the other two are
;;; compiler barriers. On ARM64 they become DMB instructions.

(deftype atomic-word ()
  'sb-ext:word)

(defmacro compare-and-swap (place old new)
  `(sb-ext:compare-and-swap ,place ,old ,new))

(defmacro atomic-incf (place &optional (delta 1))
  `(sb-ext:atomic-incf ,place ,delta))

(defmacro atomic-decf (place &optional (delta 1))
  `(sb-ext:atomic-decf ,place ,delta))

(defmacro full-fence ()
  (if (member :x86-64 *features*)
      ;; On x86-64 a locked instruction is a full barrier, and is several
      ;; times cheaper than MFENCE on current processors. Operate on a
      ;; stack-allocated cell so that no other thread contends for it.
      '(let ((cell (list nil)))
         (declare (dynamic-extent cell))
         (sb-thread:barrier (:compiler))
         (sb-ext:compare-and-swap (car cell) nil nil)
         (sb-thread:barrier (:compiler))
         (values))
      '(sb-thread:barrier (:memory))))

(defmacro acquire-fence ()
  '(sb-thread:barrier (:read)))

(defmacro release-fence ()
  '(sb-thread:barrier (:write)))

(declaim (inline spin-pause))
(defun spin-pause ()
  (sb-ext:spin-loop-hint))

;;; Definitions

(defmacro defglobal (name value &optional (documentation nil documentation-p))
  `(sb-ext:defglobal ,name ,value ,@(and documentation-p (list documentation))))

(defmacro freeze-type (&rest names)
  `(declaim (sb-ext:freeze-type ,@names)))

;;; Threads

(declaim (inline make-thread join-thread thread-alive-p thread-name
                 current-thread thread-yield))

(defun make-thread (function &key name)
  (sb-thread:make-thread function :name name))

(defun join-thread (thread)
  (sb-thread:join-thread thread :default nil))

(defun thread-alive-p (thread)
  (sb-thread:thread-alive-p thread))

(defun thread-name (thread)
  (sb-thread:thread-name thread))

(defun current-thread ()
  sb-thread:*current-thread*)

(defun thread-yield ()
  (sb-thread:thread-yield))

;;; Mutexes

(deftype mutex ()
  'sb-thread:mutex)

(declaim (inline make-mutex acquire-mutex try-acquire-mutex release-mutex))

(defun make-mutex (&key name)
  (sb-thread:make-mutex :name name))

(defun acquire-mutex (mutex)
  (sb-thread:grab-mutex mutex))

(defun try-acquire-mutex (mutex)
  (sb-thread:grab-mutex mutex :waitp nil))

(defun release-mutex (mutex)
  (sb-thread:release-mutex mutex))

(defmacro with-mutex ((mutex) &body body)
  `(sb-thread:with-mutex (,mutex)
     ,@body))

;;; Condition variables

(deftype condition-variable ()
  'sb-thread:waitqueue)

(declaim (inline make-condition-variable condition-notify condition-broadcast))

(defun make-condition-variable (&key name)
  (sb-thread:make-waitqueue :name name))

(defun condition-wait (condition-variable mutex &key timeout)
  (cond ((sb-thread:condition-wait condition-variable mutex :timeout timeout)
         t)
        (t
         ;; SBCL does not reacquire the mutex when the time runs out.
         (unless (sb-thread:holding-mutex-p mutex)
           (sb-thread:grab-mutex mutex))
         nil)))

(defun condition-notify (condition-variable)
  (sb-thread:condition-notify condition-variable))

(defun condition-broadcast (condition-variable)
  (sb-thread:condition-broadcast condition-variable))

;;; Semaphores

(deftype semaphore ()
  'sb-thread:semaphore)

(declaim (inline make-semaphore signal-semaphore wait-on-semaphore
                 try-semaphore semaphore-count))

(defun make-semaphore (&key (count 0) name)
  (sb-thread:make-semaphore :count count :name name))

(defun signal-semaphore (semaphore &optional (n 1))
  (sb-thread:signal-semaphore semaphore n))

(defun wait-on-semaphore (semaphore &key timeout)
  ;; SBCL returns the new count, which may be 0, or NIL on timeout.
  (and (sb-thread:wait-on-semaphore semaphore :timeout timeout) t))

(defun try-semaphore (semaphore)
  (and (sb-thread:try-semaphore semaphore) t))

(defun semaphore-count (semaphore)
  (sb-thread:semaphore-count semaphore))

;;; Mailboxes

(deftype mailbox ()
  'sb-concurrency:mailbox)

(declaim (inline make-mailbox send-message receive-message
                 receive-message-no-hang mailbox-count mailbox-empty-p))

(defun make-mailbox (&key name)
  (sb-concurrency:make-mailbox :name name))

(defun send-message (mailbox message)
  (sb-concurrency:send-message mailbox message))

(defun receive-message (mailbox &key timeout)
  (sb-concurrency:receive-message mailbox :timeout timeout))

(defun receive-message-no-hang (mailbox)
  (sb-concurrency:receive-message-no-hang mailbox))

(defun mailbox-count (mailbox)
  (sb-concurrency:mailbox-count mailbox))

(defun mailbox-empty-p (mailbox)
  (sb-concurrency:mailbox-empty-p mailbox))

;;; Queues

(deftype queue ()
  'sb-concurrency:queue)

(declaim (inline make-queue enqueue dequeue queue-empty-p queue-count))

(defun make-queue (&key name)
  (sb-concurrency:make-queue :name name))

(defun enqueue (item queue)
  (sb-concurrency:enqueue item queue))

(defun dequeue (queue)
  (values (sb-concurrency:dequeue queue)))

(defun queue-empty-p (queue)
  (sb-concurrency:queue-empty-p queue))

(defun queue-count (queue)
  (sb-concurrency:queue-count queue))

;;; Interrupts, deadlines, handlers and restarts

(defmacro uninterruptibly (&body body)
  `(sb-sys:with-deadline (:seconds nil :override t)
     (sb-sys:without-interrupts
       ,@body)))

;;; SB-SYS:WITH-LOCAL-INTERRUPTS may only be used lexically within
;;; SB-SYS:WITHOUT-INTERRUPTS, which is where UNINTERRUPTIBLY puts the
;;; forms that use this macro.
(defmacro with-local-interrupts (&body body)
  `(sb-sys:with-local-interrupts
     ,@body))

(defmacro without-deadline (&body body)
  `(sb-sys:with-deadline (:seconds nil :override t)
     ,@body))

(defun capture-handlers-and-restarts ()
  (cons sb-kernel:*handler-clusters* sb-kernel:*restart-clusters*))

(defmacro with-handlers-and-restarts ((captured) &body body)
  (let ((c (gensym "CAPTURED")))
    `(let* ((,c ,captured)
            (sb-kernel:*handler-clusters* (car ,c))
            (sb-kernel:*restart-clusters* (cdr ,c)))
       ,@body)))

;;; The process

(defun %positive-integer (string)
  (let ((n (and string (ignore-errors (parse-integer string)))))
    (and (integerp n) (plusp n) n)))

#+linux
(defun %affinity-cpu-count ()
  "Number of CPUs in this process's affinity mask, or NIL."
  (sb-alien:with-alien ((mask (array (sb-alien:unsigned 64) 16)))
    (let ((result (sb-alien:alien-funcall
                   (sb-alien:extern-alien
                    "sched_getaffinity"
                    (function sb-alien:int
                              sb-alien:int
                              sb-alien:unsigned-long
                              (* (array (sb-alien:unsigned 64) 16))))
                   0
                   (* 8 16)
                   (sb-alien:addr mask))))
      (when (zerop result)
        (let ((count (loop :for i :below 16
                           :sum (logcount (sb-alien:deref mask i)))))
          (and (plusp count) count))))))

#+unix
(defun %sysconf-cpu-count ()
  "Number of online CPUs according to sysconf(3), or NIL."
  (let ((name (find-symbol "SC-NPROCESSORS-ONLN" "SB-UNIX")))
    (when (and name (boundp name))
      (let ((count (sb-alien:alien-funcall
                    (sb-alien:extern-alien "sysconf"
                                           (function sb-alien:long sb-alien:int))
                    (symbol-value name))))
        (and (plusp count) count)))))

(defun available-cpu-count ()
  ;; The functions that call the operating system exist only where it
  ;; provides them.
  (or #+linux (ignore-errors (%affinity-cpu-count))
      #+unix (ignore-errors (%sysconf-cpu-count))
      ;; Set by Windows.
      (%positive-integer (sb-ext:posix-getenv "NUMBER_OF_PROCESSORS"))
      1))

(defun monotonic-seconds ()
  ;; SB-UNIX:CLOCK-GETTIME does not exist on every platform, such as
  ;; Windows, where GET-INTERNAL-REAL-TIME serves instead.
  (let* ((package (find-package "SB-UNIX"))
         (clock-gettime (and package (find-symbol "CLOCK-GETTIME" package)))
         (clock-monotonic (and package (find-symbol "CLOCK-MONOTONIC" package))))
    (if (and clock-gettime (fboundp clock-gettime)
             clock-monotonic (boundp clock-monotonic))
        (multiple-value-bind (seconds nanoseconds)
            (funcall clock-gettime (symbol-value clock-monotonic))
          (+ seconds (/ nanoseconds 1d9)))
        (/ (get-internal-real-time)
           (float internal-time-units-per-second 1d0)))))

(defun register-image-save-hook (symbol)
  (pushnew symbol sb-ext:*save-hooks*)
  (values))
