;;;; package.lisp
;;;;
;;;; The interface between coalton/threads and the Lisp implementation
;;;; that it runs on.
;;;;
;;;; Everything that coalton/threads needs from the implementation,
;;;; beyond standard Common Lisp, is exported from this package. For
;;;; each supported implementation, a file of this directory defines
;;;; all of it, and a system of its own loads that file after this one:
;;;;
;;;;   sbcl.lisp       loaded by coalton/threads/sbcl
;;;;
;;;; coalton/threads depends on the system for the implementation it is
;;;; loaded in, and on other implementations on coalton/threads/
;;;; unsupported, which signals an error. To port coalton/threads to
;;;; another implementation, define everything below as described, in a
;;;; new file loaded by a new system. See "Porting" in threads/README.md.

(defpackage #:coalton/threads/backend
  (:documentation "The operations that coalton/threads needs from the Lisp
implementation, beyond standard Common Lisp.

This package is an implementation detail of coalton/threads. Its
interface may change without notice.")
  (:use #:cl)
  (:export
   ;;; Atomic operations and memory ordering
   ;;
   ;; ATOMIC-WORD is the type of unsigned integers of the size of a
   ;; machine word.
   ;;
   ;; The macro (COMPARE-AND-SWAP place old new) stores NEW in PLACE if
   ;; PLACE holds OLD, compared with EQ, and returns the value that
   ;; PLACE held before. The macros (ATOMIC-INCF place [delta]) and
   ;; (ATOMIC-DECF place [delta]) add DELTA, 1 by default, to PLACE, or
   ;; subtract it, and return the value that PLACE held before. These
   ;; operations are atomic and sequentially consistent. PLACE is a slot
   ;; of a structure, of type T, FIXNUM or ATOMIC-WORD, or the CAR or
   ;; CDR of a cons. ATOMIC-INCF and ATOMIC-DECF only operate on slots
   ;; of type ATOMIC-WORD, where the arithmetic wraps around, and on
   ;; conses holding fixnums.
   ;;
   ;; The macros (FULL-FENCE), (ACQUIRE-FENCE) and (RELEASE-FENCE) are
   ;; memory fences: no load or store is reordered across a full fence;
   ;; loads before an acquire fence are ordered before loads and stores
   ;; after it; and stores before a release fence are ordered before
   ;; stores after it. (SPIN-PAUSE) tells the processor that the calling
   ;; thread is spinning in a loop.
   #:atomic-word
   #:compare-and-swap
   #:atomic-incf
   #:atomic-decf
   #:full-fence
   #:acquire-fence
   #:release-fence
   #:spin-pause

   ;;; Definitions
   ;;
   ;; The macro (DEFGLOBAL name value [documentation]) is like DEFVAR,
   ;; except that NAME is never bound dynamically, which may make it
   ;; faster to access. The macro (FREEZE-TYPE name...) declares that
   ;; the structure types NAMEs will not be redefined or have subtypes
   ;; defined, which may make type checks faster; it may do nothing.
   #:defglobal
   #:freeze-type

   ;;; Threads
   ;;
   ;; (MAKE-THREAD function &key name) starts a thread that calls
   ;; FUNCTION with no arguments, and returns the thread. (JOIN-THREAD
   ;; thread) waits for THREAD to end, and returns the values that its
   ;; function returned, or NIL if the function did not return. Any
   ;; number of threads may join a thread, any number of times.
   ;; (THREAD-ALIVE-P thread), (THREAD-NAME thread) and (CURRENT-THREAD)
   ;; do as their names say. (THREAD-YIELD) offers the processor running
   ;; the calling thread to other threads.
   #:make-thread
   #:join-thread
   #:thread-alive-p
   #:thread-name
   #:current-thread
   #:thread-yield

   ;;; Mutexes
   ;;
   ;; MUTEX is the type of the mutexes made by (MAKE-MUTEX &key name),
   ;; which are not reentrant. (ACQUIRE-MUTEX mutex) acquires MUTEX,
   ;; waiting while another thread holds it. (TRY-ACQUIRE-MUTEX mutex)
   ;; acquires MUTEX and returns true if no thread holds it, and returns
   ;; false otherwise. (RELEASE-MUTEX mutex) releases MUTEX, which the
   ;; calling thread must hold. The macro (WITH-MUTEX (mutex) body...)
   ;; evaluates BODY while holding MUTEX, releasing it however BODY
   ;; exits.
   #:mutex
   #:make-mutex
   #:acquire-mutex
   #:try-acquire-mutex
   #:release-mutex
   #:with-mutex

   ;;; Condition variables
   ;;
   ;; CONDITION-VARIABLE is the type of the condition variables made by
   ;; (MAKE-CONDITION-VARIABLE &key name). (CONDITION-WAIT cv mutex &key
   ;; timeout) atomically releases MUTEX, which the calling thread
   ;; holds, and sleeps until CV is notified or, if TIMEOUT is non-NIL,
   ;; until TIMEOUT seconds have passed. The sleep may also end
   ;; spuriously. It acquires MUTEX again before returning, whether or
   ;; not the time ran out, and returns false if it did and true
   ;; otherwise. (CONDITION-NOTIFY cv) wakes one thread waiting on CV,
   ;; if any, and (CONDITION-BROADCAST cv) wakes all of them.
   #:condition-variable
   #:make-condition-variable
   #:condition-wait
   #:condition-notify
   #:condition-broadcast

   ;;; Semaphores
   ;;
   ;; SEMAPHORE is the type of the counting semaphores made by
   ;; (MAKE-SEMAPHORE &key count name), which hold COUNT permits, 0 by
   ;; default. (SIGNAL-SEMAPHORE semaphore [n]) adds N permits, 1 by
   ;; default, waking threads that wait for them. (WAIT-ON-SEMAPHORE
   ;; semaphore &key timeout) takes a permit, waiting until one is
   ;; available or, if TIMEOUT is non-NIL, until TIMEOUT seconds have
   ;; passed; it returns true if it took a permit and false otherwise.
   ;; (TRY-SEMAPHORE semaphore) takes a permit and returns true if one
   ;; is available, and returns false otherwise. (SEMAPHORE-COUNT
   ;; semaphore) is the number of available permits.
   #:semaphore
   #:make-semaphore
   #:signal-semaphore
   #:wait-on-semaphore
   #:try-semaphore
   #:semaphore-count

   ;;; Mailboxes: unbounded first-in, first-out queues of messages, on
   ;;; which threads can wait
   ;;
   ;; MAILBOX is the type of the mailboxes made by (MAKE-MAILBOX &key
   ;; name). (SEND-MESSAGE mailbox message) adds MESSAGE to MAILBOX,
   ;; without waiting. (RECEIVE-MESSAGE mailbox &key timeout) removes
   ;; the oldest message of MAILBOX and returns it and true, waiting
   ;; until there is one or, if TIMEOUT is non-NIL, until TIMEOUT seconds
   ;; have passed, in which case it returns NIL and NIL.
   ;; (RECEIVE-MESSAGE-NO-HANG mailbox) does the same without waiting.
   ;; (MAILBOX-COUNT mailbox) is the number of messages in MAILBOX, and
   ;; (MAILBOX-EMPTY-P mailbox) tells whether there are none; other
   ;; threads may change that at any time.
   #:mailbox
   #:make-mailbox
   #:send-message
   #:receive-message
   #:receive-message-no-hang
   #:mailbox-count
   #:mailbox-empty-p

   ;;; Queues: unbounded first-in, first-out queues that never wait,
   ;;; used by the scheduler
   ;;
   ;; QUEUE is the type of the queues made by (MAKE-QUEUE &key name).
   ;; (ENQUEUE item queue) adds ITEM, which is not NIL, to QUEUE.
   ;; (DEQUEUE queue) removes and returns the oldest item of QUEUE, or
   ;; returns NIL if QUEUE is empty. (QUEUE-EMPTY-P queue) tells whether
   ;; QUEUE is empty, and (QUEUE-COUNT queue) is the number of items in
   ;; QUEUE; other threads may change that at any time. Any number of
   ;; threads may use a queue at the same time.
   #:queue
   #:make-queue
   #:enqueue
   #:dequeue
   #:queue-empty-p
   #:queue-count

   ;;; Interrupts, deadlines, handlers and restarts
   ;;
   ;; Some implementations can interrupt a thread, making it call a
   ;; function that may unwind it, and SBCL can also impose a deadline
   ;; on blocking operations, which then signal a timeout. The macro
   ;; (UNINTERRUPTIBLY body...) evaluates BODY so that neither can
   ;; unwind it halfway: interrupts are deferred until BODY exits, and
   ;; no deadline is in effect. Within it, the macro
   ;; (WITH-LOCAL-INTERRUPTS body...) allows interrupts again while it
   ;; evaluates BODY. The macro (WITHOUT-DEADLINE body...) evaluates
   ;; BODY with no deadline in effect. On implementations without
   ;; interrupts or deadlines, these macros just evaluate BODY.
   ;;
   ;; (CAPTURE-HANDLERS-AND-RESTARTS) returns an object representing the
   ;; condition handlers and restarts in effect in the calling thread.
   ;; The macro (WITH-HANDLERS-AND-RESTARTS (captured) body...)
   ;; evaluates BODY with the handlers and restarts represented by
   ;; CAPTURED in effect, instead of the current ones, so that BODY
   ;; signals conditions and finds restarts as if it were evaluated
   ;; where CAPTURED was captured. The scheduler uses them to run tasks
   ;; with the handlers and restarts of the base of a worker thread.
   #:uninterruptibly
   #:with-local-interrupts
   #:without-deadline
   #:capture-handlers-and-restarts
   #:with-handlers-and-restarts

   ;;; The process
   ;;
   ;; (AVAILABLE-CPU-COUNT) is the number of processors that the process
   ;; may run on, at least 1. (MONOTONIC-SECONDS) is the time in
   ;; seconds since an arbitrary origin, from a clock that never goes
   ;; backwards, as precisely as the implementation can tell; benchmarks
   ;; use it. (REGISTER-IMAGE-SAVE-HOOK symbol) arranges for the function
   ;; named SYMBOL to be called with no arguments before the Lisp image
   ;; is saved, which fails while other threads are running.
   #:available-cpu-count
   #:monotonic-seconds
   #:register-image-save-hook))
