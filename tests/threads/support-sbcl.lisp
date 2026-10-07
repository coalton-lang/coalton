;;;; support-sbcl.lisp
;;;;
;;;; Test helpers that depend on SBCL: garbage collection and weak
;;;; pointers, interrupting threads, and deadlines.

(in-package #:coalton-tests/threads)

(defun make-weak-pointer (object)
  (sb-ext:make-weak-pointer object))

(defun weak-pointer-value (weak-pointer)
  "The object that WEAK-POINTER points to, or NIL if it was collected."
  (values (sb-ext:weak-pointer-value weak-pointer)))

(defmacro collect-garbage ()
  "Collect all garbage. Clears the unused part of the control stack
first, where stale references could keep objects alive. A macro, so
that the stack is cleared right below the caller's frame."
  '(progn
     (sb-sys:scrub-control-stack)
     (sb-ext:gc :full t)))

(defun interrupt-thread (thread function)
  "Make THREAD call FUNCTION, of no arguments, as soon as possible."
  (sb-thread:interrupt-thread thread function))

(defun terminate-thread (thread)
  "Make THREAD unwind and end as soon as possible."
  (sb-thread:terminate-thread thread))

(defmacro with-blocking-deadline ((seconds) &body body)
  "Evaluate BODY with a deadline of SECONDS for blocking operations, past
which they signal a timeout."
  `(sb-sys:with-deadline (:seconds ,seconds)
     ,@body))

(defun image-save-hook-p (symbol)
  "True if the function named SYMBOL is called before the image is saved."
  (and (member symbol sb-ext:*save-hooks*) t))
