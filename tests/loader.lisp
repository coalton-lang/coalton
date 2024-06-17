;;; This package contains the function #'load-suite for loading parser
;;; test suites from specially formatted files.
;;;
;;; The format is:
;;;
;;;     ======
;;;     <test name 1>
;;;     ======
;;;     <invalid coalton program text>
;;;     ------
;;;     <expeced error message>
;;;     ======
;;;     <test name 2>
;;;     ...
;;;
;;; etc.
;;;
;;; See example in tests/parser-test-files/package.txt
;;;
;;; Use (load-suite #<pathname>) to load a suite, or (run-suite
;;; #<pathname>) to run one.  #'run-suite is defined in
;;; tests/utilities.lisp.

(defpackage #:coalton-tests/loader
  (:documentation "Load a test suite from a delimited file.")
  (:use
   #:cl)
  (:export
   #:load-suite))

(in-package #:coalton-tests/loader)

(defstruct loader
  "State machine for reading test files."
  (state ':init)
  (line 0)
  (start 0)
  header
  program
  error
  cases)

(defmacro with-next-state (state &body body)
  "Helper for functional state updates."
  `(let ((,state (copy-loader ,state)))
     ,@body))

(defun next-state (state key)
  "Update state, returning new instance."
  (with-next-state state
    (setf (loader-state state) key)
    state))

(defun collect-line (state line)
  "Collect a line into a given section"
  (with-next-state state
    (ecase (loader-state state)
      (:header (push line (loader-header state)))
      (:program (push line (loader-program state)))
      (:error (push line (loader-error state))))
    state))

(defun combine-lines (lines)
  (string-trim '(#\Space #\Newline)
               (format nil "~{~A~%~}" (reverse lines))))

(defun get-case (state)
  (list (loader-start state)
        (combine-lines (loader-header state))
        (combine-lines (loader-program state))
        (combine-lines (loader-error state))))

(defun collect-case (state)
  (with-next-state state
    (push (get-case state) (loader-cases state))
    (setf (loader-header state) nil
          (loader-program state) nil
          (loader-error state) nil)
    state))

(defun start-case (state)
  (with-next-state state
    (setf (loader-start state) (loader-line state))
    state))

(defun line-start-p (line char)
  (and (plusp (length line))
       (char= (aref line 0) char)))

(defun process-line (state line)
  (with-next-state state
    (incf (loader-line state))
    (cond ((null line)
           (ecase (loader-state state)
             (:error  (next-state (collect-case state) ':done))))
          ((line-start-p line #\=)
           (ecase (loader-state state)
             (:init (next-state (start-case state) ':header))
             (:header (next-state state ':program))
             (:error (next-state (start-case (collect-case state)) ':header))))
          ((line-start-p line #\-)
           (ecase (loader-state state)
             (:program (next-state state ':error))))
          (t
           (collect-line state line)))))

(defun load-suite (pathname)
  (with-open-file (stream pathname :direction ':input :element-type 'character)
    (loop :for state := (make-loader)
            :then (process-line state (read-line stream nil nil))
          :when (eq ':done (loader-state state))
            :return (reverse (loader-cases state)))))
