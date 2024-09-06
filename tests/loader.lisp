;;;; This package contains the function LOAD-TEST-FILE for loading
;;;; parser tests from specially formatted files.
;;;;
;;;; The format is:
;;;;
;;;;     ======
;;;;     <header>
;;;;     ======
;;;;     <program>
;;;;     ------
;;;;     <error message>
;;;;       or
;;;;     <empty string, to assert that the program must compile without error>
;;;;     ======
;;;;     <header 2>
;;;;     ...
;;;;
;;;; The first sequence of one or more consecutive numeric characters
;;;; in the test header will be interpreted as a test number, for
;;;; reexecution of single tests.
;;;;
;;;; There are examples in tests/parser-test-files/*.txt
;;;;
;;;; Use
;;;;
;;;;     (load-test-file #<pathname>)
;;;;
;;;; to load tests,
;;;;
;;;;     (run-test-file #<pathname>)
;;;;
;;;; to run tests, and
;;;;
;;;;     (run-test #<pathname> N)
;;;;
;;;; to run a single numbered test without condition handlers.
;;;;
;;;; RUN-TEST-FILE and RUN-TEST are defined in tests/utilities.lisp

(defpackage #:coalton-tests/loader
  (:documentation "Load tests from a delimited file.")
  (:use
   #:cl)
  (:export
   #:load-test-file))

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

(defun first-integer (string)
  "Return the first integer appearing in STRING, or nil."
  (let ((start (position-if #'digit-char-p string)))
    (when start
      (let* ((string (subseq string start))
             (end (position-if (complement #'digit-char-p) string)))
        (parse-integer (if end
                           (subseq string 0 end)
                           string))))))

(defun get-case (state)
  (let ((header (combine-lines (loader-header state))))
    (list (loader-start state)
          (first-integer header)
          header
          (combine-lines (loader-program state))
          (combine-lines (loader-error state)))))

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

(defun load-test-file (pathname)
  "Load PATHNAME, a file containing multiple compiler tests."
  (with-open-file (stream pathname :direction ':input :element-type 'character)
    (loop :for state := (make-loader)
            :then (process-line state (read-line stream nil nil))
          :when (eq ':done (loader-state state))
            :return (reverse (loader-cases state)))))
