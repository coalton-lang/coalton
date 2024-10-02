;;;; This package contains the function LOAD-TEST-FILE for loading
;;;; parser tests from specially formatted files.
;;;;
;;;; - The format is documented in tests/parser-test-files/README.md
;;;; - Tests are defined in tests/parser-test-files/*.txt
;;;; - These tests are loaded and run by tests/parser-tests.lisp
;;;; - COALTON-TESTS:RUN-TEST-FILE and COALTON-TESTS:RUN-TEST are
;;;;   defined in tests/utilities.lisp
;;;;
;;;; To execute a single suite:
;;;;
;;;;   (coalton-tests:run-test-file #<pathname>)
;;;;
;;;; To run a single numbered test without condition handlers:
;;;;
;;;;   (coalton-tests:run-test #<pathname> N)

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
  "Collect a line in a given section."
  (with-next-state state
    (case (loader-state state)
      (:header (push line (loader-header state)))
      (:program (push line (loader-program state)))
      (:error (push line (loader-error state)))
      (error "Invalid state, collecting line ~A" (loader-line state)))
    state))

(defun combine-lines (lines)
  "Reassemble collected lines into a single string."
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

(defun read-flags (string)
  "Read the first parenthesis-delimited form appearing in STRING, or nil."
  (let ((open (position #\( string))
        (close (position #\) string)))
    (when (and open close (< open close))
      (with-input-from-string (stream (subseq string open (1+ close)))
        (read stream)))))

(defun get-case (state)
  "Assemble a complete test case based on collected header and section values."
  (let ((header (combine-lines (loader-header state))))
    (list (loader-start state)
          (first-integer header)
          (read-flags header)
          header
          (combine-lines (loader-program state))
          (combine-lines (loader-error state)))))

(defun collect-case (state)
  "Assemble a test case, collect it, and reset to empty state."
  (with-next-state state
    (push (get-case state) (loader-cases state))
    (setf (loader-header state) nil
          (loader-program state) nil
          (loader-error state) nil)
    state))

(defun start-case (state)
  "Perform bookkeeping to begin collecting a test case."
  (with-next-state state
    (setf (loader-start state) (loader-line state))
    state))

(defun line-start-p (line char)
  "Return T if LINE begins with CHAR."
  (and (plusp (length line))
       (char= (aref line 0) char)))

(defun process-line (state line)
  "Step the case reader state machine by consuming a single line of input."
  (with-next-state state
    (incf (loader-line state))
    (cond ((null line)
           (case (loader-state state)
             ((:error :program) (next-state (collect-case state) ':done))
             (error "Invalid state at end of input, collecting line ~A" (loader-line state))))
          ((line-start-p line #\=)
           (case (loader-state state)
             (:init (next-state (start-case state) ':header))
             (:header (next-state state ':program))
             ((:error :program) (next-state (start-case (collect-case state)) ':header))
             (error "Invalid state at header divider, collecting line ~A" (loader-line state))))
          ((line-start-p line #\-)
           (ecase (loader-state state)
             (:program (next-state state ':error))
             (error "Invalid state at output divider, collecting line ~A" (loader-line state))))
          (t
           (collect-line state line)))))

(defun load-test-file (pathname)
  "Load a set of test cases from PATHNAME."
  (with-open-file (stream pathname :direction ':input :element-type 'character)
    (loop :for state := (make-loader)
            :then (process-line state (read-line stream nil nil))
          :when (eq ':done (loader-state state))
            :return (reverse (loader-cases state)))))
