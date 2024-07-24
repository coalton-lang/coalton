;;; This package provides utilities for incremental consumption of
;;; concrete syntax tree values.
;;;
;;; The package has two main points. The first is the definition of a
;;; cursor struct that maintains a reference to the current value and a
;;; pointer within it, if it is cons-valued. The functions 'next' and
;;; 'empty-p' are sufficient to build more specialized parsing
;;; functions that don't need knowledge of the cst package.
;;;
;;; The second is the definition of a syntax-error condition that
;;; wraps a list of labeled spans. Many errors reflect to the current
;;; state of the cursor (empty, not cons-valued, pointing at wron
;;; type, etc.). A function is provided to convert syntax-error
;;; conditions to base/parse-error conditions.

(defpackage #:coalton-impl/parser/cursor
  (:use
   #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:se #:source-error))
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:export
   #:collect-symbols
   #:cursor-value
   #:do-every
   #:empty-p
   #:make-cursor
   #:make-note
   #:next
   #:next-symbol
   #:parse-error
   #:peek
   #:span-error
   #:syntax-error))

(in-package #:coalton-impl/parser/cursor)

;; The value field can be any type returned by cst, generally a cons
;; or atom.  When value is a cst:cons, pointer initially points to
;; that value, and is destructively updated as values are 'popped'.

(defstruct (cursor (:constructor %make-cursor))
  "A CST node-valued cursor."
  value                                  ; current value
  pointer)                               ; pointer into current value

(defun make-cursor (value)
  "Make a cursor that points at VALUE."
  (%make-cursor :value value :pointer value))

(defun peek (cursor)
  "Peek at the Lisp value of CURSOR without changing cursor state."
  (cst:raw (cursor-pointer cursor)))

(defun cons-p (cursor)
  "T if CURSOR is pointing at a cons."
  (declare (type cursor cursor))
  (or (cst:consp (cursor-pointer cursor))
      (null (peek cursor))))

(defun empty-p (cursor)
  "T if CURSOR has no next value."
  (declare (type cursor cursor))
  (let ((pointer (cursor-pointer cursor)))
    (or (not (cst:consp pointer))
        (null (cst:first pointer)))))

(defstruct note
  "A source text annotation. The TYPE field is compatible with base/parse-error."
  (span nil :type (cons integer integer) :read-only t) ; offsets into source
  (text nil :type string                 :read-only t) ; error message or label
  (type ':primary))                                    ; primary, secondary or help

(defmethod print-object ((self note) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "~(~a~): for input range ~a: ~a"
              (note-type self)
              (note-span self)
              (note-text self))))

(define-condition syntax-error (error)
  ((notes :initarg :notes
          :reader error-notes))
  (:report (lambda (condition stream)
             (format stream "cursor syntax-error~%~{~a~%~}"
                     (error-notes condition)))))

(defun pointer-span (cursor)
  "Return the span that CURSOR is pointing at.

If the wrapped node pointed to a non-empty cons, the span is the first value.
If the node is empty, the span is empty and points at the end of the wrapped value.
Otherwise, the span is the cst:source of the wrapped node."

  (declare (type cursor cursor))
  (let ((pointer (cursor-pointer cursor))
        (cons-p (cons-p cursor))
        (empty-p (empty-p cursor)))
    (cond ((and cons-p (not empty-p))
           (cst:source (cst:first pointer)))
          (cons-p
           (cons (1- (cdr (cst:source pointer)))
                 (cdr (cst:source pointer))))
          (t
           (cst:source pointer)))))

(declaim (inline span-error))
(defun span-error (span note)
  "Signal an error with message NOTE that points at SPAN."
  (error 'syntax-error
         :notes (list (make-note :span span
                                 :text note))))

(declaim (inline syntax-error))
(defun syntax-error (cursor note)
  "Signal an error with message NOTE that points at the current span indicated by CURSOR."
  (declare (type cursor cursor))
  (span-error (pointer-span cursor) note))

(defun next (cursor &key (pred nil) (unwrap t))
  "Return the next value from a nonempty cursor.

If PRED is non-NIL, only consume a value if it is true.
If UNWRAP is NIL, return the CST node, otherwise, return the raw value."
  (declare (type cursor cursor))
  (when (not (cons-p cursor))
    (syntax-error cursor "not a list"))
  (when (empty-p cursor)
    ;; Finding empty-p = t here this would indicate that the compiler
    ;; writer hasn't checked for emptiness in the calling context in
    ;; order to construct a more specific error message.
    (syntax-error cursor "attempt to read past end of list"))
  (let ((value (cst:first (cursor-pointer cursor))))
    (when (or (null pred)
              (funcall pred (cst:raw value)))
      (setf (cursor-pointer cursor)
            (cst:rest (cursor-pointer cursor)))
      (if unwrap (cst:raw value) value))))

(defun do-every (cursor fn)
  "Wrap each value in CURSOR in a subcursor, and call FN with it."
  (loop :until (empty-p cursor)
        :do (funcall fn (make-cursor (next cursor :unwrap nil)))))

;; type-specific helpers

(defun next-symbol (cursor &key message missing require)
  "Return the next value in CURSOR as a symbol. The cursor must be nonempty, and the next value must be a symbol."
  (when (empty-p cursor)
    (syntax-error cursor (or missing "symbol is missing")))
  (next cursor
        :pred (lambda (value)
                (when (or (null value)
                          (not (symbolp value)))
                  (syntax-error cursor
                                (or message "value must be a symbol")))
                (when (and require (not (string-equal require value)))
                  (syntax-error cursor
                                (or message
                                    (format nil "expected ~A" require))))
                t)))

(defun collect-symbols (cursor)
  "Return all remaining values in CURSOR as a list of symbols."
  (loop :until (empty-p cursor)
        :collect (next-symbol cursor)))

;; conversion to parse-error conditions

(defun help-note-p (note)
  "T if NOTE is a help note."
  (eq ':help (note-type note)))

(defun note->source-note (note)
  "Convert NOTE to a source error help note."
  (se:make-source-error-note :span (note-span note)
                             :type (note-type note)
                             :message (note-text note)))

(defun note->help-note (note)
  "Convert NOTE to a source error help note."
  (se:make-source-error-help :span (note-span note)
                             :replacement #'identity
                             :message (note-text note)))

(defun parse-error (source message syntax-error &optional notes)
  "Rethrow SYNTAX-ERROR as a PARSE-ERROR."
  (destructuring-bind (primary-note &rest secondary-notes)
      (append (error-notes syntax-error) notes)
    (error
     'parse-error
     :err (se:source-error :span (note-span primary-note)
                           :source source
                           :message message
                           :primary-note (note-text primary-note)
                           :notes (mapcar #'note->source-note
                                          (remove-if #'help-note-p secondary-notes))
                           :help-notes (mapcar #'note->help-note
                                               (remove-if-not #'help-note-p secondary-notes))))))
