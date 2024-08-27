;;; This package provides utilities for incremental consumption of
;;; concrete syntax tree values.

(defpackage #:coalton-impl/parser/cursor
  (:use
   #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:source #:coalton-impl/source))
  (:export
   #:collect-symbols
   #:cursor-source
   #:cursor-value
   #:cursor-error
   #:end-error
   #:do-every
   #:empty-p
   #:make-cursor
   #:make-note
   #:next
   #:next-symbol
   #:peek))

(in-package #:coalton-impl/parser/cursor)

;; The value field can be any type returned by cst, generally a cons
;; or atom.  When value is a cst:cons, pointer initially points to
;; that value, and is destructively updated as values are 'popped'.

(defstruct (cursor (:constructor %make-cursor))
  "A CST node-valued cursor."
  source
  value                                  ; current value
  pointer)                               ; pointer into current value

(defun make-cursor (source value)
  "Make a cursor that points at VALUE."
  (%make-cursor :source source :value value :pointer value))

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

(defun cursor-error (cursor message note)
  (source:source-error message
                       (source:make-note (source:make-location (cursor-source cursor)
                                                               (cursor-value cursor))
                                         note)))

(defun end-error (cursor message note)
  (source:source-error message
                       (source:make-note (source:make-end-location (cursor-source cursor)
                                                                   (cursor-value cursor))
                                         note)))

(defun next (cursor &key (pred nil) (unwrap t))
  "Return the next value from a nonempty cursor.

If PRED is non-NIL, only consume a value if it is true.
If UNWRAP is NIL, return the CST node, otherwise, return the raw value."
  (declare (type cursor cursor))
  (when (not (cons-p cursor))
    (cursor-error cursor "type error" "not a list"))
  (when (empty-p cursor)
    ;; Finding empty-p = t here this would indicate that the compiler
    ;; writer hasn't checked for emptiness in the calling context in
    ;; order to construct a more specific error message.
    (cursor-error cursor "read error" "attempt to read past end of list"))
  (let ((value (cst:first (cursor-pointer cursor))))
    (when (or (null pred)
              (funcall pred (cst:raw value) (cst:source value)))
      (setf (cursor-pointer cursor)
            (cst:rest (cursor-pointer cursor)))
      (if unwrap (cst:raw value) value))))

(defun do-every (cursor fn)
  "Wrap each value in CURSOR in a subcursor, and call FN with it."
  (loop :until (empty-p cursor)
        :do (funcall fn (make-cursor (cursor-source cursor)
                                     (next cursor :unwrap nil)))))

;; type-specific helpers

(defun next-symbol (cursor &key message note missing require)
  "Return the next value in CURSOR as a symbol. The cursor must be nonempty, and the next value must be a symbol."
  (when (empty-p cursor)
    (end-error cursor message (or missing "symbol is missing")))
  (next cursor
        :pred (lambda (value span)
                (when (or (null value)
                          (not (symbolp value)))
                  (source:source-error message
                                       (source:make-note (source:make-location (cursor-source cursor)
                                                                               span)
                                                         (or note "value must be a symbol"))))
                (when (and require (not (string-equal require value)))
                  (source:source-error message
                                       (source:make-note (source:make-location (cursor-source cursor)
                                                                               span)
                                                         (or note
                                                             (format nil "expected ~A" require)))))
                t)))

(defun collect-symbols (cursor &key message)
  "Return all remaining values in CURSOR as a list of symbols."
  (loop :until (empty-p cursor)
        :collect (next-symbol cursor :message message)))
