;;; This package provides utilities for incremental consumption of
;;; concrete syntax tree values.  A cursor struct maintains a
;;; reference to the current value and a pointer within it, if it is
;;; cons-valued. The functions 'next' and 'empty-p' are sufficient to
;;; build more specialized parsing functions that don't need knowledge
;;; of the cst package.

(defpackage #:coalton-impl/parser/cursor
  (:use
   #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util))
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:export
   #:collect-symbols
   #:cursor-location
   #:cursor-message
   #:cursor-source
   #:cursor-value
   #:do-every
   #:empty-p
   #:make-cursor
   #:next
   #:next-symbol
   #:parse-error
   #:peek
   #:syntax-error))

(in-package #:coalton-impl/parser/cursor)

;; The value field can be any type returned by cst, generally a cons
;; or atom.  When value is a cst:cons, pointer initially points to
;; that value, and is destructively updated as values are 'popped'.

(defstruct (cursor (:constructor %make-cursor))
  "A CST node-valued cursor."
  (value   (util:required 'value)   :type cst:cst) ; current value
  (pointer (util:required 'value)   :type cst:cst) ; pointer into current value
  (source  (util:required 'source)               ) ; the source:location of cursor
  (message (util:required 'message) :type string)) ; a message providing context for errors

;;; The implementation of source:location for a cursor returns the
;;; entire span of the cursor.

(defmethod source:location ((self cursor))
  (source:make-location (cursor-source self)
                        (cst:source (cursor-value self))))

(defun make-cursor (value source message)
  "Make a cursor that points at VALUE."
  (%make-cursor :value value
                :pointer value
                :source source
                :message message))

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

(defun cursor-span (cursor)
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

(defun cursor-location (cursor)
  "Return the location of the value that CURSOR is pointing at."
  (source:make-location (cursor-source cursor)
                        (cursor-span cursor)))

(defun syntax-error (cursor note &key (end nil))
  "Signal a PARSE-ERROR related to the current value of a cursor.
If END is T, indicate the location directly following the current value."
  (let ((location (if end
                      (source:end-location (cursor-location cursor))
                      (cursor-location cursor))))
    (parse-error (cursor-message cursor)
                 (source:note location note))))

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
        :do (funcall fn (make-cursor (next cursor :unwrap nil)
                                     (cursor-source cursor)
                                     (cursor-message cursor)))))

(defun next-symbol (cursor &key message missing require)
  "Return the next value in CURSOR as a symbol. The cursor must be nonempty, and the next value must be a symbol."
  (when (empty-p cursor)
    ;; When empty, indicate the character immediately preceding the
    ;; end of the cursor's outside span.
    (let ((end (1- (source:span-end (source:location-span (source:location cursor))))))
      (parse-error (cursor-message cursor)
                   (source:note (source:make-location (cursor-source cursor)
                                                      (cons end end))
                                (or missing "symbol is missing")))))
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
