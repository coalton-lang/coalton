;;;; Utilities for incremental consumption of concrete syntax tree
;;;; values.  A cursor maintains a reference to a value, and optionally
;;;; to a position within it, if it is cons-valued. The functions
;;;; 'next' and 'empty-p' are sufficient to build specialized parsing
;;;; functions that don't need knowledge of the cst package.
;;;;
;;;; Example of a parser for expressions like "(sym* -> sym*)"
;;;;
;;;;   (let* ((cursor (cursor:make-cursor form source "invalid"))
;;;;          (left (cursor:collect cursor
;;;;                  :test (lambda (sym) (not (eq sym '->))))))
;;;;     (cursor:discard-symbol cursor)       ; discard separator
;;;;     (let ((right (cursor:collect cursor)))
;;;;       (cons left right)))

(defpackage #:coalton-impl/parser/cursor
  (:use
   #:cl)
  (:shadow
   #:error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util))
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:export
   #:atom-p
   #:collect
   #:collect-symbols
   #:cursor-location
   #:cursor-message
   #:cursor-pointer
   #:cursor-source
   #:cursor-value
   #:discard-symbol
   #:each
   #:empty-p
   #:error
   #:make-cursor
   #:next
   #:next-symbol
   #:peek))

(in-package #:coalton-impl/parser/cursor)

;;; The value field can be any type returned by cst, generally a cons
;;; or atom.  When value is a cst:cons, pointer initially points to
;;; that value, and is destructively updated as values are 'popped'.

(defstruct (cursor (:constructor %make-cursor))
  "A CST node-valued cursor."
  (value   (util:required 'value)   :type cst:cst) ; current value
  (pointer (util:required 'value)   :type cst:cst) ; pointer into current value
  (last    nil            :type (or null cst:cst)) ; pointer to most recently consumed value
  (source  (util:required 'source))                ; the source:location of cursor
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

(defun peek (cursor &key (unwrap t))
  "Peek at the value of CURSOR without changing any state."
  (declare (type cursor cursor))
  (unless (empty-p cursor)
    (let ((value (cst:first (cursor-pointer cursor))))
      (when unwrap
        (setf value (cst:raw value)))
      value)))

(defun atom-p (cursor)
  "Return T if cursor is pointing at an atom."
  (cst:atom (cursor-pointer cursor)))

(defun proper-list-p (cursor)
  "Return T if cursor is pointing at a proper list."
  (cst:proper-list-p (cursor-pointer cursor)))

(defun empty-p (cursor)
  "T if CURSOR has no next value."
  (declare (type cursor cursor))
  (let ((pointer (cursor-pointer cursor)))
    (or (not (cst:consp pointer))
        (null (cst:first pointer)))))

(defun cursor-location (cursor)
  "Return the location of the value that CURSOR is pointing at."
  (source:make-location (cursor-source cursor)
                        (cond ((cursor-last cursor)
                               (cst:source (cursor-last cursor)))
                              (t
                               (let ((s (cst:source (cursor-value cursor))))
                                 (cons (1+ (car s))
                                       (1+ (car s))))))))

(defun %label-location (cursor position)
  "Look up the CURSOR-relative error message location for a symbolic POSITION."
  (ecase position
    (:last
     (cursor-location cursor))
    (:next
     (source:make-location (cursor-source cursor)
                           (cst:source (cst:first (cursor-pointer cursor)))))
    (:form
     (source:make-location (cursor-source cursor)
                           (cst:source (cursor-value cursor))))
    (:after-last
     (source:end-location (cursor-location cursor)))))


(defun error (cursor position note)
  "Signal a PARSE-ERROR with a NOTE that labels the current value of a cursor.

Position is one of:

:last -- label the last element
:next -- label the next element
:form -- label the enclosing form
:after-last -- point just past the last element"
  (parse-error (cursor-message cursor)
               (source:note (%label-location cursor position) note)))

(defun next (cursor &key (pred nil) (unwrap t))
  "Return the next value from a nonempty cursor.

If PRED is non-NIL, only consume a value if it is true.
If UNWRAP is NIL, return the CST node, otherwise, return the raw value."
  (declare (type cursor cursor))
  (when (empty-p cursor)
    ;; Finding empty-p = t here this would indicate that the compiler
    ;; writer hasn't checked for emptiness in the calling context in
    ;; order to construct a more specific error message.
    (error cursor ':after-last "attempt to read past end of list"))
  (let ((value (cst:first (cursor-pointer cursor))))
    (when (or (null pred)
              (funcall pred (cst:raw value)))
      (setf (cursor-pointer cursor)
            (cst:rest (cursor-pointer cursor))
            (cursor-last cursor)
            value)
      (if unwrap (cst:raw value) value))))

(defun %ensure-proper-list (cursor)
  "Enforce that CURSOR points at a proper list before iterating. Empty lists are allowed."
  (unless (null (cst:raw (cursor-pointer cursor)))
    (when (atom-p cursor)
      (error cursor ':form "expected a list"))
    (unless (proper-list-p cursor)
      (error cursor ':form "unexpected dotted list"))))

(defun each (cursor f)
  "For each element in cons-valued CURSOR, create a subcursor and apply F."
  (%ensure-proper-list cursor)
  (loop :until (empty-p cursor)
        :do (let ((value (next cursor :unwrap nil)))
              (funcall f (%make-cursor :value value
                                       :pointer value
                                       :source (cursor-source cursor)
                                       :message (cursor-message cursor))))))

(defun collect (cursor &key test (key #'identity))
  "Collect values from list-valued CURSOR until empty or TEST returns NIL, optionally transforming each value with KEY."
  (%ensure-proper-list cursor)
  (loop :until (or (empty-p cursor)
                   (and test (not (funcall test (peek cursor)))))
        :collect (funcall key (next cursor :unwrap nil))))

;;; Utilities for reading symbols

(defun next-symbol (cursor missing not-symbol)
  "Return the next symbol in cursor.
If the cursor is empty, signal an error with MISSING as the message.
If the next element is not a symbol, signal an error with NOT-SYMBOL as the message."
  (when (empty-p cursor)
    (error cursor ':after-last missing))
  (let ((name (next cursor)))
    (unless (symbolp name)
      (error cursor ':last not-symbol))
    name))

(defun discard-symbol (cursor &optional symbol message)
  "Read and discard a symbol from CURSOR.
Signal a condition with MESSAGE if a symbol is not present."
  (let ((s (next-symbol cursor (or message (format nil "expected ~A" symbol))
                        (or message "must be a symbol"))))
    (when (and symbol (not (string-equal s symbol)))
      (error cursor ':last (or message (format nil "expected ~A" symbol))))
    s))

(defun collect-symbols (cursor)
  "Read a list of symbols from CURSOR."
  (collect cursor
    :test (lambda (value)
            (or (and value (symbolp value))
                (error cursor ':next "expected symbol")))
    :key #'cst:raw))
