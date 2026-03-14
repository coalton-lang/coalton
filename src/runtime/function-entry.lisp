(defpackage #:coalton-impl/runtime/function-entry
  (:use
   #:cl)
  (:import-from
   #:coalton
   #:call-coalton-function)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:function-entry
   #:function-entry-arity
   #:function-entry-function
   #:function-entry-bound-arguments
   #:exact-call
   #:construct-function-entry
   #:bind-function-entry-hidden-arguments
   #:call-coalton-function
   #:coalton-function-arity-mismatch
   #:coalton-function-arity-mismatch-function
   #:coalton-function-arity-mismatch-expected-arity
   #:coalton-function-arity-mismatch-actual-arity
   #:coalton-function-arity-mismatch-arguments))

(in-package #:coalton-impl/runtime/function-entry)

;;; A FUNCTION-ENTRY represents a first-class Coalton function value.
;;; ARITY counts only the visible positional arguments. Hidden dictionary
;;; arguments may already be bound onto the entry, and Lisp keyword arguments
;;; are forwarded directly to the underlying function.
(defstruct function-entry
  ;; The exact visible positional arity expected by the Coalton value.
  (arity    (util:required 'arity)    :type fixnum        :read-only t)
  ;; The underlying Lisp function, including any hidden dictionary or keyword
  ;; parameters present in the compiled callable shape.
  (function (util:required 'function) :type function      :read-only t)
  ;; Hidden dictionary arguments pre-bound for first-class overloaded values.
  ;; These are applied before any visible positional or keyword arguments.
  (bound-arguments nil                 :type list          :read-only t))

#+sbcl
(declaim (sb-ext:freeze-type function-entry))

(defmethod print-object ((function-entry function-entry) stream)
  (print-unreadable-object (function-entry stream :type t)
    (format stream
            ":ARITY ~a~@[ :BOUND ~a~]"
            (function-entry-arity function-entry)
            (and (function-entry-bound-arguments function-entry)
                 (length (function-entry-bound-arguments function-entry))))))

(defun construct-function-entry (function arity)
  "Return code that constructs a FUNCTION-ENTRY for FUNCTION with visible ARITY.

Nullary Coalton functions are represented explicitly with ARITY 0."
  `(make-function-entry :arity ,arity :function ,function))

(defun bind-function-entry-hidden-arguments (function-entry &rest hidden-arguments)
  "Return FUNCTION-ENTRY with HIDDEN-ARGUMENTS pre-applied to future calls.

The returned entry exposes a reduced visible positional arity. This is used for
first-class overloaded values after their hidden dictionary arguments have been
resolved."
  (declare (type function-entry function-entry)
           (type list hidden-arguments)
           (values function-entry &optional))
  (if (null hidden-arguments)
      function-entry
      (let ((new-arity (- (function-entry-arity function-entry)
                          (length hidden-arguments))))
        (assert (>= new-arity 0) ()
                "bind-function-entry-hidden-arguments: ~D hidden arguments ~
                 exceeds visible arity ~D"
                (length hidden-arguments)
                (function-entry-arity function-entry))
        (make-function-entry
         :arity new-arity
         :function (function-entry-function function-entry)
         :bound-arguments (append (function-entry-bound-arguments function-entry)
                                  hidden-arguments)))))

(define-condition coalton-function-arity-mismatch (error)
  ((function :initarg :function
             :accessor coalton-function-arity-mismatch-function)
   (expected-arity :initarg :expected-arity
                   :accessor coalton-function-arity-mismatch-expected-arity)
   (actual-arity :initarg :actual-arity
                 :accessor coalton-function-arity-mismatch-actual-arity)
   (arguments :initarg :arguments
              :accessor coalton-function-arity-mismatch-arguments))
  (:report (lambda (err stream)
             (with-slots (function expected-arity actual-arity) err
               (format stream
                       "Attempt to apply ~s to ~d arguments, but it requires ~d."
                       function actual-arity expected-arity)))))

(defun positional-argument-count (args)
  (declare (type list args)
           (values fixnum &optional))
  (loop :for arg :in args
        :for count :from 0
        :when (keywordp arg)
          :return count
        :finally (return (length args))))

(defun %call-function-entry (function-entry args)
  (declare (type function-entry function-entry)
           (type list args)
           (values t &optional))
  (let ((expected-arity (function-entry-arity function-entry))
        (actual-arity (positional-argument-count args))
        (function (function-entry-function function-entry))
        (bound-arguments (function-entry-bound-arguments function-entry)))
    (if (= expected-arity actual-arity)
        (case (length bound-arguments)
          (0 (apply function args))
          (1 (apply function
                    (first bound-arguments)
                    args))
          (2 (apply function
                    (first bound-arguments)
                    (second bound-arguments)
                    args))
          (3 (apply function
                    (first bound-arguments)
                    (second bound-arguments)
                    (third bound-arguments)
                    args))
          (4 (apply function
                    (first bound-arguments)
                    (second bound-arguments)
                    (third bound-arguments)
                    (fourth bound-arguments)
                    args))
          (t (apply function
                    (append bound-arguments args))))
        (error 'coalton-function-arity-mismatch
               :function function-entry
               :expected-arity expected-arity
               :actual-arity actual-arity
               :arguments args))))

(defmacro exact-call (function-entry &rest args)
  "Apply FUNCTION-ENTRY to the positional ARGS without generic keyword dispatch.

This is used by compiler-generated code when the callee is known to be a
Coalton function value and the call site has no keyword arguments. Any hidden
arguments already bound onto FUNCTION-ENTRY are prepended before ARGS."
  (let ((function-entry-sym (gensym "FUNCTION-ENTRY"))
        (function-sym (gensym "FUNCTION"))
        (bound-arguments-sym (gensym "BOUND-ARGUMENTS"))
        (expected-arity-sym (gensym "EXPECTED-ARITY"))
        (arg-syms (loop :repeat (length args) :collect (gensym "ARG"))))
    `(let ((,function-entry-sym ,function-entry)
           ,@(loop :for arg-sym :in arg-syms
                   :for arg :in args
                   :collect `(,arg-sym ,arg)))
       (let ((,expected-arity-sym (function-entry-arity ,function-entry-sym)))
         (if (= ,expected-arity-sym ,(length args))
             (let ((,function-sym (function-entry-function ,function-entry-sym))
                   (,bound-arguments-sym
                     (function-entry-bound-arguments ,function-entry-sym)))
               (case (length ,bound-arguments-sym)
                 (0 (funcall ,function-sym ,@arg-syms))
                 (1 (funcall ,function-sym
                             (first ,bound-arguments-sym)
                             ,@arg-syms))
                 (2 (funcall ,function-sym
                             (first ,bound-arguments-sym)
                             (second ,bound-arguments-sym)
                             ,@arg-syms))
                 (3 (funcall ,function-sym
                             (first ,bound-arguments-sym)
                             (second ,bound-arguments-sym)
                             (third ,bound-arguments-sym)
                             ,@arg-syms))
                 (4 (funcall ,function-sym
                             (first ,bound-arguments-sym)
                             (second ,bound-arguments-sym)
                             (third ,bound-arguments-sym)
                             (fourth ,bound-arguments-sym)
                             ,@arg-syms))
                 (t (apply ,function-sym
                           (append ,bound-arguments-sym
                                   (list ,@arg-syms))))))
             (error 'coalton-function-arity-mismatch
                    :function ,function-entry-sym
                    :expected-arity ,expected-arity-sym
                    :actual-arity ,(length args)
                    :arguments (list ,@arg-syms)))))))

(declaim (ftype (function ((or function function-entry) &rest t)
                          (values t &optional))
                call-coalton-function))
(defun coalton:call-coalton-function (function &rest args)
  "Apply Coalton FUNCTION to ARGS from Common Lisp.

If FUNCTION is a FUNCTION-ENTRY, this checks the visible positional arity,
prepends any hidden bound arguments, and forwards Lisp keyword arguments
unchanged."
  (typecase function
    (function-entry
      (%call-function-entry function args))
    (function
      (apply function args))
    (t
      (apply function args))))

(define-compiler-macro coalton:call-coalton-function (function &rest args)
  (let ((function-sym (gensym "FUNCTION"))
        (arg-syms (loop :repeat (length args) :collect (gensym "ARG"))))
    `(let ((,function-sym ,function)
           ,@(loop :for arg-sym :in arg-syms
                   :for arg :in args
                   :collect `(,arg-sym ,arg)))
       (typecase ,function-sym
         (function-entry
           (%call-function-entry ,function-sym (list ,@arg-syms)))
         (function
           (apply ,function-sym (list ,@arg-syms)))
         (t
           (apply ,function-sym (list ,@arg-syms)))))))
