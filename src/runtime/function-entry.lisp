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
   #:function-entry-curried
   #:construct-function-entry
   #:call-coalton-function
   #:too-many-arguments-to-coalton-function
   #:too-many-arguments-function
   #:too-many-arguments-count
   #:too-many-arguments-arguments))

(in-package #:coalton-impl/runtime/function-entry)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant function-arity-limit 32))

;;; A FUNCTION-ENTRY represents a partially applicable function.
(defstruct function-entry
  ;; The arity of the partially applicable function.
  (arity    (util:required 'arity)    :type fixnum        :read-only t)
  ;; A reference to the full arity Lisp function. We put this in its
  ;; own slot to avoid an extra indirection.
  (function (util:required 'function) :type function      :read-only t)
  ;; A jump table, where the index of each entry determines the number
  ;; of arguments to partially apply.
  (curried  (util:required 'curried)  :type simple-vector :read-only t))

#+sbcl
(declaim (sb-ext:freeze-type function-entry))

(defmethod print-object ((function-entry function-entry) stream)
  (print-unreadable-object (function-entry stream :type t)
    (format stream
            ":ARITY ~a"
            (function-entry-arity function-entry))))

(defvar *function-constructor-functions* (make-array function-arity-limit))
(defvar *function-application-functions* (make-array function-arity-limit))

(defun unreachable-fun ()
  (error "This should be unreachable."))

(defmacro define-function-macros ()
  (labels ((sub-application-sym (arity)
             (alexandria:format-symbol t "A~D" arity))
           (constructor-sym (arity)
             (alexandria:format-symbol t "F~D" arity))
           (define-function-macros-with-arity (arity)
             (declare (type fixnum arity))
             (let ((constructor-sym (constructor-sym arity))
                   (application-sym (alexandria:format-symbol t "A~D" arity))
                   (function-sym (alexandria:make-gensym "F"))
                   (applied-function-sym (alexandria:make-gensym "F"))
                   (arg-syms (alexandria:make-gensym-list arity)))

               (flet ((build-curried-function (applied-arity args)
                        (let ((fun (gensym "FUN")))
                          (cond ((> applied-arity (length args))
                                 ;; For oversaturated functions, we saturate repeatedly.
                                 (let ((temps (alexandria:make-gensym-list applied-arity)))
                                   `(lambda (,fun ,@temps)
                                      (declare (type function ,fun))
                                      (,(sub-application-sym (- applied-arity arity))
                                       (funcall ,fun ,@(subseq temps 0 arity))
                                       ,@(subseq temps arity)))))
                                ;; This is special case is handled in
                                ;; the application logic.
                                ((= applied-arity (length args))
                                 `#'unreachable-fun)
                                (t
                                 `(lambda (,fun ,@(subseq args 0 applied-arity))
                                    (declare (type function ,fun))
                                    (,(constructor-sym (- arity applied-arity))
                                     (lambda ,(subseq args applied-arity)
                                       (funcall ,fun ,@args)))))))))

                 ;; NOTE: Since we are only calling these functions
                 ;; from already typechecked coalton, we can use the
                 ;; OPTIMIZE flags to remove type checks.
                 `(progn
                    (defun ,application-sym (,applied-function-sym ,@arg-syms)
                      (declare #.coalton-impl/settings:*coalton-optimize*
                               (type function-entry ,applied-function-sym)
                               (values t))
                      (let ((function-arity (function-entry-arity ,applied-function-sym))
                            (raw-function (function-entry-function ,applied-function-sym)))
                        (declare (type (mod ,function-arity-limit) function-arity))
                        ;; Fully saturated is the most common case,
                        ;; so we special case it.
                        (if (= ,arity function-arity)
                            (funcall raw-function ,@arg-syms)
                            ;; Partially apply or overapply some
                            ;; of the arguments by looking up
                            ;; what to do in the jump table.
                            (funcall
                             (the function
                                  (aref (function-entry-curried ,applied-function-sym) ,arity))
                             raw-function
                             ,@arg-syms))))
                    (setf (aref *function-application-functions* ,arity) ',application-sym)

                    (defun ,constructor-sym (,function-sym)
                      (declare (type (function ,(loop :for i :below arity :collect 't) t) ,function-sym)
                               #.coalton-impl/settings:*coalton-optimize*
                               (values function-entry))
                      (make-function-entry
                       :arity ,arity
                       :function ,function-sym
                       :curried
                       (load-time-value
                        (vector
                         #'unreachable-fun
                         ,@(loop for applied-arity from 1 below function-arity-limit
                                 collect (build-curried-function applied-arity arg-syms)))
                        t)))
                    (setf (aref *function-constructor-functions* ,arity) ',constructor-sym))))))
    (let ((body nil)
          (funs nil))
      (loop :for i :of-type fixnum :from 1 :below function-arity-limit
            :do (push (define-function-macros-with-arity i) body)
            :do (setf funs
                      (append (list (alexandria:format-symbol *package* "F~D" i)
                                    (alexandria:format-symbol *package* "A~D" i))
                              funs)))
      `(progn
         #+sbcl
         (declaim (sb-ext:start-block ,@funs))
         ,@(reverse body)
         #+sbcl
         (declaim (sb-ext:end-block))))))

(define-function-macros)


;;
;; Functions for building and applying function entries
;;

(defun construct-function-entry (function arity)
  "Construct a FUNCTION-ENTRY object with ARITY

NOTE: There is no FUNCTION-ENTRY for arity 1 and the function will be returned"
       (let ((function-constructor (aref *function-constructor-functions* arity)))
         (unless function-constructor
           (error "Unable to construct function of arity ~A" arity))
         `(,function-constructor ,function)))

(define-condition too-many-arguments-to-coalton-function (error)
  ((function :initarg :function
             :accessor too-many-arguments-function)
   (arg-count :initarg :arg-count
              :accessor too-many-arguments-count)
   (arguments :initarg :arguments
              :accessor too-many-arguments-arguments))
  (:report (lambda (err stream)
             (with-slots (function arg-count) err
               (format stream
                       "Attempt to apply ~s to ~d arguments, but ~s is ~d."
                       function arg-count 'function-arity-limit function-arity-limit)))))

(declaim (ftype (function ((or function function-entry) &rest t)
                          (values t &optional))
                call-coalton-function))
(defun coalton:call-coalton-function (function &rest args)
  "Apply a Coalton function object FUNCTION to ARGS from Common Lisp."
  (let ((arg-count (length args)))
    (if (>= arg-count function-arity-limit)
        (error 'too-many-arguments-to-coalton-function
               :function function
               :arg-count arg-count
               :arguments args)
        (apply (aref *function-application-functions* arg-count)
               function
               args))))

(define-compiler-macro coalton:call-coalton-function (function &rest args)
  (let ((arg-count (length args)))
    (if (>= arg-count function-arity-limit)
        (error 'too-many-arguments-to-coalton-function
               :function function
               :arg-count arg-count
               :arguments args)
        `(,(aref *function-application-functions* arg-count)
          ,function
          ,@args))))
