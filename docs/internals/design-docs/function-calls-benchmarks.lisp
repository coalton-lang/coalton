(trivial-benchmark:define-benchmark-package #:coalton-benchmarks
  (:use #:cl
        #:coalton-impl)
  (:export #:run-benchmarks))

(in-package #:coalton-benchmarks)

;; This file mirrors the current fixed-arity FUNCTION-ENTRY runtime shape used
;; in Coalton's design docs. It is intentionally small and benchmarks exact
;; fixed-arity application versus the generic interop path.

(defstruct function-entry
  (arity 0 :type fixnum :read-only t)
  (function nil :type function :read-only t))

#+sbcl
(declaim (sb-ext:freeze-type function-entry))

(defmacro define-function-macros ()
  (labels ((define-function-macros-with-arity (arity)
             (declare (type fixnum arity))
             (let ((constructor-sym (intern (format nil "F~D" arity)))
                   (application-sym (intern (format nil "A~D" arity)))
                   (function-sym (alexandria:make-gensym "F"))
                   (arg-syms (alexandria:make-gensym-list arity)))
               `(progn
                  (declaim (inline ,constructor-sym))
                  (defun ,constructor-sym (,function-sym)
                    (declare (type function ,function-sym)
                             (optimize (speed 3) (safety 0))
                             (values function-entry &optional))
                    (make-function-entry :arity ,arity
                                         :function ,function-sym))

                  (declaim (inline ,application-sym))
                  (defun ,application-sym (,function-sym ,@arg-syms)
                    (declare (optimize (speed 3) (safety 0))
                             (type (or function function-entry) ,function-sym))
                    (typecase ,function-sym
                      (function-entry
                       (if (= (function-entry-arity ,function-sym) ,arity)
                           (funcall (the function (function-entry-function ,function-sym))
                                    ,@arg-syms)
                           (error "Arity mismatch: expected ~D visible positional arguments." ,arity)))
                      (function
                       (funcall (the function ,function-sym) ,@arg-syms))))))))
    `(progn
       ,@(loop :for i :of-type fixnum :from 2 :below 10
               :collect (define-function-macros-with-arity i)))))

(define-function-macros)

(defun call-coalton-function (function-value &rest args)
  (declare (optimize (speed 3) (safety 0))
           (type (or function function-entry) function-value))
  (typecase function-value
    (function-entry
     (if (= (function-entry-arity function-value) (length args))
         (apply (the function (function-entry-function function-value)) args)
         (error "Arity mismatch: expected ~D visible positional arguments."
                (function-entry-arity function-value))))
    (function
     (apply (the function function-value) args))))

(defun unoptimized-add4 (a b c d)
  (+ a b c d))

(defun add4 (a b c d)
  (declare (type (signed-byte 32) a b c d)
           (optimize (speed 3) (safety 0))
           (values (signed-byte 32) &optional))
  (+ a b c d))

(defvar fadd4 (f4 #'add4))
(defvar fadd4-no-types (f4 #'unoptimized-add4))

(trivial-benchmark:define-benchmark benchmark-add4 ()
  (declare (optimize (speed 3) (safety 0)))
  (loop :for i :below 10000
        :for j :below 10000
        :for k :below 10000
        :for l :below 10000
        :do (trivial-benchmark:with-benchmark-sampling
              (dotimes (_ 10000)
                (add4 i _ k l)))))

(trivial-benchmark:define-benchmark benchmark-fadd4-exact ()
  (declare (optimize (speed 3) (safety 0)))
  (loop :for i :below 10000
        :for j :below 10000
        :for k :below 10000
        :for l :below 10000
        :do (trivial-benchmark:with-benchmark-sampling
              (dotimes (_ 10000)
                (a4 (the function-entry fadd4) i _ k l)))))

(trivial-benchmark:define-benchmark benchmark-fadd4-generic ()
  (declare (optimize (speed 3) (safety 0)))
  (loop :for i :below 10000
        :for j :below 10000
        :for k :below 10000
        :for l :below 10000
        :do (trivial-benchmark:with-benchmark-sampling
              (dotimes (_ 10000)
                (call-coalton-function (the function-entry fadd4) i _ k l)))))

(trivial-benchmark:define-benchmark benchmark-unoptimized-add4 ()
  (declare (optimize (speed 3) (safety 0)))
  (loop :for i :below 10000
        :for j :below 10000
        :for k :below 10000
        :for l :below 10000
        :do (trivial-benchmark:with-benchmark-sampling
              (dotimes (_ 10000)
                (unoptimized-add4 i _ k l)))))

(trivial-benchmark:define-benchmark benchmark-unoptimized-fadd4-generic ()
  (declare (optimize (speed 3) (safety 0)))
  (loop :for i :below 10000
        :for j :below 10000
        :for k :below 10000
        :for l :below 10000
        :do (trivial-benchmark:with-benchmark-sampling
              (dotimes (_ 10000)
                (call-coalton-function (the function-entry fadd4-no-types) i _ k l)))))

(trivial-benchmark:define-benchmark benchmark-noop ()
  (declare (optimize (speed 3) (safety 0)))
  (loop :for i :below 10000
        :for j :below 10000
        :for k :below 10000
        :for l :below 10000
        :do (trivial-benchmark:with-benchmark-sampling
              (dotimes (_ 10000) nil))))

(defun run-benchmarks ()
  (trivial-benchmark:run-package-benchmarks :package '#:coalton-benchmarks
                                            :verbose t))
