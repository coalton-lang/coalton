(trivial-benchmark:define-benchmark-package #:coalton-benchmarks
  (:use #:cl
        #:coalton-impl)
  (:export #:run-benchmarks))

(in-package #:coalton-benchmarks)

(defstruct function-entry
  (arity 0 :type fixnum :read-only t)
  (function nil :type function :read-only t)
  (curried nil :type function :read-only t))
(declaim (sb-ext:freeze-type function-entry))


(defmacro define-function-macros ()
  (labels ((define-function-macros-with-arity (arity)
             (declare (type fixnum arity))
             (let ((constructor-sym (intern (format nil "F~D" arity)))
                   (application-sym (intern (format nil "A~D" arity)))
                   (function-sym (alexandria:make-gensym "F"))
                   (arg-syms (alexandria:make-gensym-list arity)))

               (labels ((build-curried-function (args)
                          (if (null (car args))
                              `(funcall ,function-sym ,@arg-syms)
                              `(lambda (,(car args)) ,(build-curried-function (cdr args)))))
                        (build-curried-function-type (arity)
                          (if (= 0 arity)
                              't
                              `(function (t) ,(build-curried-function-type (1- arity)))))
                        (build-curried-function-call (fun args)
                          (if (null (car args))
                              `(the ,(build-curried-function-type arity) ,fun)
                              `(funcall ,(build-curried-function-call fun (cdr args)) ,(car args)))))

                 ;; NOTE: Since we are only calling these functions
                 ;; from already typechecked coalton, we can use the
                 ;; OPTIMIZE flags to remove type checks.
                 `(progn
                    (declaim (inline ,constructor-sym))
                    (defun ,constructor-sym (,function-sym)
                      (declare (type (function ,(loop :for i :below arity :collect 't) t) ,function-sym)
                               (optimize (speed 3) (safety 0))
                               (values function-entry))
                      (make-function-entry :arity ,arity
                                           :function ,function-sym
                                           :curried ,(build-curried-function arg-syms)))

                    (declaim (inline ,application-sym))
                    (defun ,application-sym (,function-sym ,@arg-syms)
                      (declare (optimize (speed 3) (safety 0))
                               (type (or function function-entry) ,function-sym))
                      (typecase ,function-sym
                        (function-entry
                         (if (= (function-entry-arity ,function-sym) ,arity)
                             (funcall (the (function ,(loop :for i :below arity :collect 't) t)
                                           (function-entry-function ,function-sym))
                                      ,@arg-syms)
                             ,(build-curried-function-call `(function-entry-curried ,function-sym) (reverse arg-syms))))
                        (function
                         ,(build-curried-function-call function-sym arg-syms)))))))))
    `(progn
       ,@(loop :for i :of-type fixnum :from 2 :below 10
               :collect (define-function-macros-with-arity i)))))
(define-function-macros)


(defun add3 (a b c)
  (declare (optimize (speed 3) (safety 0))
           (type (signed-byte 32) a b c)
           (values (signed-byte 32)))
  (+ a b c))
(defvar fadd3 (f3 #'add3))

(defun unoptimized-add4 (a b c d)
  (+ a b c d))

(defun add4 (a b c d)
  (declare (type (signed-byte 32) a b c d)
           (values (signed-byte 32)))
  (+ a b c d))
(defun add4-curried (a)
  (declare (type (signed-byte 32) a)
           (optimize (speed 3) (safety 0)))
  (lambda (b)
    (declare (type (signed-byte 32) b)
             (optimize (speed 3) (safety 0)))
    (lambda (c)
      (declare (type (signed-byte 32) c)
               (optimize (speed 3) (safety 0)))
      (lambda (d)
        (declare (type (signed-byte 32) d)
                 (optimize (speed 3) (safety 0)))
        (+ a b c d)))))
(defvar fadd4 (f4 #'add4))
(defvar fadd4-no-types (f4 #'unoptimized-add4))

(defun run-add4 (a b c d)
  (declare (optimize (speed 3)))
  (add4 a b c d))

(defun run-add4-a4 (a b c d)
  (declare (optimize (speed 3)))
  (a4 #'add4-curried a b c d))

(defun run-fadd4 (a b c d)
  (declare (optimize (speed 3) (safety 0)))
  (a4 (the (or function function-entry) fadd4) a b c d))



(trivial-benchmark:define-benchmark benchmark-add4 ()
  (declare (optimize (speed 3) (safety 0)))
  (loop :for i :below 10000
        :for j :below 10000
        :for k :below 10000
        :for l :below 10000
        :do (trivial-benchmark:with-benchmark-sampling
              (dotimes (_ 10000)
                (add4 i _ k l)))))

(trivial-benchmark:define-benchmark benchmark-fadd4 ()
  (declare (optimize (speed 3) (safety 0)))
  (loop :for i :below 10000
        :for j :below 10000
        :for k :below 10000
        :for l :below 10000
        :do (trivial-benchmark:with-benchmark-sampling
              (dotimes (_ 10000)
                (a4 (the (or function-entry function) fadd4) i _ k l)))))

(trivial-benchmark:define-benchmark benchmark-add4-a4 ()
  (declare (optimize (speed 3) (safety 0)))
  (loop :for i :below 10000
        :for j :below 10000
        :for k :below 10000
        :for l :below 10000
        :do (trivial-benchmark:with-benchmark-sampling
              (dotimes (_ 10000)
                (a4 #'add4-curried i _ k l)))))

(trivial-benchmark:define-benchmark benchmark-unoptimized-add4 ()
  (declare (optimize (speed 3) (safety 0)))
  (loop :for i :below 10000
        :for j :below 10000
        :for k :below 10000
        :for l :below 10000
        :do (trivial-benchmark:with-benchmark-sampling
              (dotimes (_ 10000)
                (unoptimized-add4 i _ k l)))))

(trivial-benchmark:define-benchmark benchmark-unoptimized-fadd4 ()
  (declare (optimize (speed 3) (safety 0)))
  (loop :for i :below 10000
        :for j :below 10000
        :for k :below 10000
        :for l :below 10000
        :do (trivial-benchmark:with-benchmark-sampling
              (dotimes (_ 10000)
                (a4 (the function-entry fadd4-no-types) i _ k l)))))

(trivial-benchmark:define-benchmark benchmark-fadd4-a2-a2 ()
  (declare (optimize (speed 3) (safety 0)))
  (loop :for i :below 10000
        :for j :below 10000
        :for k :below 10000
        :for l :below 10000
        :do (trivial-benchmark:with-benchmark-sampling
              (dotimes (_ 10000)
                (a2 (a2 (the function-entry fadd4) i _) k l)))))

(trivial-benchmark:define-benchmark benchmark-noop ()
  (declare (optimize (speed 3) (safety 0)))
  (loop :for i :below 10000
        :for j :below 10000
        :for k :below 10000
        :for l :below 10000
        :do (trivial-benchmark:with-benchmark-sampling
              (dotimes (_ 10000) nil))))
