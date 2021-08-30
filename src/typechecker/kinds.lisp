(in-package #:coalton-impl/typechecker)

;;;
;;; Kinds
;;;

(serapeum:defstruct-read-only (kind (:constructor nil)))

(serapeum:defstruct-read-only (kstar (:include kind)))

#+sbcl
(declaim (sb-ext:freeze-type kstar))

(alexandria:define-constant kstar (make-instance 'kstar) :test #'equalp)

(serapeum:defstruct-read-only
    (kfun
     (:include kind)
     (:constructor kfun (from to)))
  (from :type kind)
  (to   :type kind))

#+sbcl
(declaim (sb-ext:freeze-type kfun))

#+sbcl
(declaim (sb-ext:freeze-type kind))

(defun simple-kind-p (kind)
  "Whether KIND is a simple kind (either * or a function from many * to *)"
  (declare (type kind kind)
           (values boolean &optional))
  (etypecase kind
    (kstar t)
    (kfun (and (kstar-p (kfun-from kind))
               (simple-kind-p (kfun-to kind))))))

(defun kind-arity (kind)
  "The arity of the simple kind KIND (number of type arguments)"
  (declare (type kind kind)
           (values fixnum))
  (loop :while (kfun-p kind)
        :sum 1
        :do (setf kind (kfun-to kind))))

(defun make-kind-of-arity (arity)
  "Make a simple kind of arity ARITY"
  (declare (type fixnum arity)
           (values kind))
  (assert (<= 0 arity))
  (let ((kind kStar))
    (loop :for i :below arity
          :do (setf kind (kFun kStar kind)))
    kind))

;;;
;;; Pretty printing
;;;

(defun pprint-kind (stream kind &optional colon-p at-sign-p)
  (declare (type stream stream)
	   (type kind kind)
	   (ignore colon-p)
           (ignore at-sign-p)
	   (values kind))
  (etypecase kind
    (kstar
     (format stream "*"))
    (kfun
     (let ((from (kfun-from kind))
           (to (kfun-to kind)))
       (when (kfun-p from)
         (format stream "("))
       (pprint-kind stream from)
       (when (kfun-p from)
         (format stream ")"))

       (format stream " -> ")

       (when (kfun-p to)
         (format stream "("))
       (pprint-kind stream to)
       (when (kfun-p to)
         (format stream ")")))))
  kind)

(set-pprint-dispatch 'kind 'pprint-kind)
