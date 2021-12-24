(in-package #:coalton-impl/typechecker)

;;;
;;; Kinds
;;;

(defstruct (kind (:constructor nil)))

(defstruct (kstar (:include kind)))

(defmethod make-load-form ((self kstar) &optional env)
  (make-load-form-saving-slots
   self
   :slot-names nil
   :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type kstar))

(alexandria:define-constant kstar (make-instance 'kstar) :test #'equalp)

(defstruct
    (kfun
     (:include kind)
     (:constructor kfun (from to)))
  (from (required 'from) :type kind :read-only t)
  (to   (required 'to)   :type kind :read-only t))

(defmethod make-load-form ((self kfun) &optional env)
  (make-load-form-saving-slots
   self
   :slot-names '(from to)
   :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type kfun))

#+(and sbcl coalton-release)
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
