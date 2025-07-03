;;;; optional.lisp
;;;;
;;;; This package contains the underlying implementation of the
;;;; Optional type for Coalton. The idea is to avoid unnecessary
;;;; boxing by defining a structure that represents None nested in N
;;;; Somes. Consider the general case of the type (Optional^M :t). In
;;;; this case, we have exactly M+1 possible cases to match
;;;; against. We have the cases (Some^N (None)) for N from 0 to M-1
;;;; and we have the case (Some^M x) where x is of type :t. Therefore,
;;;; by defining a struct CL-OPTIONAL with one slot, DEPTH, we can
;;;; always represent (Optional^M :t) as either an element of type :t
;;;; or as a CL-OPTIONAL with a depth between 0 and M-1.
;;;;
;;;; For example, (lisp (Optional (Optional :t)) () 5) is
;;;; unambiguously equal to (Some (Some 5)).

(defpackage #:coalton-impl/runtime/optional
  (:use
   #:cl)
  (:export
   #:cl-none
   #:cl-none-p
   #:cl-some
   #:cl-some-p
   #:unwrap-cl-some))

(in-package #:coalton-impl/runtime/optional)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline make-cl-optional))
  (defstruct (cl-optional (:constructor make-cl-optional (depth)))
    "A CL-OPTIONAL represents of DEPTH N represents the Coalton
value (Some^N None)."
    (depth 0 :type (unsigned-byte 16) :read-only t))

  ;; SBCL generates the load form by default, but CCL
  ;; does not, so we generate it here regardless.
  (defmethod make-load-form ((obj cl-optional) &optional env)
    (make-load-form-saving-slots obj :environment env)))

#+sbcl
(cl:declaim (sb-ext:freeze-type cl-optional))

(defmethod print-object ((opt cl-optional) stream)
  (let ((depth (cl-optional-depth opt)))
    (format stream "#.")
    (loop :repeat depth :do (format stream "(~S " 'coalton:Some))
    (format stream "~S" 'coalton:None)
    (loop :repeat depth :do (format stream ")"))))

(alexandria:define-constant +cl-optional-cache-size+ 8)

(declaim (type (simple-vector 8) cl-optional-cache))
(alexandria:define-constant cl-optional-cache
    (make-array +cl-optional-cache-size+
                :initial-contents
                (loop :for depth :below +cl-optional-cache-size+
                      :collect (make-cl-optional depth)))
  :test 'equalp)

(alexandria:define-constant cl-none (aref cl-optional-cache 0) :test 'equalp)

(declaim (inline cl-none-p))
(defun cl-none-p (x)
  ;; This would ideally be EQ instead of EQUALP,
  ;; however, we need to make sure the logic
  ;; compiles with the standard. For example,
  ;; if this were EQ, then
  ;; (CL-NONE-P (READ-FROM-STRING "#.NONE"))
  ;; would evaluate to false.
  (equalp x cl-none))

(declaim (inline cl-some))
(defun cl-some (x)
  (typecase x
    (cl-optional
     (let ((new-depth (1+ (cl-optional-depth x))))
       (if (< new-depth +cl-optional-cache-size+)
           (aref cl-optional-cache new-depth)
           (make-cl-optional new-depth))))
    (otherwise
     x)))

(declaim (inline cl-some-p))
(defun cl-some-p (x)
  (not (cl-none-p x)))

(declaim (inline unwrap-cl-some))
(defun unwrap-cl-some (x)
  (typecase x
    (cl-optional
     (let ((new-depth (1- (cl-optional-depth x))))
       (if (< new-depth +cl-optional-cache-size+)
           (aref cl-optional-cache new-depth)
           (make-cl-optional new-depth))))
    (otherwise
     x)))

