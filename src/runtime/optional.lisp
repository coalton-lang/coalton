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
   #:cl
   #:coalton-compatibility-layer)
  (:local-nicknames
   (#:compat #:coalton-compatibility-layer))
  (:export
   #:cl-none
   #:cl-none-p
   #:cl-some
   #:cl-some-p
   #:unwrap-cl-some))

(in-package #:coalton-impl/runtime/optional)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (cl-optional (:constructor %make-cl-optional (depth))
                          (:predicate nil)
                          (:copier nil)
                          (:conc-name nil))
    "A CL-OPTIONAL represents of DEPTH N represents the Coalton
value (Some^N None)."
    (depth 0 :type (unsigned-byte 16) :read-only t))

  (compat:try-freeze-type cl-optional)

  ;; Below is some pretty cursed code.
  ;;
  ;; The goal here is to cache some CL-OPTIONAL instances for small
  ;; depths. We want to make sure of the following:
  ;;
  ;;     - We can compare None, which is (%make-cl-optional 0), with
  ;;       CL:EQ.
  ;;
  ;;     - All of the constants should survive as objects in file
  ;;       compilation, which means MAKE-LOAD-FORM needs to work.
  ;;
  ;;     - It must be portable, at least to SBCL and CCL.
  ;;
  ;; There must be a better way to do the below; I'm just not sure
  ;; what it is.
  (defconstant cl-optional-cache-size 4)

  (unless (get 'cl-optional ':cache)
    (setf (get 'cl-optional ':cache)
          (loop :for i :below cl-optional-cache-size
                :collect (%make-cl-optional i))))

  (defmethod make-load-form ((obj cl-optional) &optional env)
    (declare (ignore env))
    (if (< (depth obj) cl-optional-cache-size)
        `(nth ,(depth obj) (get 'cl-optional ':cache))
        `(%make-cl-optional ,(depth obj))))

  ;; Note: This is a throwaway macro used just once for its
  ;; compile-time nature. We could probably do it just as well with
  ;; macrolet.
  (defmacro define-cached-constructor (name)
    (let ((cached-optionals (loop :for i :below cl-optional-cache-size
                                  :collect (intern (format nil "%NONE~D" i)))))
      `(progn
         ;; First, define global constants which refer to each cached
         ;; value. They will all be named %NONE<n>.
         (eval-when (:compile-toplevel :load-toplevel :execute)
           ,@(loop :for i :below cl-optional-cache-size
                   :for s :in cached-optionals
                   :collect `(defconstant ,s
                               (if (boundp ',s)
                                   (symbol-value ',s)
                                   (nth ,i (get 'cl-optional ':cache))))))

         ;; Second, define an actual constructor function that can
         ;; efficiently produce our cached results, including when the
         ;; argument is known.
         (declaim (ftype (function ((unsigned-byte 16)) cl-optional) ,name)
                  (inline make-cl-optional))
         (defun ,name (depth)
           (case depth
             ,@(loop :for i :below cl-optional-cache-size
                     :for s :in cached-optionals
                     :collect `((,i) ,s))
             (otherwise (%make-cl-optional depth))))))))

(define-cached-constructor make-cl-optional)

(define-symbol-macro cl-none %none0)

(defmethod print-object ((opt cl-optional) stream)
  (format stream "#.")
  (loop :with start := 'coalton:None
        :repeat (1+ (depth opt))
        :for obj := start :then `(coalton:Some ,obj)
        :finally (print-object obj stream)))

(declaim (inline cl-none-p))
(defun cl-none-p (x)
  (eq x cl-none))

(declaim (inline cl-some))
(defun cl-some (x)
  (declare (optimize speed (safety 0) (debug 0)))
  (typecase x
    (cl-optional
     (make-cl-optional (1+ (depth x))))
    (otherwise
     x)))

(declaim (inline cl-some-p))
(defun cl-some-p (x)
  (not (cl-none-p x)))

(declaim (inline unwrap-cl-some))
(defun unwrap-cl-some (x)
  (declare (optimize speed (safety 0) (debug 0)))
  (typecase x
    (cl-optional
     ;; The Coalton type system should guarantee this never goes below
     ;; 0. We have seen bugs before, though, where CL-NONE-P fails
     ;; when the argument *is* CL-NONE, causing the downstream effect
     ;; of decrementing this below zero. (These bugs arose from the
     ;; EQ-ness of CL-NONE.)
     (make-cl-optional (1- (depth x))))
    (otherwise
     x)))
