(defpackage #:coalton-impl/environment
  (:use
   #:cl)
  (:shadow
   #:get
   #:set
   #:map)
  (:local-nicknames
   (#:a #:alexandria))
  (:export
   #:do-environment
   #:empty
   #:entries-by-package
   #:entries-for-package
   #:environment
   #:exported-symbol-p
   #:get
   #:keys
   #:namespace
   #:operation
   #:parent
   #:set
   #:set*
   #:update-entries
   #:unset))

;;;
;;; Low-level environment, representing all that Coalton knows about
;;; types and classes. Definitions are stored in a two-level map,
;;; where the first level is keyed by major namespace (value, type,
;;; class, instance, specialization, and so on) and the second by
;;; symbol, to instances of per-namespace class.
;;;
;;; The global map is immutable, and versions are maintained across
;;; environment records linked by a 'parent' slot.
;;;
;;; A global environment is constructed and maintained by propagating
;;; an initial environment through a compilation unit, and reassigning
;;; the global environment to the returned environment on success.
;;;
;;; The file typechecker/environment.lisp provides higher level
;;; per-namespace/type set of functions for modifying the environment.
;;;

(in-package #:coalton-impl/environment)

(defstruct (environment (:conc-name nil))
  (namespace nil :type symbol)
  parent
  operation
  value)                                ; namespace->k->v

(defmethod print-object ((environment environment) stream)
  (print-unreadable-object (environment stream :type t :identity t)))

(defun empty ()
  "Return an empty environment."
  (make-environment :value (fset:empty-map)))

(declaim (inline get-namespace))
(defun get-namespace (environment namespace)
  (or (fset:lookup (value environment) namespace)
      (fset:empty-map)))

(declaim (inline update-namespace))
(defun update-namespace (environment namespace f)
  "Update map-valued NAMESPACE in ENVIRONMENT using function F."
  (let ((v (value environment)))
    (fset:with v namespace (funcall f (or (fset:lookup v namespace)
                                          (fset:empty-map))))))

(defun get (environment namespace name)
  "Return the entry bound by NAME in NAMESPACE of ENVIRONMENT."
  (fset:lookup (get-namespace environment namespace) name))

(declaim (inline set))
(defun set (environment namespace name entry)
  "Bind NAME to ENTRY in NAMESPACE of ENVIRONMENT."
  (make-environment :namespace namespace
                    :parent environment
                    :operation (list :set name entry)
                    :value (update-namespace environment namespace
                                             (a:rcurry #'fset:with name entry))))

(declaim (inline unset))
(defun unset (environment namespace name)
  "Unbind NAME in NAMESPACE of ENVIRONMENT."
  (make-environment :namespace namespace
                    :parent environment
                    :operation (list :unset name)
                    :value (update-namespace environment namespace
                                             (a:rcurry #'fset:less name))))

(defun set* (environment namespace &rest entries)
  "Create multiple NAME -> ENTRY bindings in NAMESPACE of ENVIRONMENT."
  (loop :for (name entry) :on entries :by #'cddr
        :do (setf environment (set environment namespace name entry)))
  environment)

(defun keys (environment namespace)
  "Return the names in NAMESPACE of ENVIRONMENT."
  (fset:convert 'list (fset:domain (get-namespace environment namespace))))

(defun map (environment namespace f)
  "Apply 2-ary function F to each name and entry in ENVIRONMENT."
  (dolist (k (keys environment namespace))
    (funcall f k (get environment namespace k))))

(defmacro do-environment ((name entry environment namespace) &body body)
  "For each entry in ENVIRONMENT, bind NAME and ENTRY for the scope of BODY."
  `(map ,environment ,namespace (lambda (,name ,entry) ,@body)))

(defun update-entries (environment namespace f)
  "Rewrite NAMESPACE by applying function F to each entry."
  (reduce (lambda (environment k)
            (set environment namespace k (funcall f (get environment namespace k))))
          (keys environment namespace) :initial-value environment))

(defun exported-symbol-p (symbol package)
  "Test that SYMBOL is exported by PACKAGE."
  (declare (type symbol symbol))
  (and (equalp (symbol-package symbol) package)
       (eql :external (nth-value 1 (find-symbol (symbol-name symbol) package)))))

(defun entries-for-package (environment namespace package
                            &key (public nil)
                                 (entry-resolver #'identity))
  "Return the entries in NAMESPACE of ENVIRONMENT matching PACKAGE."
  (let ((result nil))
    (do-environment (sym entry environment namespace)
      (when (if public
                (exported-symbol-p sym package)
                (eql package (symbol-package sym)))
        (push (cons sym (funcall entry-resolver entry)) result)))
    result))

(defun entries-by-package (environment namespace
                           &key (public nil)
                                (entry-resolver #'identity))
  "Group the entries in NAMESPACE of ENVIRONMENT by package."
  (let ((table (make-hash-table)))
    (do-environment (sym entry environment namespace)
      (let ((package (symbol-package sym)))
        (when (or (not public)
                  (exported-symbol-p sym package))
          (push (cons sym (funcall entry-resolver entry))
                (gethash package table)))))
    table))
