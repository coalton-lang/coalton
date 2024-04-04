(defpackage #:coalton-impl/debug
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:algo #:coalton-impl/algorithm)
   (#:entry #:coalton-impl/entry)
   (#:env #:coalton-impl/environment)
   (#:settings #:coalton-impl/settings)
   (#:tc #:coalton-impl/typechecker)))

;;; Utilities for dumping the contents of Coalton's global environment
;;;
;;; Covers values, types, classes, instances and specializations.

(in-package #:coalton-impl/debug)

(defun print-package (stream entry-printer package entries)
  (format stream "[package ~A]~%~%" (package-name package))
  ;; Remove qualifications from package symbols
  (let ((*package* package))
    (dolist (entry entries)
      (funcall entry-printer stream entry)
      (format stream "~%"))
    (format stream "~%")))

(defun print-one-package (stream env entry-printer ns entry-resolver package)
  (let ((p (find-package package)))
    (unless p
      (error "Invalid package ~A" package))
    (print-package stream entry-printer p
                   (env:entries-for-package env ns p
                                            :entry-resolver entry-resolver))))

(defun print-all-packages (stream env entry-printer ns entry-resolver)
  (maphash (lambda (package entries)
             (print-package stream entry-printer package entries))
           (env:entries-by-package env ns :entry-resolver entry-resolver)))

(defun print-db (printer ns &key package (entry-resolver #'identity)) 
  "Print namespace NS of the global environment"
  (let ((env entry:*global-environment*)
        (stream t))
    (if package
        (print-one-package stream env printer ns entry-resolver package)
        (print-all-packages stream env printer ns entry-resolver))))

;; Coalton environment-specific structure printers

(defun print-class (stream entry)
  (destructuring-bind (name . entry) entry
    (declare (ignore name))
    (tc:with-pprint-variable-context ()
      (let ((class-pred (tc:ty-class-predicate entry)))
        (format stream "  [~S (~A :: ~A)]~%"
                (tc:ty-predicate-class class-pred)
                (tc:ty-predicate-types class-pred)
                (mapcar #'tc:kind-of (tc:ty-predicate-types class-pred))))
      (loop :for (method-name . method-type) :in (tc:ty-class-unqualified-methods entry) :do
        (format stream "    ~S :: ~A~%" method-name method-type)))))

(defun print-instance (stream entry)
  (destructuring-bind (entry . instances) entry
    (unless (null instances)
      ;; Generate substitutions for class
      (tc:with-pprint-variable-context ()
        (let ((class-pred (tc:ty-class-predicate entry)))
          (format stream "  [~S (~A :: ~A)]~%"
                  (tc:ty-predicate-class class-pred)
                  (tc:ty-predicate-types class-pred)
                  (mapcar #'tc:kind-of (tc:ty-predicate-types class-pred)))))
      (dolist (instance instances)
        (format stream "    ")
        ;; Generate type variable substitutions from instance
        ;; constraints
        (tc:with-pprint-variable-context ()
          (let* ((instance-constraints (tc:ty-class-instance-constraints instance))
                 (instance-predicate (tc:ty-class-instance-predicate instance)))
            (case (length instance-constraints)
              (0
               (format stream "~A~%" instance-predicate))
              (1
               (format stream "~A ~A ~A~%"
                       (first instance-constraints)
                       (if settings:*coalton-print-unicode* "⇒" "=>")
                       instance-predicate))
              (t
               (format stream "~A ~A ~A~%"
                       instance-constraints
                       (if settings:*coalton-print-unicode* "⇒" "=>")
                       instance-predicate)))))))))

(defun print-specialization (stream entry env)
  (destructuring-bind (name . specs) entry
    (format stream "  ~A :: ~A~%" name (tc:lookup-value-type env name))
    (dolist (spec specs)
      (format stream "    ~A :: ~A~%"
              (tc:specialization-entry-to spec)
              (tc:specialization-entry-to-ty spec)))
    (format stream "~%"))
  (format stream "~%"))

;; Toplevel debug functions

(defun coalton:print-value-db (&optional package)
  "Print the global value environment"
  (print-db (lambda (stream entry)
              (destructuring-bind (name . type) entry
                (format stream "  ~A :: ~A~%" name type)))
            :value :package package))

(defun coalton:print-type-db (&optional package)
  "Print the global type environment"
  (print-db (lambda (stream entry)
              (destructuring-bind (name . entry) entry
                (format stream "  ~A :: ~A~%" name (tc:kind-of entry))))
            :type :package package))

(defun coalton:print-class-db (&optional package)
  "Print the global class environment"
  (print-db #'print-class :class :package package))

(defun coalton:print-instance-db (&optional package)
  "Print the global instance environment"
  (let* ((env entry:*global-environment*)
         (class-instances (lambda (class)
                            (tc:lookup-class-instances env class :no-error t))))
    (print-db #'print-instance :class
              :package package
              :entry-resolver class-instances)))

(defun coalton:print-specializations (&optional package)
  "Print all specializations"
  (print-db (a:rcurry #'print-specialization entry:*global-environment*) :specialization :package package))

(defun coalton:type-of (symbol)
  "Look up the type of value SYMBOL in the global environment"
  (tc:lookup-value-type entry:*global-environment* symbol))

(defun coalton:kind-of (symbol)
  "Look up the kind of type SYMBOL in the global environment"
  (tc:kind-of (coalton-impl/typechecker::type-entry-type (tc:lookup-type entry:*global-environment* symbol))))

(defun coalton:lookup-code (name)
  "Look up the compiled code of a given definition"
  (declare (type symbol name))
  (tc:lookup-code entry:*global-environment* name))

(defun coalton:lookup-class (name)
  "Look up a given class"
  (declare (type symbol name))
  (tc:lookup-class entry:*global-environment* name))

(defun coalton:lookup-fundeps (name)
  "Look up the fundep structure for a given class"
  (declare (type symbol name))
  (tc:lookup-fundep-environment entry:*global-environment* name))
