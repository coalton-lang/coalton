;;;; utilities.lisp

(in-package #:coalton-impl/util)

(defmacro debug-log (&rest vars)
  "Log names and values of VARS to standard output"
  `(format t
           ,(format nil "~{~A: ~~A~~%~}" vars)
           ,@vars))

(defmacro debug-tap (var)
  (let ((var-name (gensym)))
    `(let ((,var-name ,var))
       (format t ,(format nil "~A: ~~A~~%" var) ,var-name)
       ,var-name)))

(define-condition coalton-bug (error)
  ((reason :initarg :reason
           :reader coalton-bug-reason)
   (args :initarg :args
         :reader coalton-bug-args))
  (:report (lambda (c s)
             (format s "Internal coalton bug: ~?~%~%If you are seeing this, please file an issue on Github."
                     (coalton-bug-reason c)
                     (coalton-bug-args c)))))

(defun coalton-bug (reason &rest args)
  (error 'coalton-bug
         :reason reason
         :args args))

(defmacro unreachable ()
  "Assert that a branch of code cannot be evaluated in the course of normal execution."
  ;; Ideally, we would *catch* the code-deletion-note condition and signal a
  ;; warning if no such condition was seen (i.e., if SBCL didn't prove the
  ;; (UNREACHABLE) form to be prunable). As far as I can tell, though, that
  ;; requires wrapping the entire containing toplevel form in a HANDLER-BIND,
  ;; which cannot be done by the expansion of an inner macro form.
  '(locally
      #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
      (coalton-bug "This error was expected to be unreachable in the Coalton source code.")))

(defun maphash-values-new (function table)
  "Map across the values of a hash-table. Returns a new hash-table with unchanged keys."
  (declare (type function function)
           (type hash-table table))
  (let ((new (make-hash-table)))
    (loop :for k :being :the :hash-keys :of table
          :for v :being :the :hash-values :of table
          :do (setf (gethash k new) (funcall function v)))
    new))

(defun find-symbol? (name package)
  (declare (type string name package)
           (values symbol-list))
  (unless (find-package package)
    (return-from find-symbol?))

  (list (alexandria:ensure-symbol name package)))

(defun required (name)
  "A function to call as a slot initializer when it's required."
  (declare (type symbol name))
  (coalton-bug "A slot ~S (of package ~S) is required but not supplied" name (symbol-package name)))

(defun sexp-fmt (stream object &optional colon-modifier at-modifier)
  "A formatter for qualified S-expressions. Use like

    (format t \"~/coalton-impl::sexp-fmt/\" '(:x y 5))

and it will print a flat S-expression with all symbols qualified."
  (declare (ignore colon-modifier at-modifier))
  (let ((*print-pretty* nil)
        (*package* (find-package "KEYWORD")))
    (prin1 object stream)))

(defmacro include-if (condition &body body)
  `(when ,condition
     (list ,@ (remove nil body))))

(defmacro define-symbol-property (property-accessor)
  "Define an accessor for a symbol property.

Implementation notes: These notes aren't relevant to users of this macro, but are Good To Know.

    * The symbol's property is stored as a part of the symbol's plist.

    * The plist key is just the name of the accessor.
    "
  (check-type property-accessor symbol)
  (let ((symbol (gensym "SYMBOL"))
        (new-value (gensym "NEW-VALUE")))
    `(progn
       (declaim (inline ,property-accessor (setf ,property-accessor)))
       (defun ,property-accessor (,symbol)
         (get ,symbol ',property-accessor))
       (defun (setf ,property-accessor) (,new-value ,symbol)
         (setf (get ,symbol ',property-accessor) ,new-value)))))

(defun symbol-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'symbolp x)))

(deftype symbol-list ()
  '(satisfies symbol-list-p))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type symbol-list))

(deftype literal-value ()
  "Allowed literal values as Lisp objects."
  '(or integer ratio single-float double-float string character))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type literal-value))
