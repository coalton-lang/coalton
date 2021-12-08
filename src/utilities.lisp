;;;; utilities.lisp

(in-package #:coalton-util)

(define-condition coalton-bug (error)
  ((reason :initarg :reason
           :reader coalton-bug-reason)
   (args :initarg :args
         :reader coalton-bug-args))
  (:report (lambda (c s)
             (format s "Internal coalton bug: ~?"
                     (coalton-bug-reason c)
                     (coalton-bug-args c)))))

(defun coalton-bug (reason &rest args)
  (error 'coalton-bug
         :reason reason
         :args args))

(defun unreachable ()
  "A function to call when something should be unreachable."
  (coalton-bug "This error was expected to be unreachable in the Coalton source code."))

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

(defmacro define-symbol-property (property-accessor &key
                                                      (type nil type-provided)
                                                      documentation)
  "Define an accessor for a symbol property.

Implementation notes: These notes aren't relevant to users of this macro, but are Good To Know.

    * The symbol's property is stored as a part of the symbol's plist.

    * The plist key is just the name of the accessor.
    "
  (check-type property-accessor symbol)
  (check-type documentation (or null string))
  (let ((symbol (gensym "SYMBOL"))
        (new-value (gensym "NEW-VALUE")))
    `(progn
       (declaim (inline ,property-accessor (setf ,property-accessor)))
       ,@(include-if type-provided
           `(declaim (ftype (function (symbol) (or null ,type)) ,property-accessor))
           `(declaim (ftype (function (,type symbol) ,type) (setf ,property-accessor))))
       (defun ,property-accessor (,symbol)
         ,@(include-if documentation documentation)
         (get ,symbol ',property-accessor))
       (defun (setf ,property-accessor) (,new-value ,symbol)
         (setf (get ,symbol ',property-accessor) ,new-value))
       ;; Return the name defined.
       ',property-accessor)))
