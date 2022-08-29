(defpackage #:coalton-impl/typechecker/parse-define
  (:use
   #:cl
   #:coalton-impl/algorithm
   #:coalton-impl/ast)
  (:export
   #:parse-define-form))

(in-package #:coalton-impl/typechecker/parse-define)

(defun parse-define-form (form package &key (skip-inherited-symbol-checks nil))
  "Parse a COALTON:DEFINE form."
  (declare (type list form)
           (type package package)
           (values symbol node (or null string) &optional))
  (unless (and (eql (first form) 'coalton:define)
               (or (<= 3 (length form))   ; Without docstring
                    )) ; With docstring
    (error-parsing form "malformed DEFINE form"))

  ;; Defines either define a value or a function. Values and functions
  ;; in Coalton occupy the namespace, but the intent of the user can
  ;; be distinguished. A definition either looks like:
  ;;
  ;;     (DEFINE <var> <val>)
  ;;
  ;; or
  ;;
  ;;     (DEFINE (<fvar> <arg>*) <val>+)
  ;;
  ;; The former defines a variable, the latter defines a function.

  (let ((docstring nil)
        (var-thing (second form)))

    ;; Grab the docstring if it exists
    (when (and (> (length form) 3) (typep (third form) 'string))
      (setf docstring (third form)))


    (cond
      ;; (define () 5) is invalid
      ((null var-thing)
       (error-parsing form "Found a null value where a symbol or function was expected"))


    ;; Parse a variable declaration
    ((symbolp var-thing)
     (parse-define-form-variable
      var-thing
      (if docstring
          (fourth form)
          (third form))
      docstring
      package
      :skip-inherited-symbol-checks skip-inherited-symbol-checks))

    ((and (listp var-thing)
          (every #'symbolp var-thing))
     (parse-define-form-function
      (first var-thing)
      (rest var-thing)
      (if docstring
          (nthcdr 3 form)
          (nthcdr 2 form))
      docstring
      package
      :skip-inherited-symbol-checks skip-inherited-symbol-checks))

    (t
     (error-parsing form "Invalid define form.")))))

(defun parse-define-form-variable (var val docstring package &key (skip-inherited-symbol-checks nil))
  (declare (type symbol var)
           (type t val)
           (type package package)
           (values symbol node (or null string)))
  ;; The (DEFINE <var> <val>) case.
  ;; XXX: Should this be LETREC too? Probably for something like F = x => ... F.
  (unless (or skip-inherited-symbol-checks (equalp package (symbol-package var)))
    (error-inherited-symbol
     var
     package))
  (values var
          (parse-form val (make-immutable-map) package)
          docstring))

(defun parse-define-form-function (fvar args forms docstring package &key (skip-inherited-symbol-checks nil))
  (declare (type symbol fvar)
           (type list args)
           (type list forms)
           (type package package)
           (values symbol node (or null string)))
  ;; The (DEFINE (<fvar> <arg>*) <val>+) case.
  (unless (or skip-inherited-symbol-checks (equalp package (symbol-package fvar)))
    (error-inherited-symbol
     fvar
     package))
  (values fvar
          (parse-form `(coalton:fn ,args ,@forms) (make-immutable-map) package)
          docstring))

