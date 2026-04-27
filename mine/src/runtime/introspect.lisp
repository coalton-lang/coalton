;;;; introspect.lisp -- Runtime introspection for the IDE.
;;;;
;;;; Provides symbol lookup, arglist queries, and package enumeration.

(in-package #:mine/runtime/introspect)

;;; Helpers

(defun %find-symbol-in-package (symbol-name package-name)
  "Find the symbol named SYMBOL-NAME in PACKAGE-NAME.
Returns the symbol or NIL."
  (let ((pkg (find-package (string-upcase package-name))))
    (when pkg
      (find-symbol (string-upcase symbol-name) pkg))))

(defun %symbol-type (sym)
  "Classify SYM as :function, :macro, :special-form, :generic-function,
:class, :variable, :constant, or :symbol."
  (cond ((and (fboundp sym)
              (macro-function sym))
         ':macro)
        ((and (fboundp sym)
              (special-operator-p sym))
         ':special-form)
        ((and (fboundp sym)
              (typep (fdefinition sym) 'generic-function))
         ':generic-function)
        ((fboundp sym)
         ':function)
        ((find-class sym nil)
         ':class)
        ((constantp sym)
         ':constant)
        ((boundp sym)
         ':variable)
        (t ':symbol)))

(defun %get-arglist (sym)
  "Get the arglist for SYM using sb-introspect, returning a string or NIL."
  (when (fboundp sym)
    (handler-case
        (let ((arglist (sb-introspect:function-lambda-list sym)))
          (let ((*package* (symbol-package sym)))
            (princ-to-string arglist)))
      (error () nil))))

(defun %get-documentation (sym)
  "Collect documentation for SYM across all documentation types."
  (let ((docs nil))
    (dolist (type '(function variable type structure setf
                   compiler-macro method-combination))
      (let ((doc (documentation sym type)))
        (when doc
          (push (cons type doc) docs))))
    (nreverse docs)))

(defun %coalton-type (sym)
  "Try to get the Coalton type for SYM. Returns a string or NIL."
  (handler-case
      (let* ((env coalton-impl/entry:*global-environment*)
             (ty (coalton-impl/typechecker/environment:lookup-value-type env sym)))
        (coalton-impl/typechecker/type-string:type-to-string ty env))
    (error () nil)))

;;; Public API

(defun symbol-info (symbol-name package-name)
  "Return an alist of information about SYMBOL-NAME in PACKAGE-NAME.
Keys: :name, :package, :type, :documentation, :arglist, :value, :coalton-type."
  (let ((sym (%find-symbol-in-package symbol-name package-name)))
    (when sym
      (let ((info (list (cons ':name (symbol-name sym))
                        (cons ':package (package-name (symbol-package sym)))
                        (cons ':type (%symbol-type sym)))))
        ;; Documentation
        (let ((docs (%get-documentation sym)))
          (when docs
            (push (cons ':documentation
                        (cdr (first docs)))  ; primary documentation string
                  info)))
        ;; Arglist for callable symbols
        (when (fboundp sym)
          (let ((arglist (%get-arglist sym)))
            (when arglist
              (push (cons ':arglist arglist) info))))
        ;; Value for bound variables
        (when (and (boundp sym)
                   (not (fboundp sym)))
          (handler-case
              (let ((val (princ-to-string (symbol-value sym))))
                (push (cons ':value
                            (if (> (length val) 200)
                                (concatenate 'string
                                             (subseq val 0 200)
                                             "...")
                                val))
                      info))
            (error () nil)))
        ;; Coalton type
        (let ((ct (%coalton-type sym)))
          (when ct
            (push (cons ':coalton-type ct) info)))
        (nreverse info)))))

(defun function-arglist (function-name package-name)
  "Return the arglist of FUNCTION-NAME as a string.
Looks up the symbol in PACKAGE-NAME, then queries sb-introspect.
For Coalton functions, also tries the Coalton environment."
  (let ((sym (%find-symbol-in-package function-name package-name)))
    (cond
      ;; Standard CL function
      ((and sym (fboundp sym))
       (or (%get-arglist sym) ""))
      ;; Try Coalton type signature as a fallback
      ((and sym (%coalton-type sym))
       (format nil "~A :: ~A" function-name (%coalton-type sym)))
      (t ""))))
