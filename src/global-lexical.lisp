(in-package #:coalton-impl)

;;; Allow the definition of global lexical values in Common
;;; Lisp. Based off of Ron Garret's GLOBALS.

(define-symbol-property lexical-cell)

(defun get-lexical-cell (symbol)
  (or (lexical-cell symbol)
      ;; Stash away the symbol so we don't have to cons up a new
      ;; string every time.
      (setf (lexical-cell symbol)
            ;; Intentionally obtuse name.
            (alexandria:format-symbol '#:coalton-global-symbols
                                      "(lexical) ~A::~A"
                                      (package-name (symbol-package symbol))
                                      symbol))))

(defmacro define-global-lexical (var val &key documentation)
  (let ((cell (get-lexical-cell var)))
    `(progn
       (global-vars:define-global-var ,cell ':|@@unbound@@|)
       (define-symbol-macro ,var ,cell)
       ,@(when documentation
           (list
            `(setf (documentation ',var 'cl:variable) ,documentation)))
       (setf ,var (load-time-value ,val))
       ',var)))
