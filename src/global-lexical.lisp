(in-package #:coalton-impl)

;;; Allow the definition of global lexical values in Common
;;; Lisp. Based off of Ron Garret's GLOBALS.

(define-symbol-property lexical-cell)

(defun get-lexical-cell (symbol)
  (or (lexical-cell symbol)
      (setf (lexical-cell symbol)
            ;; Intentionally obtuse name.
            (intern (format nil "(lexical) ~A::~A"
                            (package-name (symbol-package symbol))
                            symbol)
                    '#:coalton-global-symbols))))

(defmacro lexical-value (var)
  `(symbol-value ',(get-lexical-cell var)))

;;; TODO: Allow the type to be declared.
(defmacro define-global-lexical (var val)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (define-symbol-macro ,var (lexical-value ,var)))
     (setf (lexical-value ,var) ,val)
     ',var))
