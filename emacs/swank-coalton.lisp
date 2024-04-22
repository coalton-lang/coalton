;;; swank-coalton.lisp

(in-package :swank)

(defun system-loaded-p (system-designator)
  (find system-designator (asdf:already-loaded-systems)
        :test #'string=))

(defun system-available-p (system-designator)
  (asdf:find-system system-designator))

(defun system-status (system-designator)
  (cond ((system-loaded-p system-designator)
         :loaded)
        ((system-available-p system-designator)
         :available)
        (t
         :unavailable)))


(defslimefun swank-coalton-status ()
  (system-status "coalton"))

(defslimefun swank-coalton-init ()
  (asdf:load-system "coalton"))


(defslimefun swank-coalton--ast (form package)
  :xyz)

(provide :swank-coalton)



(swank-coalton--ast "(define (symbol-name sym)
  (match sym
    ((Symbol s) s)))"
                    "(package diff-example)")

