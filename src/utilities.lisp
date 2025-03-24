;;;; utilities.lisp

(defpackage #:coalton-impl/util
  (:documentation "Utility functions and methods used throughout COALTON.")
  (:use #:cl)
  (:shadow
   #:find-package
   #:find-symbol)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree))
  (:export
   #:+keyword-package+                  ; CONSTANT
   #:required                           ; FUNCTION
   #:unreachable                        ; MACRO
   #:coalton-bug                        ; FUNCTION
   #:debug-log                          ; MACRO
   #:debug-tap                          ; MACRO
   #:runtime-quote                      ; FUNCTION
   #:symbol-list                        ; TYPE
   #:string-list                        ; TYPE
   #:cst-list                           ; TYPE
   #:cst-source-range                   ; FUNCTION
   #:literal-value                      ; TYPE
   #:literal-equal                      ; FUNCTION
   #:maphash-values-new                 ; FUNCTION
   #:take                               ; FUNCTION
   #:drop                               ; FUNCTION
   #:find-package                       ; FUNCTION
   #:find-symbol                        ; FUNCTION
   #:find-symbol?                       ; FUNCTION
   #:take-until                         ; FUNCTION
   #:project-indices                    ; FUNCTION
   #:project-elements                   ; FUNCTION
   #:maybe-read-form                    ; FUNCTION
   #:ufixnum                            ; TYPE
   ))

(in-package #:coalton-impl/util)

(alexandria:define-constant +keyword-package+ (cl:find-package "KEYWORD") :test #'eq)

(defun symbol-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'symbolp x)))

(deftype symbol-list ()
  '(satisfies symbol-list-p))

(defun string-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'stringp x)))

(deftype string-list ()
  '(satisfies string-list-p))

(defun cst-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (x) (typep x 'cst:cst)) x)))

(deftype cst-list ()
  '(satisfies cst-list-p))

(defun cst-source-range (csts)
  (declare (type cst-list csts)
           (values cons))
  (cons
   (car (cst:source (first csts)))
   (cdr (cst:source (car (last csts))))))

(defmacro debug-log (&rest vars)
  "Log names and values of VARS to standard output"
  `(let ((*print-circle* nil))
     (format t
             ,(format nil "~&~{~A: ~~A~~%~}" vars)
             ,@vars)))

(defmacro debug-tap (var)
  (let ((var-name (gensym)))
    `(let ((,var-name ,var))
       (format t ,(format nil "~A: ~~A~~%" var) ,var-name)
       ,var-name)))

(defun runtime-quote (x)
  `',x)

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


;; Functions for runtime lookup of packages and symbols, used for
;; looking up parts of the standard library that aren't yet defined
;; when the compiler is loaded.

(defun find-package (name)
  "Look up a standard package by NAME, signalling a error if the package is not found."
  (declare (type string name)
           (values package &optional))
  (or (cl:find-package name)
      (coalton-bug "Missing required package: ~S" name)))

(defun find-symbol (name package)
  "Look up a symbol by NAME and PACKAGE, signalling a error if either package or the symbol is not found."
  (declare (type string name)
           (type (or package string) package)
           (values symbol))
  (when (stringp package)
    (setf package (find-package package)))
  (let ((sym (cl:find-symbol name package)))
    (unless sym
      (coalton-bug "Unable to find symbol with name ~A in package ~A" name package))
    sym))

(defun find-symbol? (name package)
  "Look up a symbol by NAME and PACKAGE. If the package is not found, return nil. Otherwise return the named symbol, interning it if necessary."
  (declare (type string name package)
           (values symbol-list))
  (unless (cl:find-package package)
    (return-from find-symbol?))
  (list (alexandria:ensure-symbol name package)))


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

(defun required (name)
  "A function to call as a slot initializer when it's required."
  (declare (type symbol name))
  (coalton-bug "A slot ~S (of package ~S) is required but not supplied" name (symbol-package name)))

(deftype literal-value ()
  "Allowed literal values as Lisp objects."
  '(or integer ratio single-float double-float string character))

(defun literal-equal (x y)
  "Are coalton literal values equal?"
  (declare (type literal-value x y)
           (values boolean))
  (equal x y))

(defun take-until (pred list)
  "Splits LIST into two lists on the element where PRED first returns true"
  (declare (type list list)
           (values list list))
  (let (out)
    (labels ((inner (xs)
               (cond
                 ((null xs) nil)
                 ((funcall pred (car xs)) xs)
                 (t
                  (push (car xs) out)
                  (inner (cdr xs))))))
      (declare (dynamic-extent #'inner))
      (let ((result (inner list)))
        (values
         (nreverse out)
         result)))))

(defun take (n list)
  (declare (type fixnum n)
           (type list list)
           (values list))
  (subseq list 0 n))

(defun drop (n list)
  (declare (type fixnum n)
           (type list list)
           (values list))
  (subseq list n))

(defun project-indices (indices data)
  "Select items from DATA at positions specified by INDICES.

Example:
  (project-indices '(0 2) '(a b c)) => (a c)"
  (mapcar (lambda (i)
            (if (>= i (length data))
                (error "Index ~A extends past data length ~A" i (length data))
                (nth i data)))
          indices))

(defun project-elements (elements list data)
  "Select items from DATA based on positions of ELEMENTS in LIST.

Examples:
  (project-elements '(:a) '(:a :b) '(1 2)) => (1)
  (project-elements '(:b :a) '(:a :b) '(1 2)) => (2 1)"
  (mapcar (lambda (idx)
            (nth (position idx list) data))
          elements))

(deftype ufixnum ()
  '(and fixnum unsigned-byte))
