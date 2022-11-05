;;;; utilities.lisp

(defpackage #:coalton-impl/util
  (:documentation "Utility functions and methods used throughout COALTON.")
  (:use #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree))
  (:export
   #:+keyword-package+                  ; CONSTANT
   #:required                           ; FUNCTION
   #:unreachable                        ; MACRO
   #:coalton-bug                        ; FUNCTION
   #:debug-log                          ; MACRO
   #:debug-tap                          ; MACRO
   #:symbol-list                        ; TYPE
   #:cst-list                           ; TYPE
   #:cst-source-range                   ; FUNCTION
   #:literal-value                      ; TYPE
   #:maphash-values-new                 ; FUNCTION
   #:find-symbol?                       ; FUNCTION
   #:sexp-fmt                           ; FUNCTION
   #:take-until                         ; FUNCTION
   #:project-indicies                   ; FUNCTION
   #:project-map                        ; FUNCTION
   ))

(in-package #:coalton-impl/util)

(alexandria:define-constant +keyword-package+ (find-package "KEYWORD") :test #'eq)

(defun symbol-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'symbolp x)))

(deftype symbol-list ()
  '(satisfies symbol-list-p))

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

(deftype literal-value ()
  "Allowed literal values as Lisp objects."
  '(or integer ratio single-float double-float string character))


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

(defun project-indicies (indices data)
  (declare (type list indices data)
           (values list))
  (labels ((inner (is xs pos out)
             (cond
               ;; Data is done, indices are not
               ((and is (null xs))
                (error "Indices ~A extend past data" is))

               ;; Data or indicies are done
               ((or (null is)
                    (null xs))
                out)

               ;; match
               ((eql (car is) pos)
                (inner (cdr is) (cdr xs) (1+ pos) (cons (car xs) out)))

               ;; indicie is past pos
               ((> pos (car is))
                (inner (cdr is) xs pos out))

               (t
                (inner is (cdr xs) (1+ pos) out)))))
    (declare (dynamic-extent #'inner))
    (nreverse (inner indices data 0 nil))))

(defun project-map (indicies map data)
  (declare (type symbol-list indicies)
           (type hash-table map)
           (type list data))
  (project-indicies
   (sort 
    (loop :for key :in indicies
          :collect (gethash key map))
    #'<)
   data))
