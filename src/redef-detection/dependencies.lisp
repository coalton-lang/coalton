(defpackage #:coalton-impl/redef-detection/dependencies
  (:use #:cl)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:tc-env #:coalton-impl/typechecker/environment))
  (:export
   #:extract-function-dependencies
   #:dependency-registry
   #:dependency-registry-forward-deps
   #:dependency-registry-reverse-deps
   #:make-dependency-registry
   #:*dependency-registry*
   #:record-dependencies
   #:get-function-callers
   #:get-function-location
   #:find-affected-functions))
(in-package #:coalton-impl/redef-detection/dependencies)

(defun extract-function-dependencies (node env &key renamed)
  "Return the global values in ENV referenced by NODE. Set RENAMED if NODE
has already passed through the parser's lexical renamer."
  (declare (type tc-env:environment env)
           (values list))
  (loop :for name :in (parser:collect-referenced-variable-names
                       (if renamed node (parser:rename-variables node)))
        :when (tc-env:lookup-value-type env name :no-error t)
          :collect name))

;;;
;;; Dependency Registry
;;;

(defstruct dependency-registry
  "Tracks function call relationships."
  ;; Map: function-name (symbol) -> list of function names it calls
  (forward-deps (make-hash-table :test 'eq) :type hash-table :read-only t)
  ;; Map: function-name (symbol) -> list of function names that call it
  (reverse-deps (make-hash-table :test 'eq) :type hash-table :read-only t))

(defvar *dependency-registry* (make-dependency-registry)
  "Global registry of function dependencies.")

(defun record-dependencies (function-name code env &key renamed)
  "Record the globals referenced in CODE, excluding FUNCTION-NAME itself.
Set RENAMED when CODE is already lexically renamed."
  (declare (type symbol function-name)
           (type tc-env:environment env))

  (let* ((forward-deps (dependency-registry-forward-deps *dependency-registry*))
         (reverse-deps (dependency-registry-reverse-deps *dependency-registry*))
         (called-functions (remove function-name
                                   (extract-function-dependencies code env :renamed renamed)
                                   :test #'eq)))

    ;; Store forward dependencies
    (setf (gethash function-name forward-deps) called-functions)

    ;; Update reverse dependencies
    ;; First, clear old reverse dependencies for this function
    (maphash
     (lambda (callee callers)
       (setf (gethash callee reverse-deps)
             (remove function-name callers :test #'eq)))
     reverse-deps)

    ;; Add new reverse dependencies
    (dolist (callee called-functions)
      (push function-name (gethash callee reverse-deps nil)))

    (values)))

(defun get-function-callers (function-name)
  "Get list of functions that directly call FUNCTION-NAME."
  (declare (type symbol function-name)
           (values list))
  (gethash function-name (dependency-registry-reverse-deps *dependency-registry*) nil))

(defun get-function-location (function-name env)
  "Get source location for FUNCTION-NAME from environment, or NIL if not available."
  (declare (type symbol function-name)
           (type tc-env:environment env)
           (values (or null source:location)))
  (let ((name-entry (tc-env:lookup-name env function-name :no-error t)))
    (when name-entry
      (source:location name-entry))))

(defun find-affected-functions (function-name &optional (visited (make-hash-table :test 'eq)))
  "Find all functions transitively affected by changes to FUNCTION-NAME.
Handles circular dependencies by tracking VISITED functions."
  (declare (type symbol function-name)
           (type hash-table visited)
           (values list))

  (let ((result nil))
    (labels ((visit (fname)
               (unless (gethash fname visited)
                 (setf (gethash fname visited) t)
                 (let ((callers (get-function-callers fname)))
                   (dolist (caller callers)
                     (push caller result)
                     (visit caller))))))
      (visit function-name))

    ;; Return unique, sorted list
    (sort (remove-duplicates result :test #'eq) #'string< :key #'symbol-name)))
