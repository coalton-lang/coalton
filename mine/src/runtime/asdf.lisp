;;;; asdf.lisp -- ASDF integration for the runtime server.
;;;;
;;;; Loads systems, queries dependencies, and enumerates source files.

(in-package #:mine/runtime/asdf)

;;; Public API

(defun load-system (system-name)
  "Load the ASDF system named SYSTEM-NAME.
Returns T on success, or (values NIL error-message) on failure."
  (handler-case
      (progn
        (asdf:load-system system-name)
        t)
    (error (c)
      (values nil (format nil "~A" c)))))

(defun system-dependencies (system-name)
  "Return a list of dependency name strings for the ASDF system SYSTEM-NAME.
Returns NIL if the system is not found."
  (handler-case
      (let ((system (asdf:find-system system-name nil)))
        (when system
          (mapcar (lambda (dep)
                    (etypecase dep
                      (string dep)
                      (symbol (string-downcase (symbol-name dep)))
                      (cons
                       ;; Handle (:feature :foo "bar") and (:version "foo" "1.0")
                       ;; by extracting the system name
                       (let ((name (if (keywordp (first dep))
                                       (second dep)
                                       (first dep))))
                         (etypecase name
                           (string name)
                           (symbol (string-downcase (symbol-name name))))))))
                  (asdf:system-depends-on system))))
    (error () nil)))

(defun system-files (system-name)
  "Return a list of source file pathname strings for SYSTEM-NAME.
Recursively walks the system's component tree."
  (handler-case
      (let ((system (asdf:find-system system-name nil)))
        (when system
          (%collect-files system)))
    (error () nil)))

;;; Internal helpers

(defun %collect-files (component)
  "Recursively collect source file pathnames from COMPONENT."
  (let ((files nil))
    (typecase component
      (asdf:module
       (dolist (child (asdf:component-children component))
         (setf files (nconc files (%collect-files child)))))
      (asdf:source-file
       (let ((path (asdf:component-pathname component)))
         (when path
           (push (namestring path) files)))))
    files))
