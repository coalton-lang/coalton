#!/usr/bin/env -S sbcl --script

(require :asdf)

(defpackage #:dependency-audit
  (:use #:cl)
  (:import-from #:asdf
                #:component-name
                #:find-system
                #:system-source-file)
  (:import-from #:asdf/system
                #:system-defsystem-depends-on
                #:system-depends-on)
  (:import-from #:uiop
                #:command-line-arguments
                #:ensure-directory-pathname
                #:getcwd
                #:getenv
                #:quit
                #:split-native-pathnames-string)
  (:import-from #:uiop/os
                #:featurep))

(in-package #:dependency-audit)

(defstruct system-info
  name
  depends-on
  defsystem-depends-on
  source-file
  origin)

(defun usage (&optional (stream *error-output*))
  (format stream
          "Usage: dependency-audit.lisp [--output PATH] [--all-local] SYSTEM...~%~
Produces a DOT graph for the transitive ASDF dependencies of the given root systems.~%"))

(defun normalize-system-name (name)
  (etypecase name
    (string (string-downcase name))
    (symbol (string-downcase (symbol-name name)))))

(defun form-head-name (form)
  (and (consp form)
       (symbolp (first form))
       (string-upcase (symbol-name (first form)))))

(defun defsystem-form-p (form)
  (equal (form-head-name form) "DEFSYSTEM"))

(defun in-package-form-p (form)
  (equal (form-head-name form) "IN-PACKAGE"))

(defun option-value (options option-name)
  (loop for tail on options by #'cddr
        for key = (first tail)
        for value = (second tail)
        when (and (symbolp key)
                  (string-equal option-name (symbol-name key)))
          do (return value)))

(defun dependency-spec-systems (spec)
  (labels ((recur (value)
             (cond
               ((null value) nil)
               ((stringp value) (list (normalize-system-name value)))
               ((symbolp value) (list (normalize-system-name value)))
               ((consp value)
                (let ((head (form-head-name value)))
                  (cond
                    ((equal head "FEATURE")
                     (destructuring-bind (_ feature dependency &rest ignored) value
                       (declare (ignore _ ignored))
                       (when (featurep feature)
                         (recur dependency))))
                    ((equal head "VERSION")
                     (recur (second value)))
                    (t nil))))
               (t nil))))
    (recur spec)))

(defun dependency-list-systems (dependency-list)
  (remove-duplicates
   (loop for spec in dependency-list
         append (dependency-spec-systems spec))
   :test #'string=))

(defun make-local-system-info (form source-file)
  (let* ((name (normalize-system-name (second form)))
         (options (cddr form)))
    (make-system-info
     :name name
     :depends-on (dependency-list-systems
                  (or (option-value options "DEPENDS-ON") '()))
     :defsystem-depends-on (dependency-list-systems
                            (or (option-value options "DEFSYSTEM-DEPENDS-ON") '()))
     :source-file (namestring source-file)
     :origin :local)))

(defun register-local-system (info systems)
  (unless (gethash (system-info-name info) systems)
    (setf (gethash (system-info-name info) systems) info)))

(defun load-local-systems-from-file (file systems)
  (handler-case
      (with-open-file (stream file :direction :input)
        (let ((*package* (find-package :cl-user))
              (*read-eval* nil)
              (eof (gensym "EOF")))
          (loop for form = (read stream nil eof)
                until (eq form eof)
                do (cond
                     ((defsystem-form-p form)
                      (register-local-system (make-local-system-info form file) systems))
                     ((in-package-form-p form)
                      (let* ((package-designator (second form))
                             (package-name (and package-designator
                                                (normalize-system-name package-designator)))
                             (package (and package-name
                                           (find-package package-name))))
                        (when package
                          (setf *package* package))))))))
    (error (condition)
      (format *error-output* "warning: failed to read ~A: ~A~%" file condition))))

(defun asd-files-under (root)
  (let ((files '())
        (seen-dirs (make-hash-table :test #'equal)))
    (labels ((walk (directory)
               (let ((true-directory (ignore-errors (truename directory))))
                 (when (and true-directory
                            (null (gethash true-directory seen-dirs)))
                   (setf (gethash true-directory seen-dirs) t)
                   (dolist (file (directory (merge-pathnames "*.asd" true-directory)))
                     (push file files))
                   (dolist (subdirectory (uiop:subdirectories true-directory))
                     (walk subdirectory))))))
      (when (probe-file root)
        (walk (ensure-directory-pathname root))))
    files))

(defun registry-roots ()
  (let ((raw (getenv "CL_SOURCE_REGISTRY")))
    (if (and raw (not (string= raw "")))
        (mapcar #'ensure-directory-pathname
                (split-native-pathnames-string raw))
        (list (ensure-directory-pathname (getcwd))))))

(defun build-local-system-index ()
  (let ((systems (make-hash-table :test #'equal)))
    (dolist (root (registry-roots))
      (dolist (file (asd-files-under root))
        (load-local-systems-from-file file systems)))
    systems))

(defun local-system-names (systems)
  (sort (loop for name being the hash-keys of systems collect name)
        #'string<))

(defun maybe-load-quicklisp ()
  (let* ((quicklisp-home
           (ensure-directory-pathname
            (or (getenv "QUICKLISP_HOME")
                (merge-pathnames "quicklisp/" (user-homedir-pathname)))))
         (setup-file (merge-pathnames "setup.lisp" quicklisp-home)))
    (when (probe-file setup-file)
      (load setup-file :verbose nil :print nil))))

(defun external-system-info (name)
  (handler-case
      (let ((system (find-system name nil)))
        (if system
            (make-system-info
             :name (normalize-system-name (component-name system))
             :depends-on (dependency-list-systems (system-depends-on system))
             :defsystem-depends-on
             (dependency-list-systems (system-defsystem-depends-on system))
             :source-file (when (system-source-file system)
                            (namestring (system-source-file system)))
             :origin :external)
            (make-system-info :name name :origin :missing)))
    (error (condition)
      (format *error-output* "warning: failed to resolve system ~A: ~A~%" name condition)
      (make-system-info :name name :origin :missing))))

(defun resolve-system (name local-systems resolved-systems)
  (multiple-value-bind (cached presentp)
      (gethash name resolved-systems)
    (cond
      (presentp cached)
      ((gethash name local-systems)
       (setf (gethash name resolved-systems) (gethash name local-systems)))
      (t
       (setf (gethash name resolved-systems) (external-system-info name))))))

(defun collect-graph (roots local-systems)
  (let ((resolved-systems (make-hash-table :test #'equal))
        (edges (make-hash-table :test #'equal))
        (queue (copy-list roots))
        (seen (make-hash-table :test #'equal)))
    (labels ((enqueue (name)
               (unless (gethash name seen)
                 (push name queue)))
             (add-edge (from kind to)
               (setf (gethash (list from kind to) edges) t)))
      (loop while queue
            for name = (pop queue)
            unless (gethash name seen)
              do (setf (gethash name seen) t)
                 (let ((info (resolve-system name local-systems resolved-systems)))
                   (dolist (dependency (system-info-depends-on info))
                     (add-edge name :depends-on dependency)
                     (enqueue dependency))
                   (dolist (dependency (system-info-defsystem-depends-on info))
                     (add-edge name :defsystem dependency)
                     (enqueue dependency)))))
    (values resolved-systems edges)))

(defun dot-string (value)
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across value
          do (case char
               (#\\ (write-string "\\\\" out))
               (#\" (write-string "\\\"" out))
               (#\Newline (write-string "\\n" out))
               (t (write-char char out))))
    (write-char #\" out)))

(defun write-attributes (stream attributes)
  (when attributes
    (format stream " [")
    (loop for (name . value) in attributes
          for firstp = t then nil
          do (unless firstp
               (write-string ", " stream))
             (format stream "~A=~A" name (dot-string value)))
    (write-char #\] stream)))

(defun node-attributes (info roots)
  (let ((attributes '())
        (styles '()))
    (when (member (system-info-name info) roots :test #'string=)
      (push '("fillcolor" . "#fff2cc") attributes)
      (push "filled" styles)
      (push "rounded" styles))
    (when (eq (system-info-origin info) :missing)
      (push '("color" . "#cc0000") attributes)
      (push "dashed" styles)
      (pushnew "rounded" styles :test #'string=))
    (when styles
      (push (cons "style" (format nil "~{~A~^,~}" (nreverse styles))) attributes))
    (nreverse attributes)))

(defun edge-attributes (kind)
  (case kind
    (:defsystem
     '(("color" . "#666666")
       ("label" . "defsystem")
       ("style" . "dashed")))
    (otherwise nil)))

(defun write-dot (roots systems edges stream)
  (let ((sorted-names
          (sort (loop for name being the hash-keys of systems collect name)
                #'string<))
        (sorted-edges
          (sort (loop for edge being the hash-keys of edges collect edge)
                (lambda (left right)
                  (destructuring-bind (left-from left-kind left-to) left
                    (destructuring-bind (right-from right-kind right-to) right
                      (or (string< left-from right-from)
                          (and (string= left-from right-from)
                               (or (string< (symbol-name left-kind) (symbol-name right-kind))
                                   (and (eq left-kind right-kind)
                                        (string< left-to right-to)))))))))))
    (format stream "digraph asdf_dependencies {~%")
    (format stream "  rankdir=LR;~%")
    (format stream "  graph [fontname=~A];~%" (dot-string "Helvetica"))
    (format stream "  node [fontname=~A, shape=~A];~%"
            (dot-string "Helvetica")
            (dot-string "box"))
    (format stream "  edge [fontname=~A];~%~%" (dot-string "Helvetica"))
    (dolist (name sorted-names)
      (format stream "  ~A" (dot-string name))
      (write-attributes stream (node-attributes (gethash name systems) roots))
      (format stream ";~%"))
    (when sorted-edges
      (terpri stream))
    (dolist (edge sorted-edges)
      (destructuring-bind (from kind to) edge
        (format stream "  ~A -> ~A" (dot-string from) (dot-string to))
        (write-attributes stream (edge-attributes kind))
        (format stream ";~%")))
    (format stream "}~%")))

(defun parse-arguments (arguments)
  (let ((output-file nil)
        (all-local-p nil)
        (roots '()))
    (loop while arguments
          for argument = (pop arguments)
          do (cond
               ((string= argument "--help")
                (usage *standard-output*)
                (quit 0))
               ((string= argument "--output")
                (unless arguments
                  (format *error-output* "error: --output requires a path~%")
                  (usage)
                  (quit 1))
                (setf output-file (pop arguments)))
               ((string= argument "--all-local")
                (setf all-local-p t))
               (t
                (push (normalize-system-name argument) roots))))
    (values output-file all-local-p (nreverse roots))))

(defun main ()
  (multiple-value-bind (output-file all-local-p roots)
      (parse-arguments (command-line-arguments))
    (maybe-load-quicklisp)
    (let* ((local-systems (build-local-system-index))
           (effective-roots
             (append (when all-local-p
                       (local-system-names local-systems))
                     roots))
           (root-systems (remove-duplicates effective-roots :test #'string=))
           (resolved-output-file
             (and output-file
                  (namestring
                   (merge-pathnames output-file
                                    (ensure-directory-pathname (getcwd)))))))
      (when (null root-systems)
        (usage)
        (quit 1))
      (multiple-value-bind (systems edges)
          (collect-graph root-systems local-systems)
        (if resolved-output-file
            (with-open-file (stream resolved-output-file
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
              (write-dot root-systems systems edges stream))
            (write-dot root-systems systems edges *standard-output*))
        (when resolved-output-file
          (format *error-output* "wrote ~A~%" resolved-output-file))))))

(handler-case
    (main)
  (error (condition)
    (format *error-output* "error: ~A~%" condition)
    (quit 1)))
