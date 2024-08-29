;;;; These are the entry points for the coalton documentation
;;;; generator. A standard library package is any package with the
;;;; exact name 'coalton' or whose name starts with 'coalton-library'.
;;;;
;;;; To generate markdown input for the static website generator, do:
;;;;
;;;;   (write-stdlib-documentation-to-file
;;;;     "../coalton-website/content/reference.md"
;;;;     :backend :hugo)

(defpackage #:coalton/doc
  (:documentation "Documentation generator for COALTON.")
  (:use
   #:cl
   #:coalton/doc/base
   #:coalton/doc/model)
  (:export
   #:write-stdlib-documentation-to-file
   #:write-documentation))

(in-package #:coalton/doc)

(defvar *coalton-github*
  "https://github.com/coalton-lang/coalton")

(defun head-revision ()
  "Get the current commit by checking the environment variable GITHUB_SHA set by GitHub CI. If it is unset, use the output of git rev-parse instead."
  (or (uiop:getenv "GITHUB_SHA")
      (string-trim '(#\Newline)
                   (uiop:run-program "git rev-parse HEAD" :output ':string))))

;;;  In GitHub CI, the environment variable GITHUB_WORKSPACE names the
;;;  directory containing the directory into which Coalton is cloned
;;;  during build: appending '/coalton' produces a valid repository
;;;  path.

(defun local-path ()
  "Return the path to the directory containing Coalton source."
  (let ((ci-workspace (uiop:getenv "GITHUB_WORKSPACE")))
    (if ci-workspace
        (format nil "~A/coalton" ci-workspace)
        (namestring (asdf:system-source-directory "coalton")))))

(defun remote-path (revision)
  "Return the URL corresponding to the local revision of Coalton source."
  (format nil "~A/tree/~A"
          *coalton-github*
          (or revision
              (head-revision))))

(defun write-documentation (filename packages
                            &key (backend :markdown) (revision nil))
  "Write the documentation for a set of PACKAGES to FILENAME, using backend named by keyword BACKEND."
  (let ((*local* (local-path))
        (*remote* (remote-path revision)))
    (format t "Generating documentation for ~D packages~%~
Local path = ~A~%~
Remote path = ~A~%"
            (length packages)
            *local*
            *remote*)
    (with-open-file (stream filename
                            :direction ':output
                            :if-exists ':supersede
                            :if-does-not-exist ':create)
      (write-packages (make-backend backend stream) packages))))

(defun write-stdlib-documentation-to-file (filename
                                           &key (backend ':markdown) (revision nil))
  "Write standard library documentation to FILENAME, using backend named by keyword BACKEND.

Possible values for BACKEND are:

  :markdown generate markdown documentation
  :hugo     generate input for hugo static site generator
            (mostly markdown, wrapped in metadata cruft)"
  (write-documentation filename (find-packages)
                       :backend backend
                       :revision revision))
