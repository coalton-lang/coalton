;;;; These are the entry points for the coalton documentation
;;;; generator. A standard library package is any package with the
;;;; exact name 'coalton' or whose name starts with 'coalton-library'.
;;;;
;;;; To generate markdown input for the static website generator, do:
;;;;
;;;;   (write-stdlib-documentation-to-file "../coalton-website/content/reference.md"
;;;;                                       :backend :hugo)

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

(defun head-revision ()
  "Look up the most recent commit, either in the env var GITHUB_SHA which is provided by github CI, or by calling out to git rev-parse."
  (or (uiop:getenv "GITHUB_SHA")
      (string-trim '(#\Newline)
                   (uiop:run-program "git rev-parse HEAD" :output ':string))))

(defun local-path ()
  (or (uiop:getenv "GITHUB_WORKSPACE")
      (namestring (asdf:system-source-directory "coalton"))))

(defun remote-path (revision)
  (format nil "http://github.com/coalton-lang/coalton/tree/~A"
          (or revision
              (head-revision))))

(defun write-documentation (filename packages
                            &key (backend :markdown) (revision nil))
  "Write the documentation for a set of PACKAGES to FILENAME, using backend named by keyword BACKEND."
  (let ((*local* (local-path))
        (*remote* (remote-path revision)))
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
