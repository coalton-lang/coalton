(in-package #:mine-tests)

(defun %mine-project-test-root ()
  (merge-pathnames
   (format nil "mine-project-test-~A/"
           (string-downcase (symbol-name (gensym))))
   (uiop:temporary-directory)))

(defun %read-utf8-file (pathname)
  (with-open-file (stream pathname
                          :direction :input
                          :external-format :utf-8)
    (let ((text (make-string (file-length stream))))
      (read-sequence text stream)
      text)))

(defun check-create-project-creates-new-project ()
  (let* ((root (%mine-project-test-root))
         (name "fresh-project")
         (project-dir (merge-pathnames (format nil "~A/" name) root))
         (main-path (merge-pathnames "src/main.lisp" project-dir))
         (asd-path (merge-pathnames (format nil "~A.asd" name) project-dir)))
    (unwind-protect
         (let ((result (mine/app/mine::create-project-files!
                        name
                        (namestring root)
                        coalton:False)))
           (%check (coalton/result:ok? result)
                   "Expected new project creation to succeed, got ~S"
                   result)
           (%check (probe-file main-path)
                   "Expected main file to be created at ~S"
                   main-path)
           (%check (probe-file asd-path)
                   "Expected ASD file to be created at ~S"
                   asd-path)
           (%check (search "(defun main" (%read-utf8-file main-path)
                           :test #'char=)
                   "Expected Lisp project skeleton in ~S"
                   main-path))
      (ignore-errors (uiop:delete-directory-tree root :validate t)))))

(defun check-create-project-refuses-existing-directory ()
  (let* ((root (%mine-project-test-root))
         (name "existing-project")
         (project-dir (merge-pathnames (format nil "~A/" name) root))
         (main-path (merge-pathnames "src/main.ct" project-dir))
         (asd-path (merge-pathnames (format nil "~A.asd" name) project-dir))
         (main-sentinel ";; keep this main file intact")
         (asd-sentinel ";; keep this asd file intact"))
    (unwind-protect
         (progn
           (ensure-directories-exist main-path)
           (%write-utf8-file main-path main-sentinel)
           (%write-utf8-file asd-path asd-sentinel)
           (let ((result (mine/app/mine::create-project-files!
                          name
                          (namestring root)
                          coalton:True)))
             (%check (coalton/result:err? result)
                     "Expected existing project directory to be rejected, got ~S"
                     result))
           (%check (string= main-sentinel (%read-utf8-file main-path))
                   "Expected existing main file to remain unchanged")
           (%check (string= asd-sentinel (%read-utf8-file asd-path))
                   "Expected existing ASD file to remain unchanged"))
      (ignore-errors (uiop:delete-directory-tree root :validate t)))))

(defun check-create-project-rejects-path-like-name ()
  (dolist (name '("path-like-project/" "path\\like-project"))
    (let ((root (%mine-project-test-root)))
      (unwind-protect
           (let ((result (mine/app/mine::create-project-files!
                          name
                          (namestring root)
                          coalton:True)))
             (%check (coalton/result:err? result)
                     "Expected path-like project name ~S to be rejected, got ~S"
                     name
                     result)
             (%check (not (probe-file root))
                     "Expected invalid project name ~S to leave root untouched at ~S"
                     name
                     root))
        (ignore-errors (uiop:delete-directory-tree root :validate t))))))
