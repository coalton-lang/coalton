(in-package #:mine-tests)

(defun %mine-project-test-root ()
  (merge-pathnames
    (format nil "mine-project-test-~A/"
            (string-downcase (symbol-name (gensym))))
    (uiop:temporary-directory)))

(defun %read-utf8-file (pathname)
  (with-open-file (stream pathname
                          :direction ':input
                          :external-format ':utf-8)
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

(defun check-buffer-manager-any-dirty-sees-non-current-buffer ()
  (let* ((root (%mine-project-test-root))
         (path-a (merge-pathnames "a.lisp" root))
         (path-b (merge-pathnames "b.lisp" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist path-a)
           (%write-utf8-file path-a "(defun a () nil)")
           (%write-utf8-file path-b "(defun b () nil)")
           (let* ((bm (mine/buffer/manager:bufmgr-new))
                  (result-a (mine/buffer/manager:bufmgr-open-file!
                             bm
                             (namestring path-a)))
                  (result-b (mine/buffer/manager:bufmgr-open-file!
                             bm
                             (namestring path-b))))
             (%check (coalton/result:ok? result-a)
                     "Expected first buffer to open, got ~S"
                     result-a)
             (%check (coalton/result:ok? result-b)
                     "Expected second buffer to open, got ~S"
                     result-b)
             (let* ((fallback (mine/buffer/buffer:buffer-new
                               (mine/buffer/buffer:BufferId 999999)
                               "fallback"))
                    (buf-a (coalton/result:ok-or-def fallback result-a))
                    (buf-b (coalton/result:ok-or-def fallback result-b)))
               (%check (not (mine/buffer/manager:bufmgr-any-dirty? bm))
                       "Expected clean buffers to report no dirty state")
               (mine/buffer/manager:bufmgr-switch! bm (mine/buffer/buffer:buffer-id buf-b))
               (mine/buffer/buffer:buffer-mark-dirty! buf-a)
               (%check (mine/buffer/manager:bufmgr-any-dirty? bm)
                       "Expected a dirty non-current buffer to be detected"))))
      (ignore-errors (uiop:delete-directory-tree root :validate t)))))

(defun check-project-tree-shows-hidden-rows-after-growing ()
  (let* ((tp (mine/pane/tree:tree-pane-new))
         (files (loop :for i :below 10
                      :collect (mine/pane/tree:TreeFile (format nil "file~D.lisp" i)
                                                        (format nil "/project/file~D.lisp" i))))
         (scr (mine/term/screen:screen-new 30 30)))
    (mine/pane/tree:tree-pane-set-root! tp (mine/pane/tree:TreeDir "project" files coalton:True))
    (dotimes (i 10) (mine/pane/tree:tree-pane-move-down! tp))
    (flet ((render (height)
             (mine/pane/tree:tree-pane-render tp scr (wt:Rect 0 0 30 height) coalton:True
                                              (lambda (path) (declare (ignore path)) coalton:False)
                                              (lambda (path) (declare (ignore path)) 0))
             (loop :for y :below height
                   :collect (string-trim " " (coerce (loop :for x :below 29
                                                           :collect (uiop:symbol-call
                                                                     ':mine-tests/editor-layout
                                                                     ':screen-cell-character scr x y))
                                                     'string)))))
      (%check (not (find "file0.lisp" (render 8) :test #'string=))
              "The short tree unexpectedly showed its first file")
      (let ((rows (render 18)))
        (%check (and (find "file0.lisp" rows :test #'string=)
                     (find "file9.lisp" rows :test #'string=))
                "Growing the tree did not show every row again: ~S" rows)))))

(defun check-project-tree-uses-the-opened-asd-file ()
  (let ((name "mine-project-tree-identity-test")
        (roots (list (%mine-project-test-root) (%mine-project-test-root))))
    (unwind-protect
         (progn
           ;; The editor image registers its own systems, such as mine and
           ;; coalton, as immutable at the paths they were built from.
           (asdf:register-immutable-system name)
           (dolist (root roots)
             (let ((asd (merge-pathnames (format nil "~A.asd" name) root)))
               (ensure-directories-exist root)
               (%write-utf8-file asd (format nil "(asdf:defsystem ~S :components ((:file \"main\")))"
                                             name))
               (%write-utf8-file (merge-pathnames "main.lisp" root) "")
               (let* ((tree (app::%coalton-optional-value-or-nil
                             (mine/project/asdf-parser:parse-asd-to-tree (namestring asd))))
                      (path (and tree (app::%coalton-optional-value-or-nil
                                       (app::%first-file-path tree)))))
                 (%check (and path (uiop:subpathp (pathname path) (truename root)))
                         "The project tree for ~A listed ~S instead of its own file" asd path)))))
      (remhash name asdf::*registered-systems*)
      (remhash name asdf::*preloaded-systems*)
      (when asdf::*immutable-systems*
        (remhash name asdf::*immutable-systems*))
      (dolist (root roots)
        (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))))

(defvar *asd-helper-loads* 0)

(defun check-project-tree-loads-defsystem-dependencies-once ()
  (let* ((root (%mine-project-test-root))
         (helper-dir (merge-pathnames "helper/" root))
         (project-dir (merge-pathnames "project/" root))
         (asd (merge-pathnames "mine-asd-helper-project-test.asd" project-dir))
         (*asd-helper-loads* 0))
    (unwind-protect
         (let ((asdf:*central-registry* (cons helper-dir asdf:*central-registry*)))
           (ensure-directories-exist helper-dir)
           (ensure-directories-exist project-dir)
           (%write-utf8-file
            (merge-pathnames "mine-asd-helper-test.asd" helper-dir)
            "(defpackage #:mine-asd-helper-test (:use #:cl) (:export #:noted-file))
(in-package #:mine-asd-helper-test)
(incf mine-tests::*asd-helper-loads*)
(defclass noted-file (asdf:cl-source-file) ())
(asdf:defsystem \"mine-asd-helper-test\")
")
           ;; The secondary system can be read only once the helper is loaded.
           (%write-utf8-file
            asd
            "(asdf:defsystem \"mine-asd-helper-project-test\"
  :defsystem-depends-on (\"mine-asd-helper-test\")
  :components ((:file \"main\")))

(asdf:defsystem \"mine-asd-helper-project-test/notes\"
  :components ((mine-asd-helper-test:noted-file \"notes\")))
")
           (%write-utf8-file (merge-pathnames "main.lisp" project-dir) "")
           (%write-utf8-file (merge-pathnames "notes.lisp" project-dir) "")
           (dotimes (i 2)
             (let* ((tree (app::%coalton-optional-value-or-nil
                           (mine/project/asdf-parser:parse-asd-to-tree (namestring asd))))
                    (path (and tree (app::%coalton-optional-value-or-nil
                                     (app::%first-file-path tree)))))
               (%check (and path (string= "main.lisp" (file-namestring path)))
                       "Parse ~D of a project tree with a :defsystem-depends-on system listed ~S"
                       (1+ i) path)))
           (%check (= 1 *asd-helper-loads*)
                   "The :defsystem-depends-on system was loaded ~D times for two parses"
                   *asd-helper-loads*)
           (%check (null (asdf:registered-system "mine-asd-helper-project-test"))
                   "Parsing the project tree left the project's system registered"))
      (asdf:clear-system "mine-asd-helper-test")
      (remhash "mine-asd-helper-project-test" asdf::*registered-systems*)
      (remhash "mine-asd-helper-project-test/notes" asdf::*registered-systems*)
      (when (find-package '#:mine-asd-helper-test)
        (delete-package '#:mine-asd-helper-test))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore))))
