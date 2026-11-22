(defpackage #:mine-tests/completion-context
  (:use #:cl)
  (:local-nicknames (#:completion #:mine/app/completion)
                    (#:gap #:mine/buffer/gap))
  (:export #:run-completion-context-tests))

(in-package #:mine-tests/completion-context)

(defun candidates (&rest names)
  (mapcar (lambda (name) (completion:Candidate name "function" "")) names))

(defun check-path-completion (complete text expected &rest candidates)
  (let ((result (funcall complete text)))
    (assert (equal expected (coalton-prelude:fst result)))
    (assert (equal candidates (coalton-prelude:snd result)))))

(defun test-path-completion ()
  (mine-tests::with-test-directory (directory)
    (let ((*default-pathname-defaults* directory))
      (ensure-directories-exist "sample/src-two/")
      (ensure-directories-exist "sample/src-one/")
      (dolist (name '("sample/stuff.txt" "sample/src.ASD"))
        (with-open-file (stream name :direction :output :if-exists :error)))
      (check-path-completion #'completion:complete-directory-path "sample/s" "sample/src-"
                             "src-one/" "src-two/")
      (check-path-completion #'completion:complete-project-path "sample/s" "sample/src"
                             "src-one/" "src-two/" "src.ASD")
      (check-path-completion #'completion:complete-file-path "sample/s" "sample/s"
                             "src-one/" "src-two/" "src.ASD" "stuff.txt")
      (check-path-completion #'completion:complete-directory-path "sample/st" "sample/st")
      (check-path-completion #'completion:complete-file-path "sample/S" "sample/S")
      (check-path-completion #'completion:complete-directory-path "sam" "sample/")
      (check-path-completion #'completion:complete-project-path "sample/src." "sample/src.ASD")
      ;; Reach the fixture through ~/ without creating anything in the user's home.
      (let ((home (user-homedir-pathname)))
        (when (equal (uiop:pathname-root home) (uiop:pathname-root directory))
          (let ((prefix (format nil "~~/~{../~*~}~A"
                                (rest (pathname-directory home))
                                (uiop:unix-namestring (uiop:relativize-pathname-directory directory)))))
            (check-path-completion #'completion:complete-directory-path
                                   (concatenate 'string prefix "sam")
                                   (concatenate 'string prefix "sample/"))))))))

(defun run-completion-context-tests ()
  (test-path-completion)
  (dolist (entry mine-tests/source-context:*completion-cases*)
    (destructuring-bind (text pos expected) entry
      (assert (equal expected (completion:extract-symbol-prefix-input text pos)))
      (assert (equal expected (completion:extract-symbol-prefix (gap:gap-from-string text) pos)))))
  (assert (equal "" (completion:common-prefix nil)))
  (assert (equal "Map" (completion:common-prefix (candidates "Map"))))
  (assert (equal "map" (completion:common-prefix (candidates "map" "mapcar" "mapcan"))))
  (assert (equal "|Foo" (completion:common-prefix (candidates "|FooA|" "|FooB|"))))
  (assert (equal "" (completion:common-prefix (candidates "car" "map"))))
  (assert (equal "" (completion:common-prefix (candidates "map" ""))))
  (let ((cs (completion:completion-new)))
    (completion:completion-open! cs "|Fo" (candidates "|FooA|" "|FooB|" "|fooC|") 0 0)
    (completion:completion-filter! cs "|Fo")
    (completion:completion-cycle-up! cs)
    (assert (equal "|FooB|" (coalton-impl/runtime/optional:unwrap-cl-some
                            (completion:completion-selected-name cs))))
    (completion:completion-filter! cs "|fo")
    (assert (not (completion:completion-active? cs))))
  t)
