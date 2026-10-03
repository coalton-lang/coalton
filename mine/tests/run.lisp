;;; Run with: sbcl --dynamic-space-size 4096 --noinform --non-interactive --load mine/tests/run.lisp
;;; Resolve all project paths from this file, independently of the working directory.
(require ':asdf)

(let* ((tests-directory (uiop:pathname-directory-pathname *load-truename*))
       (mine-directory (truename (merge-pathnames "../" tests-directory)))
       (repository (truename (merge-pathnames "../" mine-directory)))
       (quicklisp-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (unless (find-package "QL")
    (when (probe-file quicklisp-setup)
      (load quicklisp-setup)))
  (pushnew ':coalton-portable-bigfloat *features*)
  (load (merge-pathnames "coalton-config.lisp" mine-directory))
  (asdf:initialize-source-registry
   `(:source-registry (:tree ,repository) :ignore-inherited-configuration))
  (cond
    ((find-package "QL")
     (uiop:symbol-call ':ql ':quickload "mine-tests" :silent t))
    (t
     (asdf:load-system "mine-tests"))))

(handler-case
    (unless (uiop:symbol-call ':mine-tests ':run-mine-tests)
      (uiop:quit 1))
  (error (condition)
    (format *error-output* "~&Mine test failure: ~A~%" condition)
    (uiop:quit 1)))
(format t "~&All mine tests passed.~%")
