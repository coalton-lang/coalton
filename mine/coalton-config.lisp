;;; coalton-config.lisp -- Coalton compiler settings for building mine.
;;;
;;; Loaded before (asdf:load-system "mine") in both Makefile and CI.

(let ((config '((:compiler-mode              "development")
                (:print-unicode              t)
                (:perform-specialization     t)
                (:perform-heuristic-inlining nil)
                (:emit-type-annotations      t)
                (:print-types                t)
                (:print-rewrites             nil)
                (:auto-continue-redefinition t))))
  (setf (symbol-plist ':coalton-config) nil)
  (loop :for (key value) :in config
        :do (setf (get ':coalton-config key) value)))
