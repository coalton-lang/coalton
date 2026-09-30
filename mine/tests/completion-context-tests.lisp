(defpackage #:mine-tests/completion-context
  (:use #:cl)
  (:local-nicknames (#:completion #:mine/app/completion)
                    (#:gap #:mine/buffer/gap))
  (:export #:run-completion-context-tests))

(in-package #:mine-tests/completion-context)

(defun candidates (&rest names)
  (mapcar (lambda (name) (completion:Candidate name "function" "")) names))

(defun run-completion-context-tests ()
  (dolist (entry mine-tests/source-context:*completion-cases*)
    (destructuring-bind (text pos expected) entry
      (assert (equal expected (completion:extract-symbol-prefix-input text pos)))
      (assert (equal expected (completion:extract-symbol-prefix (gap:gap-from-string text) pos)))))
  (assert (equal "" (completion:common-prefix nil)))
  (assert (equal "Map" (completion:common-prefix (candidates "Map"))))
  (assert (equal "map" (completion:common-prefix (candidates "Map" "MAPCAR" "mapcan"))))
  (assert (equal "" (completion:common-prefix (candidates "car" "map"))))
  (assert (equal "" (completion:common-prefix (candidates "map" ""))))
  t)
