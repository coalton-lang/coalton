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
