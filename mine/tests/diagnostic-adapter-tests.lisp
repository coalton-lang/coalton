(defpackage #:mine-tests/diagnostic-adapter
  (:use #:cl)
  (:local-nicknames (#:diagnostics #:mine/app/diagnostics)
                    (#:store #:mine/app/diagnostic-store))
  (:export #:run-diagnostic-adapter-tests))

(in-package #:mine-tests/diagnostic-adapter)

(defun run-diagnostic-adapter-tests ()
  (let ((diagnostics::*diagnostic-store* (store:diagnostic-store-new))
        (diagnostics::*compile-file-remap* nil))
    (diagnostics:clear-diagnostics-for-file "buffer://1")
    (diagnostics:track-diagnostic-request 1)
    (let ((old (list :file "buffer://1" :start 2 :end 6 :severity :warning
                      :request 1 :group 0 :label-kind :primary :summary "old")))
      (assert (diagnostics:store-diagnostic old))
      (assert (diagnostics:diagnostic-announcement-p old))
      (assert (not (diagnostics:diagnostic-announcement-p old)))
      (diagnostics:clear-diagnostics-for-file "buffer://1")
      (diagnostics:track-diagnostic-request 2)
      (assert (diagnostics:diagnostic-stale-p old))
      (assert (not (diagnostics:store-diagnostic old)))
      (assert (not (diagnostics:diagnostic-announcement-p old)))
      (diagnostics:forget-diagnostic-request 1))
    (diagnostics:store-diagnostic
     (list :file "buffer://1" :start 2 :end 4 :severity :error
           :request 2 :group 0 :label-kind :primary :summary "new"))
    (assert (= 4 (diagnostics:diagnostic-rank-for-file "buffer://1")))
    (assert (equal '((2 4 :error)) (diagnostics:line-diagnostic-spans "buffer://1" 0 10)))
    (assert (equal "new" (diagnostics:diagnostics-message-for-position "buffer://1" 2)))
    (assert (equal '(("buffer://1" 2 4)) (diagnostics::all-diagnostic-locations)))
    (diagnostics:forget-diagnostic-request 2)
    (assert (zerop (store:store-announcement-count diagnostics::*diagnostic-store*)))
    (diagnostics:invalidate-diagnostics-for-file "buffer://1")
    (assert (zerop (diagnostics:diagnostic-rank-for-file "buffer://1")))
    ;; Invalid protocol spans are normalized before entering unsigned fields.
    (assert (diagnostics:store-diagnostic
             (list :file "buffer://2" :start -2 :end -5 :summary "point" :severity :note)))
    (assert (equal '((0 0 :note)) (diagnostics:line-diagnostic-spans "buffer://2" 0 1)))
    (diagnostics:clear-all-diagnostics))
  t)
