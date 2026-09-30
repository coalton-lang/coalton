(in-package #:mine-tests)

(defun %test-state ()
  "Construct the real application model without terminal or runtime side effects."
  (mine/app/state:mine-state-new (mine/config/parser:default-config)))

(defun %test-current-buffer (state)
  (app::%coalton-optional-value-or-nil
   (mine/buffer/manager:bufmgr-current (mine/app/state:get-bufmgr state))))

(defmacro with-test-directory ((directory) &body body)
  "Run BODY with a fresh temporary directory and remove only that directory."
  `(let ((,directory
           (merge-pathnames
            (format nil "mine-regression-~A/" (symbol-name (gensym)))
            (uiop:temporary-directory))))
     (ensure-directories-exist ,directory)
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,directory :validate t :if-does-not-exist :ignore))))
