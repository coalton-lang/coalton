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
            (format nil "mine-regression-~A-~X/" (symbol-name (gensym))
                    (random most-positive-fixnum (make-random-state t)))
            (uiop:temporary-directory))))
     (ensure-directories-exist ,directory)
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,directory :validate t :if-does-not-exist :ignore))))

(defun %test-terminal (cols rows)
  "Construct an offscreen terminal for rendering the application."
  (mine/term/terminal:Terminal (mine/term/screen:screen-new cols rows)
                               (coalton/cell:new coalton:False)
                               (mine/term/terminal::%terminal-input-runtime-new)
                               (coalton/cell:new (coalton/vector:new))
                               (coalton/cell:new cols)
                               (coalton/cell:new rows)))
