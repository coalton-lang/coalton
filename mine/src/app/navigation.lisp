(defpackage #:mine/app/navigation
  (:use #:cl)
  (:export
   #:completion-anchor-col
   #:completion-anchor-row
   #:jump-to-document-key
   #:jump-to-file))

(in-package #:mine/app/navigation)

(defun coalton-optional-value-or-nil (value)
  "Return NIL for Coalton None, otherwise return the wrapped value."
  (if (coalton-impl/runtime/optional:cl-none-p value)
      nil
      (coalton-impl/runtime/optional:unwrap-cl-some value)))

(defun user-error (st text)
  "Queue a user-facing modal error."
  (coalton/cell:write!
   (mine/app/state:get-pending-error-cell st)
   (coalton:Some text)))

(defun jump-to-file (st filepath char-offset)
  "Open FILEPATH in the editor and move cursor to CHAR-OFFSET."
  (labels
      ((display-name (path)
         (handler-case
             (file-namestring (pathname path))
           (error () path))))
    (handler-case
        (let* ((raw-path (and filepath (princ-to-string filepath)))
               (resolved-path
                 (or (ignore-errors
                       (when raw-path
                         (let ((truename (probe-file raw-path)))
                           (when truename
                             (namestring truename)))))
                     raw-path))
               (document-key (and resolved-path
                                  (mine/buffer/buffer::%normalize-file-document-key
                                   resolved-path))))
          (unless (and (stringp resolved-path)
                       (plusp (length resolved-path)))
            (user-error st "Jump failed: no file path")
            (return-from jump-to-file nil))
          (let* ((bm (mine/app/state:get-bufmgr st))
                 (ep (mine/app/state:get-editor-pane st))
                 (cs (mine/app/state:get-cursor-state st))
                 (existing-buf
                   (coalton-optional-value-or-nil
                    (mine/buffer/manager::bufmgr-find-by-document-key
                     bm document-key)))
                 (opened-buf nil))
            (unless existing-buf
              (let ((buf-result (mine/buffer/manager::bufmgr-open-file! bm resolved-path)))
                (if (typep buf-result 'coalton-library/classes::result/ok)
                    (setf opened-buf
                          (coalton-library/classes::result/ok-_0 buf-result))
                    (progn
                      (user-error st
                                  (format nil "Jump failed: could not open ~A"
                                          (display-name resolved-path)))
                      (return-from jump-to-file nil)))))
            (let* ((buf (or existing-buf opened-buf))
                   (gb (mine/buffer/buffer::buffer-gap buf))
                   (bid (mine/buffer/buffer::buffer-id buf))
                   (safe-offset
                     (max 0
                          (min (or char-offset 0)
                               (mine/buffer/gap::gap-length gb)))))
              (mine/buffer/manager::bufmgr-switch! bm bid)
              (mine/pane/editor::editor-pane-set-buffer! ep bid)
              (mine/edit/cursor::cursor-move-to-position! cs safe-offset)
              (mine/app/layout:show-editor! st)
              (mine/pane/status::statusbar-set-message!
               (mine/app/state:get-status-bar st)
               (format nil "Jumped to ~A" (display-name resolved-path)))
              t)))
      (error (c)
        (user-error st (format nil "Jump failed: ~A" c))))))

(defun jump-to-document-key (st document-key char-offset)
  "Jump to DOCUMENT-KEY, which may name either a file or an open unnamed buffer."
  (cond
    ((and (stringp document-key)
          (>= (length document-key) 9)
          (string= document-key "buffer://" :end1 9 :end2 9))
     (let* ((bm (mine/app/state:get-bufmgr st))
            (ep (mine/app/state:get-editor-pane st))
            (cs (mine/app/state:get-cursor-state st))
            (buf (coalton-optional-value-or-nil
                  (mine/buffer/manager::bufmgr-find-by-document-key bm document-key))))
       (if (null buf)
           (progn
             (user-error st "Jump skipped: buffer is no longer open")
             nil)
           (let* ((gb (mine/buffer/buffer::buffer-gap buf))
                  (bid (mine/buffer/buffer::buffer-id buf))
                  (safe-offset
                    (max 0
                         (min (or char-offset 0)
                              (mine/buffer/gap::gap-length gb)))))
             (mine/buffer/manager::bufmgr-switch! bm bid)
             (mine/pane/editor::editor-pane-set-buffer! ep bid)
             (mine/edit/cursor::cursor-move-to-position! cs safe-offset)
             (mine/app/layout:show-editor! st)
             (mine/pane/status::statusbar-set-message!
              (mine/app/state:get-status-bar st)
              (format nil "Jumped to ~A"
                      (mine/buffer/buffer::buffer-name buf)))
             t))))
    (t
     (jump-to-file st document-key char-offset))))

(defun %completion-anchor (st is-repl prefix)
  "Read native terminal dimensions at the boundary; layout policies are Coalton."
  (multiple-value-bind (rows cols) (mine/bindings/terminal:terminal-get-size)
    (coalton-optional-value-or-nil
     (mine/app/layout:completion-anchor st is-repl prefix cols rows))))

(defun completion-anchor-col (st is-repl prefix)
  (let ((anchor (%completion-anchor st is-repl prefix)))
    (if anchor (coalton-prelude:fst anchor) 0)))

(defun completion-anchor-row (st is-repl)
  (let ((anchor (%completion-anchor st is-repl "")))
    (if anchor (coalton-prelude:snd anchor) 0)))