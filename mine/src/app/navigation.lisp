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
            (mine/pane/status::statusbar-set-message!
             (mine/app/state:get-status-bar st)
             "Jump failed: no file path")
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
                      (mine/pane/status::statusbar-set-message!
                       (mine/app/state:get-status-bar st)
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
        (mine/pane/status::statusbar-set-message!
         (mine/app/state:get-status-bar st)
         (format nil "Jump failed: ~A" c))))))

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
             (mine/pane/status::statusbar-set-message!
              (mine/app/state:get-status-bar st)
              "Jump skipped: buffer is no longer open")
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

(defun completion-anchor-col (st is-repl prefix)
  "Compute the screen column for the completion popup anchor."
  (if is-repl
      (let* ((rp (mine/app/state:get-repl-pane st))
             (cursor (mine/pane/repl:repl-pane-input-cursor rp))
             (prompt-len (mine/text/width:string-cell-width-cl
                          (mine/pane/repl:repl-pane-prompt-text rp)))
             (text (mine/pane/repl:repl-pane-get-input rp))
             (text-before (subseq text 0 (min cursor (length text))))
             (vcol (mine/text/width:string-cell-width-cl text-before))
             (content-x (coalton/cell:read (mine/app/state:get-content-x-cell st))))
        (+ content-x prompt-len
           (max 0 (- vcol (length prefix)))))
      (let* ((cs (mine/app/state:get-cursor-state st))
             (buf (coalton-optional-value-or-nil
                   (mine/buffer/manager:bufmgr-current
                    (mine/app/state:get-bufmgr st)))))
        (if buf
            (let* ((gb (mine/buffer/buffer:buffer-gap buf))
                   (ep (mine/app/state:get-editor-pane st))
                   (lc (mine/edit/cursor:cursor-line-col cs gb))
                   (cur-col (coalton-prelude:snd lc))
                   (line (coalton-prelude:fst lc))
                   (line-text (mine/buffer/gap:gap-line-text gb line))
                   (tab-w mine/buffer/gap::*tab-width*)
                   (vcol (mine/buffer/gap:visual-col line-text cur-col tab-w))
                   (sc (mine/pane/editor:editor-pane-scroll-col ep))
                   (gw (mine/pane/editor:editor-pane-gutter-width ep gb))
                   (wrap-w (mine/pane/editor:editor-pane-effective-wrap-width ep))
                   (seg-vcol (mine/pane/editor:wrap-seg-start-vcol
                              gb line cur-col wrap-w tab-w))
                   (origin (+ sc seg-vcol))
                   (eff-x (max 0 (- vcol origin)))
                   (content-x (coalton/cell:read (mine/app/state:get-content-x-cell st))))
              (+ content-x gw (max 0 (- eff-x (length prefix)))))
            0))))

(defun completion-anchor-row (st is-repl)
  "Compute the screen row for the completion popup anchor."
  (if is-repl
      (let* ((rp (mine/app/state:get-repl-pane st))
             (text (mine/pane/repl:repl-pane-get-input rp))
             (cursor (mine/pane/repl:repl-pane-input-cursor rp))
             (cursor-row (count #\Newline text :end (min cursor (length text))))
             (rows (mine/bindings/terminal:terminal-get-size))
             (middle-h (max 1 (- rows 2)))
             (layout (coalton/cell:read
                      (mine/app/state:get-layout-cell st)))
             (repl-full (mine/app/layout:layout-repl-full? layout))
             (repl-h (if repl-full
                         (max 1 (- middle-h 1))
                         (max 3 (floor middle-h 3))))
             (editor-h (if repl-full
                           0
                           (if (> middle-h (+ repl-h 2))
                               (- middle-h (+ repl-h 2))
                               1)))
             (repl-y (if repl-full
                         1
                         (+ 1 editor-h 1)))
             (input-lines (mine/pane/repl:repl-pane-input-line-count rp))
             (max-input-h (max 1 (floor repl-h 2)))
             (input-h (max 1 (min input-lines max-input-h)))
             (output-h (- repl-h input-h))
             (input-y (+ repl-y output-h)))
        (+ input-y cursor-row))
      (let* ((cs (mine/app/state:get-cursor-state st))
             (buf (coalton-optional-value-or-nil
                   (mine/buffer/manager:bufmgr-current
                    (mine/app/state:get-bufmgr st)))))
        (if buf
            (let* ((gb (mine/buffer/buffer:buffer-gap buf))
                   (lc (mine/edit/cursor:cursor-line-col cs gb))
                   (cur-line (coalton-prelude:fst lc))
                   (cur-col (coalton-prelude:snd lc))
                   (ep (mine/app/state:get-editor-pane st))
                   (sr (mine/pane/editor:editor-pane-scroll-row ep))
                   (wrap-w (mine/pane/editor:editor-pane-effective-wrap-width ep))
                   (tab-w mine/buffer/gap::*tab-width*)
                   (row-off (mine/pane/editor:cursor-screen-row-offset
                             gb sr cur-line cur-col wrap-w tab-w)))
              (+ 1 row-off))
            1))))
