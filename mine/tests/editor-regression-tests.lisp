(in-package #:mine-tests)

(defun %editor-result-ok (result)
  (%check (typep result 'coalton-library/classes::result/ok)
          "Expected a successful editor result, got ~S" result)
  (coalton-library/classes::result/ok-_0 result))

(defun %editor-result-error-p (result)
  (typep result 'coalton-library/classes::result/err))

(defun %editor-temp-path ()
  (merge-pathnames (make-pathname :name (format nil "mine-editor-~A-~X" (gensym)
                                              (random most-positive-fixnum (make-random-state t)))
                                :type "ct")
                  (uiop:temporary-directory)))

(defun %editor-write-source (path text)
  (with-open-file (stream path :direction :output :if-exists :supersede
                              :external-format (source:source-external-format))
    (write-string text stream)))

(defun %editor-write-invalid-source (path)
  (with-open-file (stream path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
    (write-byte #xff stream)))

(defun check-editor-file-load-errors-preserve-manager ()
  (let* ((path (%editor-temp-path))
         (name (namestring path))
         (manager (mine/buffer/manager:bufmgr-new)))
    (unwind-protect
         (progn
           (%check (%editor-result-error-p (buf:buffer-from-file (buf:BufferId 0) name))
                   "Missing files must not load as empty successful buffers")
           (%editor-write-invalid-source path)
           (%check (%editor-result-error-p (mine/buffer/manager:bufmgr-open-file! manager name))
                   "Invalid UTF-8 must produce an open error")
           (%check (coalton-impl/runtime/optional:cl-none-p
                    (mine/buffer/manager:bufmgr-find-by-path manager name))
                   "Failed opens must not register a buffer")
           (%editor-write-source path "")
           (let ((buffer (%editor-result-ok
                          (mine/buffer/manager:bufmgr-open-file! manager name))))
             (%check (zerop (gap:gap-length (buf:buffer-gap buffer)))
                     "A genuinely empty file should load successfully")))
      (when (probe-file path) (delete-file path)))))

(defun check-editor-document-identity-ignores-file-existence ()
  ;; Temporary directories are often reached through an alias (/var on macOS,
  ;; 8.3 short names on Windows); a symbolic link reproduces one anywhere else.
  (let* ((base (%editor-temp-path))
         (directory (uiop:ensure-directory-pathname (make-pathname :type nil :defaults base)))
         #-win32
         (alias (make-pathname :type "alias" :defaults base))
         (path (namestring (merge-pathnames "document.ct" directory)))
         (manager (mine/buffer/manager:bufmgr-new)))
    (unwind-protect
         (progn
           (ensure-directories-exist directory)
           #-win32
           (uiop:run-program (list "ln" "-s" (uiop:native-namestring directory)
                                   (uiop:native-namestring alias)))
           (let* ((paths (list path
                               #-win32
                               (namestring (merge-pathnames "document.ct"
                                                            (uiop:ensure-directory-pathname alias)))))
                  (buffer (%editor-result-ok
                           (mine/buffer/manager:bufmgr-open-file! manager (car (last paths)))))
                  (key (buf:buffer-document-key buffer)))
             (flet ((check-identity (stage)
                      (%check (string= key (buf:buffer-document-key buffer))
                              "The document key changed ~A" stage)
                      (dolist (alternative paths)
                        (%check (not (coalton-impl/runtime/optional:cl-none-p
                                      (mine/buffer/manager:bufmgr-find-by-path manager alternative)))
                                "~A did not find the document ~A" alternative stage))))
               (check-identity "before its file existed")
               (%editor-write-source path "saved")
               (check-identity "after its file was created")
               (delete-file path)
               (check-identity "after its file was deleted"))))
      (ignore-errors (delete-file path))
      #-win32 (ignore-errors (delete-file alias))
      (ignore-errors (uiop:delete-empty-directory directory)))))

(defun check-editor-open-deduplicates-canonical-paths ()
  (let* ((path (%editor-temp-path))
         (manager (mine/buffer/manager:bufmgr-new)))
    (unwind-protect
         (progn
           (%editor-write-source path "original")
           (let* ((buffer (%editor-result-ok
                           (mine/buffer/manager:bufmgr-open-file! manager (namestring path))))
                  (*default-pathname-defaults* (uiop:pathname-directory-pathname path))
                  (again (%editor-result-ok
                          (mine/buffer/manager:bufmgr-open-file! manager (file-namestring path)))))
             (%check (eq buffer again) "Relative and absolute opens should reuse the same buffer")
             (buf:buffer-mark-dirty! buffer)
             (%check (mine/buffer/manager:bufmgr-path-dirty? manager (file-namestring path))
                     "Dirty lookup must use the same canonical identity as open")
             #+win32
             (%check (eq buffer (%editor-result-ok
                                 (mine/buffer/manager:bufmgr-open-file!
                                  manager (string-upcase (namestring path)))))
                     "Windows path case must not create a duplicate buffer")))
      (when (probe-file path) (delete-file path)))))

(defun check-editor-refresh-is-transactional ()
  (let* ((path (%editor-temp-path))
         (cs (cursor:cursor-new)))
    (unwind-protect
         (progn
           (%editor-write-source path "original")
           (let* ((buffer (%editor-result-ok (buf:buffer-from-file (buf:BufferId 0) (namestring path))))
                  (gb (buf:buffer-gap buffer)))
             (ops:insert-string! buffer (buf:buffer-undo buffer) cs "edited ")
             (%editor-write-invalid-source path)
             (%check (%editor-result-error-p (buf:buffer-refresh-from-file! buffer))
                     "Refresh should report decoding failures")
             (%check (and (eq gb (buf:buffer-gap buffer))
                          (string= "edited original" (gap:gap-to-string gb))
                          (buf:buffer-dirty? buffer))
                     "Failed refresh must preserve buffer identity, text, and dirty state")
             (%check (not (coalton-impl/runtime/optional:cl-none-p
                           (undo:undo-undo! (buf:buffer-undo buffer))))
                     "Failed refresh must preserve undo history")
             (%editor-write-source path "fresh")
             (%editor-result-ok (buf:buffer-refresh-from-file! buffer))
             (%check (and (eq gb (buf:buffer-gap buffer))
                          (string= "fresh" (gap:gap-to-string gb))
                          (not (buf:buffer-dirty? buffer)))
                     "Successful refresh should preserve gap identity and install clean text")
             (%check (coalton-impl/runtime/optional:cl-none-p (undo:undo-undo! (buf:buffer-undo buffer)))
                     "Refreshing from disk must discard undo operations for the old document")))
      (when (probe-file path) (delete-file path)))))

(defun check-editor-undo-tracks-saved-revision ()
  (let* ((buffer (buf:buffer-new (buf:BufferId 0) "checkpoint"))
         (cs (cursor:cursor-new))
         (tree (buf:buffer-undo buffer))
         (changes 0)
         (buf::*buffer-dirty-hook* (lambda (_key) (declare (ignore _key)) (incf changes))))
    (ops:insert-string! buffer tree cs "a")
    (buf:buffer-mark-clean! buffer)
    (%check (ops:undo! buffer cs) "Expected undo after saving")
    (%check (buf:buffer-dirty? buffer) "Undo away from a saved revision must be dirty")
    (%check (ops:redo! buffer cs) "Expected redo to saved revision")
    (%check (not (buf:buffer-dirty? buffer)) "Redo back to saved revision must be clean")
    (ops:insert-string! buffer tree cs "b")
    (%check (ops:undo! buffer cs) "Expected undo of second edit")
    (%check (not (buf:buffer-dirty? buffer)) "Undo back to saved revision must be clean")
    (ops:replace-range! buffer tree cs 0 1 "a" 1)
    (%check (= changes 5) "Identical replacement must not invalidate diagnostics")
    (%check (ops:redo! buffer cs) "Identical replacement must preserve redo")
    (%check (= changes 6) "Redo must invalidate diagnostics exactly once")
    (ops:undo! buffer cs)
    (ops:undo! buffer cs)
    (ops:insert-string! buffer tree cs "other")
    (%check (buf:buffer-dirty? buffer) "A new branch must not reuse the saved revision")
    (%check (not (ops:redo! buffer cs)) "A real edit should clear redo history")))

(defun check-editor-refresh-normalizes-cursor-units ()
  (with-test-directory (directory)
    (let* ((state (%test-state))
           (path (namestring (merge-pathnames "refresh-units.lisp" directory)))
           (other (namestring (merge-pathnames "other.lisp" directory)))
           (cs (mine/app/state:get-cursor-state state)))
      (%editor-write-source path "abcd")
      (%editor-write-source other "other")
      (app::open-loose-file! state path)
      (cursor:cursor-move-to-position! cs 1)
      (cursor:cursor-start-selection! cs)
      (%editor-write-source path (format nil "~C~Cab" #\Return #\Newline))
      (app::%do-refresh-file! state path)
      (%check (= 2 (cursor:cursor-position cs))
              "Successful refresh left the cursor between CR and LF")
      (%check (coalton-impl/runtime/optional:cl-none-p (cursor:cursor-selection-anchor cs))
              "Successful refresh retained its old selection")
      ;; An inactive document is normalized when its saved view is restored.
      (%editor-write-source path "abcd")
      (app::%do-refresh-file! state path)
      (cursor:cursor-move-to-position! cs 1)
      (cursor:cursor-start-selection! cs)
      (cursor:cursor-move-to-position! cs 4)
      (app::open-loose-file! state other)
      (%editor-write-source path (format nil "~C~C" #\Return #\Newline))
      (app::%do-refresh-file! state path)
      (app::open-loose-file! state path)
      (%check (= 2 (cursor:cursor-position cs))
              "Restoring a refreshed document failed to clamp its saved cursor")
      (%check (= 2 (coalton-impl/runtime/optional:unwrap-cl-some
                    (cursor:cursor-selection-anchor cs)))
              "Restoring a refreshed document left its saved anchor inside CRLF"))))

(defun check-editor-nested-replacements-form-one-undo-step ()
  (let* ((buffer (buf:buffer-new (buf:BufferId 0) "group"))
         (cs (cursor:cursor-new))
         (tree (buf:buffer-undo buffer)))
    (undo:undo-begin-group! tree 0)
    (ops:insert-string! buffer tree cs "a")
    (ops:insert-string! buffer tree cs "b")
    (undo:undo-end-group! tree 2)
    (%check (ops:undo! buffer cs) "Expected grouped undo")
    (%check (and (string= "" (gap:gap-to-string (buf:buffer-gap buffer)))
                 (= 0 (cursor:cursor-position cs))
                 (not (buf:buffer-dirty? buffer)))
            "Nested edit groups should undo completely to the original clean state")
    (%check (not (ops:undo! buffer cs)) "Nested group should occupy just one undo entry")
    (%check (ops:redo! buffer cs) "Expected grouped redo")
    (%check (string= "ab" (gap:gap-to-string (buf:buffer-gap buffer)))
            "Grouped redo should restore both insertions")))

(defun check-editor-replacement-preserves-crlf-units ()
  (let* ((buffer (buf:buffer-new (buf:BufferId 0) "crlf"))
         (gb (buf:buffer-gap buffer))
         (cs (cursor:cursor-new))
         (text (format nil "a~C~Cb" #\Return #\Newline)))
    (gap:gap-insert-string! gb 0 text)
    (cursor:cursor-move-to-buffer-position! gb cs 2)
    (%check (= 3 (cursor:cursor-position cs)) "Direct cursor positioning must not split CRLF")
    (ops:replace-range! buffer (buf:buffer-undo buffer) cs 2 3 "-" 2)
    (%check (string= "a-b" (gap:gap-to-string gb)) "Replacement should expand a partial CRLF range")
    (ops:undo! buffer cs)
    (%check (and (string= text (gap:gap-to-string gb)) (= 3 (cursor:cursor-position cs)))
            "Undo should restore the CRLF and original editor-unit cursor")))

(defun check-editor-page-motion-preserves-visual-column ()
  (let* ((gb (gap:gap-from-string (format nil "abcde~%~CXYZ~%abcde" #\Tab)))
         (cs (cursor:cursor-new)))
    (cursor:cursor-move-to-buffer-position! gb cs 4)
    (cursor:cursor-move-page-down! gb cs 1)
    (%check (= 7 (cursor:cursor-position cs)) "PageDown should target visual column 4 after the tab")
    (cursor:cursor-move-page-down! gb cs 1)
    (%check (= 15 (cursor:cursor-position cs)) "PageDown should retain the sticky visual column")
    (cursor:cursor-move-page-up! gb cs 2)
    (%check (= 4 (cursor:cursor-position cs)) "PageUp should restore the same visual column"))
  (let* ((gb (gap:gap-from-string (format nil "界ab~%abcde")))
         (cs (cursor:cursor-new)))
    (cursor:cursor-move-to-buffer-position! gb cs 2)
    (cursor:cursor-move-page-down! gb cs 1)
    (%check (= 7 (cursor:cursor-position cs)) "PageDown should account for wide characters")))

(defun run-editor-regression-tests ()
  (dolist (test '(check-editor-file-load-errors-preserve-manager
                  check-editor-document-identity-ignores-file-existence
                  check-editor-open-deduplicates-canonical-paths
                  check-editor-refresh-is-transactional
                  check-editor-refresh-normalizes-cursor-units
                  check-editor-undo-tracks-saved-revision
                  check-editor-nested-replacements-form-one-undo-step
                  check-editor-replacement-preserves-crlf-units
                  check-editor-page-motion-preserves-visual-column))
    (funcall test))
  t)
