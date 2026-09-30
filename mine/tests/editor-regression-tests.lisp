(in-package #:mine-tests)

(defun %editor-result-ok (result)
  (%check (typep result 'coalton-library/classes::result/ok)
          "Expected a successful editor result, got ~S" result)
  (coalton-library/classes::result/ok-_0 result))

(defun %editor-result-error-p (result)
  (typep result 'coalton-library/classes::result/err))

(defun %editor-temp-path ()
  (merge-pathnames (make-pathname :name (format nil "mine-editor-~A" (gensym))
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

(defun run-editor-regression-tests ()
  (dolist (test '(check-editor-file-load-errors-preserve-manager
                  check-editor-document-identity-ignores-file-existence
                  check-editor-open-deduplicates-canonical-paths
                  check-editor-refresh-is-transactional))
    (funcall test))
  t)