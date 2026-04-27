(in-package #:mine-tests)

(defun %write-utf8-file (pathname text)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string text stream)))

(defun %read-utf8-file (pathname)
  (with-open-file (stream pathname :direction :input :external-format :utf-8)
    (let ((text (make-string (file-length stream))))
      (read-sequence text stream)
      text)))

(defun %muffle-warning-if-possible (condition)
  (let ((restart (find-restart 'muffle-warning condition)))
    (when restart
      (invoke-restart restart))))

(defun %collect-compile-file-diagnostics (pathname)
  (let ((diagnostics nil)
        (group 0)
        (fasl nil))
    (labels ((record (condition &key muffle)
               (let ((items (diag:condition-diagnostics condition
                                                        :request 1
                                                        :group group)))
                 (when items
                   (incf group)
                   (setf diagnostics (nconc diagnostics items))))
               (when muffle
                 (%muffle-warning-if-possible condition))))
      (unwind-protect
           (handler-case
               (handler-bind ((sb-ext:compiler-note
                                (lambda (condition)
                                  (record condition)))
                              (sb-c:compiler-error
                                (lambda (condition)
                                  (record condition)))
                              (warning
                                (lambda (condition)
                                  (record condition :muffle t)))
                              (error
                                (lambda (condition)
                                  (record condition))))
                 (setf fasl (compile-file pathname)))
             (sb-c:compiler-error ()
               nil)
             (error ()
               nil))
        (when fasl
          (ignore-errors (delete-file fasl)))))
    diagnostics))

(defun %diagnostic-snippet (diagnostic text)
  (subseq text
          (getf diagnostic ':start)
          (getf diagnostic ':end)))

(defun %find-diagnostic (diagnostics predicate)
  (find-if predicate diagnostics))

(defun %collect-server-messages (thunk)
  (uiop:with-temporary-file (:pathname pathname :keep t)
    (unwind-protect
         (progn
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
             (funcall thunk stream))
           (with-open-file (stream pathname
                                   :direction :input
                                   :element-type '(unsigned-byte 8))
             (loop :for message = (server::read-message stream)
                   :while message
                   :collect message)))
      (ignore-errors (delete-file pathname)))))

(defun %message-diagnostic-plist (message)
  (when (and (consp message)
             (eq (first message) ':notify)
             (consp (second message))
             (eq (first (second message)) ':diagnostic))
    (rest (second message))))

(defun %canonical-namestring (pathspec)
  (let* ((pathname (pathname pathspec))
         (probe (ignore-errors (probe-file pathname))))
    (namestring (or probe pathname))))

(defun %check (condition control &rest arguments)
  (unless condition
    (apply #'error control arguments)))

(defun check-diagnostics-locate-style-warning-spans ()
  (uiop:with-temporary-file (:pathname pathname :type "lisp" :keep t)
    (unwind-protect
         (let* ((text (format nil "(in-package #:cl-user)~%(defun style-warning-target () (unknown-fn 1))~%"))
                (unused (%write-utf8-file pathname text))
                (diagnostics (%collect-compile-file-diagnostics pathname))
                (diagnostic (%find-diagnostic
                             diagnostics
                             (lambda (item)
                               (eq (getf item ':severity) ':style-warning)))))
           (declare (ignore unused))
           (%check diagnostic "Expected a style-warning diagnostic. Diagnostics: ~S" diagnostics)
           (%check (string= (namestring pathname) (getf diagnostic ':file))
                   "Expected diagnostic file ~S, got ~S"
                   (namestring pathname)
                   (getf diagnostic ':file))
           (%check (search "unknown-fn"
                           (%diagnostic-snippet diagnostic text)
                           :test #'char-equal)
                   "Expected the style-warning span to cover UNKNOWN-FN: ~S"
                   diagnostic))
      (ignore-errors (delete-file pathname)))))

(defun check-diagnostics-use-character-offsets-for-unicode-files ()
  (uiop:with-temporary-file (:pathname pathname :type "lisp" :keep t)
    (unwind-protect
         (let* ((text (format nil ";; λ prefix~%(in-package #:cl-user)~%(defun unicode-target () (unknown-fn 1))~%"))
                (unused (%write-utf8-file pathname text))
                (diagnostics (%collect-compile-file-diagnostics pathname))
                (diagnostic (%find-diagnostic
                             diagnostics
                             (lambda (item)
                               (eq (getf item ':severity) ':style-warning)))))
           (declare (ignore unused))
           (%check diagnostic "Expected a style-warning diagnostic")
           (%check (search "unknown-fn"
                           (%diagnostic-snippet diagnostic text)
                           :test #'char-equal)
                   "Expected character offsets, not byte offsets: ~S"
                   diagnostic))
      (ignore-errors (delete-file pathname)))))

(defun check-coalton-source-conditions-expand-to-grouped-diagnostics ()
  (let* ((file (source:make-source-file #P"/tmp/source-warning.ct"))
         (primary (source:make-location file '(2 . 5)))
         (secondary (source:make-location file '(9 . 12)))
         (captured nil))
    (handler-bind ((source:source-warning
                     (lambda (condition)
                       (setf captured condition)
                       (%muffle-warning-if-possible condition))))
      (source:warn "warning summary"
                   (source:note primary "primary label")
                   (source:secondary-note secondary "secondary label")
                   (source:help secondary #'identity "help label")))
    (let ((diagnostics (diag:condition-diagnostics captured :request 5 :group 17)))
      (%check (= 3 (length diagnostics)) "Expected 3 diagnostics, got ~S" diagnostics)
      (%check (every (lambda (item) (eq (getf item ':severity) ':warning)) diagnostics)
              "Expected warning severities: ~S"
              diagnostics)
      (%check (equal '(:primary :secondary :help)
                     (mapcar (lambda (item) (getf item ':label-kind)) diagnostics))
              "Unexpected label kinds: ~S"
              diagnostics)
      (%check (every (lambda (item) (= 17 (getf item ':group))) diagnostics)
              "Expected shared group id 17: ~S"
              diagnostics)
      (%check (every (lambda (item)
                       (string= "/tmp/source-warning.ct" (getf item ':file)))
                     diagnostics)
              "Unexpected files in Coalton diagnostics: ~S"
              diagnostics))))

(defun check-wrapped-coalton-source-conditions-use-note-spans ()
  (let* ((file (source:make-source-file #P"/tmp/source-error.ct"))
         (primary (source:make-location file '(24 . 29)))
         (secondary (source:make-location file '(8 . 15)))
         (wrapped nil))
    (handler-case
        (source:error "type mismatch"
                      (source:note primary "expected String")
                      (source:secondary-note secondary "declared String"))
      (source:source-error (condition)
        (setf wrapped (make-condition 'sb-c:compiler-error :condition condition))))
    (let ((diagnostics (diag:condition-diagnostics wrapped :request 9 :group 23)))
      (%check (= 2 (length diagnostics))
              "Expected wrapped Coalton diagnostics to preserve note spans: ~S"
              diagnostics)
      (%check (equal '(24 8)
                     (mapcar (lambda (item) (getf item ':start)) diagnostics))
              "Expected wrapped Coalton starts, got ~S"
              diagnostics)
      (%check (equal '(29 15)
                     (mapcar (lambda (item) (getf item ':end)) diagnostics))
              "Expected wrapped Coalton ends, got ~S"
              diagnostics)
      (%check (equal '(:primary :secondary)
                     (mapcar (lambda (item) (getf item ':label-kind)) diagnostics))
              "Expected wrapped Coalton label kinds, got ~S"
              diagnostics)
      (%check (every (lambda (item)
                       (eq (getf item ':severity) ':error))
                     diagnostics)
              "Expected wrapped Coalton severities to stay :error: ~S"
              diagnostics))))

(defun check-generic-coalton-toplevel-notes-stay-textual ()
  (uiop:with-temporary-file (:pathname pathname :type "ct" :keep t)
    (unwind-protect
         (let* ((text (format nil "(in-package #:cl-user)~%~%(coalton-toplevel~%  (declare f (U64 -> U64))~%  (define (f x)~%    x))~%"))
                (unused (%write-utf8-file pathname text))
                (start (search "(coalton-toplevel" text))
                (demoted (diag::%low-confidence-coalton-compiler-span-p
                          (namestring pathname)
                          start
                          ':note))
                (not-demoted (diag::%low-confidence-coalton-compiler-span-p
                              (namestring pathname)
                              start
                              ':error)))
           (declare (ignore unused))
           (%check demoted
                   "Expected generic note span on a coalton-toplevel to be treated as low-confidence")
           (%check (not not-demoted)
                   "Did not expect error severity on a coalton-toplevel to be demoted"))
      (ignore-errors (delete-file pathname)))))

(defun check-source-diagnostic-hook-sees-source-error-subclasses ()
  (let* ((file (source:make-source-file #P"/tmp/source-hook.ct"))
         (primary (source:make-location file '(41 . 42)))
         (captured nil))
    (handler-case
        (let ((source:*source-diagnostic-hook*
                (lambda (condition)
                  (setf captured condition))))
          (coalton-impl/typechecker/base:tc-error
           "unknown instance"
           (source:note primary "Unknown instance Num String")))
      (source:source-error ()
        nil))
    (%check captured "Expected the source diagnostic hook to capture a source-error subclass")
    (%check (typep captured 'coalton-impl/typechecker/base:tc-error)
            "Expected the hook to receive the original tc-error, got ~S"
            captured)
    (let ((diagnostics (diag:condition-diagnostics captured :request 13 :group 6)))
      (%check (= 1 (length diagnostics))
              "Expected one diagnostic from the hooked source-error subclass, got ~S"
              diagnostics)
      (%check (= 41 (getf (first diagnostics) ':start))
              "Expected the hooked source-error subclass to preserve the note start: ~S"
              diagnostics)
      (%check (= 42 (getf (first diagnostics) ':end))
              "Expected the hooked source-error subclass to preserve the note end: ~S"
              diagnostics))))

(defun check-reader-errors-produce-point-diagnostics ()
  (uiop:with-temporary-file (:pathname pathname :type "lisp" :keep t)
    (unwind-protect
         (let* ((text (format nil "(defun broken-reader ()~%  (list 1 2~%"))
                (unused (%write-utf8-file pathname text))
                (diagnostics (%collect-compile-file-diagnostics pathname))
                (diagnostic (%find-diagnostic
                             diagnostics
                             (lambda (item)
                               (eq (getf item ':severity) ':error)))))
           (declare (ignore unused))
           (%check diagnostic "Expected a reader error diagnostic")
           (%check (string= (namestring pathname) (getf diagnostic ':file))
                   "Expected reader error file ~S, got ~S"
                   (namestring pathname)
                   (getf diagnostic ':file))
           (%check (= (getf diagnostic ':start) (getf diagnostic ':end))
                   "Expected point diagnostic, got ~S"
                   diagnostic)
           (%check (<= 0 (getf diagnostic ':start) (length text))
                   "Expected reader error start within file, got ~S"
                   diagnostic))
      (ignore-errors (delete-file pathname)))))

(defun check-beam-system-emits-diagnostics-before-return ()
  (let* ((system-name (format nil "mine-beam-test-~A"
                              (string-downcase (symbol-name (gensym)))))
         (dir (merge-pathnames (format nil "~A/" system-name)
                               (uiop:temporary-directory)))
         (source-path (merge-pathnames "main.lisp" dir))
         (asd-path (merge-pathnames (format nil "~A.asd" system-name) dir)))
    (unwind-protect
         (progn
           (ensure-directories-exist source-path)
           (%write-utf8-file
            source-path
            (format nil "(in-package #:cl-user)~%(defun beam-system-target () (unknown-fn 1))~%"))
           (%write-utf8-file
            asd-path
            (format nil "(asdf:defsystem ~S :serial t :components ((:file ~S)))~%"
                    system-name
                    "main"))
           (let* ((messages (%collect-server-messages
                             (lambda (stream)
                               (server::handle-beam-system 7 system-name (namestring asd-path) stream))))
                  (return-message (find-if (lambda (message)
                                             (and (consp message)
                                                 (eq (first message) ':return)))
                                           messages))
                  (diagnostic-message (find-if (lambda (message)
                                                 (let ((plist (%message-diagnostic-plist message)))
                                                   (and plist
                                                        (eq (getf plist ':severity) ':style-warning))))
                                               messages))
                  (diagnostic (and diagnostic-message
                                   (%message-diagnostic-plist diagnostic-message))))
             (%check diagnostic "Expected a Beam System diagnostic notification: ~S" messages)
             (%check return-message "Expected a Beam System return message: ~S" messages)
             (%check (eq ':ok (first (third return-message)))
                     "Expected Beam System success return: ~S"
                     return-message)
             (%check (string= (%canonical-namestring source-path)
                              (%canonical-namestring (getf diagnostic ':file)))
                     "Expected diagnostic file ~S, got ~S"
                     (%canonical-namestring source-path)
                     (getf diagnostic ':file))
             (%check (< (position diagnostic-message messages :test #'equal)
                        (position return-message messages :test #'equal))
                     "Expected diagnostic before return: ~S"
                     messages)))
      (ignore-errors (asdf:clear-system system-name))
      (ignore-errors (uiop:delete-directory-tree dir :validate t)))))

(defun check-beam-system-preserves-coalton-error-spans ()
  (let* ((system-name (format nil "mine-beam-coalton-~A"
                              (string-downcase (symbol-name (gensym)))))
         (package-name (string-upcase system-name))
         (dir (merge-pathnames (format nil "~A/" system-name)
                               (uiop:temporary-directory)))
         (source-path (merge-pathnames "main.ct" dir))
         (asd-path (merge-pathnames (format nil "~A.asd" system-name) dir)))
    (unwind-protect
         (progn
           (ensure-directories-exist source-path)
           (%write-utf8-file
            source-path
            (format nil "(defpackage #:~A~%  (:use #:coalton #:coalton-prelude))~%(in-package #:~A)~%~%(coalton-toplevel~%  (declare f (String -> String))~%  (define (f x)~%    (+ 1 x)))~%"
                    package-name
                    package-name))
           (%write-utf8-file
            asd-path
            (format nil "(asdf:defsystem ~S~%  :depends-on (~S)~%  :defsystem-depends-on (~S)~%  :components ((:ct-file ~S)))~%"
                    system-name
                    "coalton"
                    "coalton-asdf"
                    "main"))
           (let* ((messages (%collect-server-messages
                             (lambda (stream)
                               (server::handle-beam-system 11 system-name (namestring asd-path) stream))))
                  (return-message (find-if (lambda (message)
                                             (and (consp message)
                                                  (eq (first message) ':return)))
                                           messages))
                  (diagnostic-message (find-if (lambda (message)
                                                 (let ((plist (%message-diagnostic-plist message)))
                                                   (and plist
                                                        (eq (getf plist ':severity) ':error))))
                                               messages))
                  (diagnostic (and diagnostic-message
                                   (%message-diagnostic-plist diagnostic-message))))
             (%check diagnostic "Expected a Beam System Coalton diagnostic notification: ~S" messages)
             (%check return-message "Expected a Beam System return message: ~S" messages)
             (%check (eq ':error (first (third return-message)))
                     "Expected Beam System error return: ~S"
                     return-message)
             (%check (string= (%canonical-namestring source-path)
                              (%canonical-namestring (getf diagnostic ':file)))
                     "Expected Coalton diagnostic file ~S, got ~S"
                     (%canonical-namestring source-path)
                     (getf diagnostic ':file))
             (%check (< (getf diagnostic ':start) (getf diagnostic ':end))
                     "Expected a non-empty Coalton diagnostic span, got ~S"
                     diagnostic)
             (%check (string= (getf diagnostic ':summary)
                              (second (third return-message)))
                     "Expected return text to mirror the diagnostic summary: ~S / ~S"
                     diagnostic
                     return-message)))
      (ignore-errors (asdf:clear-system system-name))
      (ignore-errors (uiop:delete-directory-tree dir :validate t)))))
