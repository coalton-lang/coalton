(in-package #:mine/protocol/diagnostics)

;;; A small, local adaptation of Swank's source-path parsing. We only need
;;; character-accurate positions within an already-available source string.

(defun %make-sharpdot-reader (orig-reader)
  (lambda (stream char n)
    (ignore-errors (funcall orig-reader stream char n))))

(defun %make-source-recorder (fn source-map)
  (lambda (stream char)
    (let ((start (1- (file-position stream)))
          (values (multiple-value-list (funcall fn stream char)))
          (end (file-position stream)))
      (when values
        (destructuring-bind (&optional existing-start &rest existing-end)
            (car (gethash (car values) source-map))
          (unless (and existing-start existing-end
                       (<= start existing-start end)
                       (<= start existing-end end))
            (push (cons start end) (gethash (car values) source-map)))))
      (values-list values))))

(defun %make-source-recording-readtable (readtable source-map)
  (let ((rt (copy-readtable readtable)))
    (dotimes (code 128)
      (let ((char (code-char code)))
        (multiple-value-bind (fn non-terminating-p)
            (get-macro-character char rt)
          (when fn
            (set-macro-character char
                                 (%make-source-recorder fn source-map)
                                 non-terminating-p
                                 rt)))))
    (let ((sharpdot (ignore-errors
                      (get-dispatch-macro-character #\# #\. rt))))
      (when sharpdot
        (set-dispatch-macro-character #\# #\.
                                      (%make-sharpdot-reader sharpdot)
                                      rt)))
    rt))

(defun %read-and-record-source-map (stream)
  (let* ((source-map (make-hash-table :test #'eq))
         (*readtable* (%make-source-recording-readtable *readtable* source-map))
         (*read-suppress* nil)
         (start (file-position stream))
         (form (ignore-errors (read stream)))
         (end (file-position stream)))
    (unless (gethash form source-map)
      (push (cons start end) (gethash form source-map)))
    (values form source-map)))

(defun %skip-whitespace (stream)
  (peek-char t stream nil nil))

(defun %skip-toplevel-forms (n stream)
  (let ((*read-suppress* t))
    (dotimes (_ n)
      (read stream))
    (%skip-whitespace stream)))

(defun %read-source-form (n stream)
  (%skip-toplevel-forms n stream)
  (%read-and-record-source-map stream))

(defgeneric %sexp-in-bounds-p (sexp index)
  (:method ((sexp list) index)
    (< index (loop :for tail :on sexp
                   :count t
                   :if (not (listp (cdr tail)))
                     :count t)))
  (:method ((sexp t) index)
    (declare (ignore index))
    nil))

(defgeneric %sexp-ref (sexp index)
  (:method ((sexp list) index)
    (loop :for i :from 0
          :for tail :on sexp
          :when (= i index)
            :return (car tail)
          :if (and (= (1+ i) index)
                   (not (listp (cdr tail))))
            :return (cdr tail))))

(defun %source-path-source-position (path form source-map)
  (let ((forms (loop :for index :in path
                     :for current = form :then (and (%sexp-in-bounds-p current index)
                                                    (%sexp-ref current index))
                     :collect current)))
    (loop :for candidate :in (nreverse forms)
          :for ((start . end) . rest) := (gethash candidate source-map)
          :when (and start end (not rest))
            :return (values start end))))

(defun %source-path-string-position (path string)
  (unless (and (consp path)
               (every #'integerp path))
    (return-from %source-path-string-position nil))
  (with-input-from-string (stream string)
    (destructuring-bind (tlf-number . form-path) path
      (multiple-value-bind (form source-map)
          (%read-source-form tlf-number stream)
        (%source-path-source-position (cons 0 form-path) form source-map)))))

(defun %source-path-file-position (path filename)
  (unless (and (consp path)
               (every #'integerp path))
    (return-from %source-path-file-position nil))
  (let ((buffer nil))
    (with-open-file (stream filename)
      (%skip-toplevel-forms (1+ (first path)) stream)
      (let ((endpos (file-position stream)))
        (setf buffer (make-array (list endpos)
                                 :element-type 'character
                                 :initial-element #\Space))
        (assert (file-position stream 0))
        (read-sequence buffer stream :end endpos)))
    (%source-path-string-position path buffer)))

(defun %safe-string (thing)
  (handler-case
      (string-trim '(#\Space #\Tab #\Newline #\Return)
                   (format nil "~A" thing))
    (error ()
      "[unprintable condition]")))

(defun %real-condition (condition)
  (typecase condition
    (sb-int:encapsulated-condition
     (or (ignore-errors (sb-int:encapsulated-condition condition))
         condition))
    (t condition)))

(defun %normalize-file (file)
  (typecase file
    (pathname (namestring file))
    (string (and (plusp (length file))
                 (not (char= (char file 0) #\<))
                 file))
    (t nil)))

(defun %normalize-severity (condition)
  (typecase condition
    (sb-c:compiler-error :error)
    (sb-c:fatal-compiler-error :error)
    (sb-ext:compiler-note :note)
    (style-warning :style-warning)
    (warning :warning)
    (error :error)
    (t :note)))

(defun %coalton-toplevel-prefix-p (text)
  (and (stringp text)
       (or (and (>= (length text) 18)
                (string-equal "(coalton-toplevel " text :end2 18))
           (and (>= (length text) 17)
                (string-equal "(coalton-toplevel" text :end2 17))
           (and (>= (length text) 9)
                (string-equal "(coalton " text :end2 9))
           (and (>= (length text) 8)
                (string-equal "(coalton" text :end2 8)))))

(defun %source-prefix-at (file start)
  (handler-case
      (with-open-stream
          (stream (coalton-impl/source:source-stream
                   (coalton-impl/source:make-source-file file)))
        (file-position stream start)
        (let* ((buffer (make-string 32))
               (read-count (read-sequence buffer stream)))
          (string-left-trim '(#\Space #\Tab #\Newline #\Return)
                            (subseq buffer 0 read-count))))
    (error ()
      nil)))

(defun %low-confidence-coalton-compiler-span-p (file start severity)
  (and (member severity '(:note :style-warning :warning))
       (stringp (%normalize-file file))
       (integerp start)
       (not (minusp start))
       (%coalton-toplevel-prefix-p (%source-prefix-at file start))))

(defun %map-span (file start end file-override offset-base synthetic-prefix)
  (let* ((target-file (or (%normalize-file file-override)
                          (%normalize-file file)))
         (mapped-start (max 0 (- start synthetic-prefix)))
         (mapped-end (max 0 (- end synthetic-prefix))))
    (cond
      ((<= mapped-end 0)
       (values nil 0 0))
      (t
       (values target-file
               (+ offset-base mapped-start)
               (+ offset-base mapped-end))))))

(defun %compiler-condition-span (condition)
  (declare (ignore condition))
  (let ((context (ignore-errors (sb-c::find-error-context nil))))
    (when context
      (let* ((source-path (ignore-errors
                            (reverse
                             (sb-c::compiler-error-context-original-source-path context))))
             (file (ignore-errors
                     (sb-c::compiler-error-context-file-name context))))
        (when (and (pathnamep file)
                   (consp source-path))
          (multiple-value-bind (start end)
              (%source-path-file-position source-path file)
            (when (and (integerp start) (integerp end))
              (values file start end))))))))

(defun %compiler-context-point-span ()
  (let ((context (ignore-errors (sb-c::find-error-context nil))))
    (when context
      (let ((file (ignore-errors (sb-c::compiler-error-context-file-name context)))
            (pos (ignore-errors (sb-c::compiler-error-context-file-position context))))
        (when (and (pathnamep file)
                   (integerp pos))
          (values file pos pos))))))

(defun %reader-error-span (condition)
  (when (typep condition 'stream-error)
    (let ((stream (ignore-errors (stream-error-stream condition))))
      (when stream
        (let* ((file (ignore-errors (pathname stream)))
               (stream-pos (ignore-errors (file-position stream)))
               (condition-pos (ignore-errors
                                (sb-c::input-error-in-compile-file-position condition)))
               (point (cond
                        ((integerp stream-pos)
                         (max 0 (1- stream-pos)))
                        ((integerp condition-pos)
                         condition-pos))))
          (when (integerp point)
            (let ((point (max 0 point)))
              (values file point point))))))))

(defun %compiler-condition-diagnostics (condition request group file-override offset-base synthetic-prefix)
  (let ((real-condition (%real-condition condition)))
    (multiple-value-bind (file start end)
      (%compiler-condition-span condition)
      (when (not start)
        (multiple-value-setq (file start end)
          (%reader-error-span real-condition)))
      (when (not start)
        (multiple-value-setq (file start end)
          (%compiler-context-point-span)))
      (multiple-value-bind (mapped-file mapped-start mapped-end)
          (if (and start end)
              (%map-span file start end file-override offset-base synthetic-prefix)
              (values (%normalize-file file-override) 0 0))
        (when (%low-confidence-coalton-compiler-span-p mapped-file
                                                       mapped-start
                                                       (%normalize-severity condition))
          (setf mapped-file nil
                mapped-start 0
                mapped-end 0))
        (list (list ':request request
                    ':file mapped-file
                    ':severity (%normalize-severity condition)
                    ':summary (%safe-string condition)
                    ':label (%safe-string condition)
                    ':label-kind ':primary
                    ':start mapped-start
                    ':end mapped-end
                    ':group group))))))

(defun %coalton-source-condition-p (condition)
  (or (typep condition 'coalton-impl/source:source-error)
      (typep condition 'coalton-impl/source:source-warning)))

(defun %coalton-diagnostic-label-kind (note)
  (ecase (coalton-impl/source:note-type note)
    (:primary ':primary)
    (:secondary ':secondary)
    (:help ':help)))

(defun %coalton-diagnostic-severity (condition)
  (ecase (coalton-impl/source:severity condition)
    (:error ':error)
    (:warn ':warning)))

(defun %coalton-condition-diagnostics (condition request group file-override offset-base synthetic-prefix)
  (let ((severity (%coalton-diagnostic-severity condition))
        (summary (coalton-impl/source:message condition)))
    (loop :for note :in (coalton-impl/source:notes condition)
          :for loc := (coalton-impl/source:location note)
          :for span := (coalton-impl/source:location-span loc)
          :collect
          (multiple-value-bind (mapped-file mapped-start mapped-end)
              (%map-span (coalton-impl/source:source-file-path
                          (coalton-impl/source:location-source loc))
                         (coalton-impl/source:span-start span)
                         (coalton-impl/source:span-end span)
                         file-override
                         offset-base
                         synthetic-prefix)
            (list ':request request
                  ':file mapped-file
                  ':severity severity
                  ':summary summary
                  ':label (coalton-impl/source:message note)
                  ':label-kind (%coalton-diagnostic-label-kind note)
                  ':start mapped-start
                  ':end mapped-end
                  ':group group)))))

(defun condition-diagnostics (condition &key request group file-override (offset-base 0) (synthetic-prefix 0))
  "Return CONDITION as a list of normalized diagnostic plists."
  (let* ((real-condition (%real-condition condition))
         (coalton-condition real-condition))
    (cond
      ((%coalton-source-condition-p coalton-condition)
       (%coalton-condition-diagnostics coalton-condition
                                       request
                                       group
                                       file-override
                                       offset-base
                                       synthetic-prefix))
      (t
       (%compiler-condition-diagnostics condition
                                        request
                                        group
                                        file-override
                                        offset-base
                                        synthetic-prefix)))))
