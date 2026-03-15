(defpackage #:coalton-impl/parser/reader
  (:use #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:source #:coalton-impl/source))
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:export
   #:*coalton-eclector-client*
   #:shorthand-binder-symbol
   #:with-reader-context
   #:maybe-read-form
   #:maybe-read-expression-form))

(in-package #:coalton-impl/parser/reader)

(defclass coalton-eclector-client (eclector.concrete-syntax-tree:cst-client)
  ())

(defvar *coalton-eclector-client* (make-instance 'coalton-eclector-client))

;;;; Shorthand functions are recognized in the reader rather than in the
;;;; expression parser. Under Eclector, forms such as `\x.x` arrive as ordinary
;;;; tokens whose first character was escaped, not as a dedicated reader macro.
;;;; We therefore reinterpret tokens of the form `binders.body` in
;;;; `eclector.reader:interpret-token`.
;;;;
;;;; There are two cases:
;;;;   - `\x.x` or `\.0`: the body is in the same token, so we can immediately
;;;;     rewrite to `(FN (X) X)` or `(FN () 0)`.
;;;;   - `\x.(+ x 1)`: the token ends at the dot, so the body is the next reader
;;;;     form. In that case we return a temporary marker carrying the binders and
;;;;     later combine it with the following CST form in
;;;;     `expand-reader-expression-from-list` / `maybe-read-expression-form`.
;;;;
;;;; Doing this in the reader keeps shorthand syntax uniform anywhere an
;;;; expression can appear, allows nested shorthand like `\x.\y.0`, and
;;;; preserves precise CST source spans for later error reporting. It also
;;;; ensures we do not interfere with backslashes in string syntax, since string
;;;; contents are read before token interpretation and never reach this path.
(defstruct (shorthand-function-marker
            (:constructor make-shorthand-function-marker (binders))
            (:copier nil))
  (binders nil :type list :read-only t))

(defun reader-syntax-error (control &rest args)
  (error "~?" control args))

(defun find-reader-symbol (package-name symbol-name)
  (let ((package (find-package package-name)))
    (unless package
      (reader-syntax-error "Coalton reader requires package ~A" package-name))
    (or (find-symbol symbol-name package)
        (reader-syntax-error "Coalton reader requires symbol ~A::~A"
                             package-name
                             symbol-name))))

(defun coalton-symbol (name)
  (find-reader-symbol "COALTON" name))

(defun escaped-token-index-p (escape-ranges index)
  (some (lambda (range)
          (<= (car range) index (1- (cdr range))))
        escape-ranges))

(defun shift-escape-ranges (escape-ranges offset)
  (loop :for (start . end) :in escape-ranges
        :for shifted-start := (- start offset)
        :for shifted-end := (- end offset)
        :when (plusp shifted-end)
          :collect (cons (max 0 shifted-start)
                         shifted-end)))

(defun shorthand-binder-symbol (char)
  (if (char= char #\_)
      (intern "_" *package*)
      (intern (string (char-upcase char)) *package*)))

(defun parse-shorthand-binders (token end)
  (let ((binders nil)
        (seen (make-hash-table :test #'eql)))
    (loop :for index :below end
          :for char := (aref token index)
          :do
             (cond
               ((char= char #\_)
                (push (shorthand-binder-symbol char) binders))
               ((alpha-char-p char)
                (let ((canonical-char (char-upcase char)))
                  (when (gethash canonical-char seen)
                    (reader-syntax-error "Malformed shorthand function: duplicate binder ~C" char))
                  (setf (gethash canonical-char seen) t)
                  (push (shorthand-binder-symbol char) binders)))
               (t
                (return-from parse-shorthand-binders :invalid))))
    (nreverse binders)))

(defun shorthand-function-form (binders body)
  `(,(coalton-symbol "FN") ,binders ,body))

(defun maybe-make-shorthand-function-token (client input-stream token escape-ranges)
  (when (and (plusp (length token))
             (escaped-token-index-p escape-ranges 0))
    (let ((separator
            (loop :for index :below (length token)
                  :when (and (char= (aref token index) #\.)
                             (or (and (zerop index)
                                      (escaped-token-index-p escape-ranges index))
                                 (not (escaped-token-index-p escape-ranges index))))
                    :do (return index))))
      (when separator
        (let ((binders (parse-shorthand-binders token separator)))
          (unless (eq binders :invalid)
            (let ((body-token (subseq token (1+ separator))))
              (if (string= body-token "")
                  (make-shorthand-function-marker binders)
                  (shorthand-function-form
                   binders
                   (eclector.reader:interpret-token client
                                                    input-stream
                                                    body-token
                                                    (shift-escape-ranges escape-ranges (1+ separator))))))))))))

(defmethod eclector.reader:interpret-token
    ((client coalton-eclector-client) input-stream token escape-ranges)
  (or (maybe-make-shorthand-function-token client input-stream token escape-ranges)
      (call-next-method)))

(defun source-span-union (left right)
  (cond
    ((and left right)
     (cons (source:span-start left)
           (source:span-end right)))
    (left
     left)
    (t
     right)))

(defun make-reader-atom-cst (raw &optional span)
  (make-instance 'cst:atom-cst :raw raw :source span))

(defun make-reader-list-cst (elements &optional span)
  (labels ((build (forms)
             (if (endp forms)
                 (make-reader-atom-cst nil)
                 (let* ((first (first forms))
                        (rest (build (rest forms)))
                        (node-span (or span
                                       (source-span-union (cst:source first)
                                                          (cst:source rest)))))
                   (cst:cons first rest :source node-span)))))
    (build elements)))

(defun shorthand-function-marker-cst-p (form)
  (and (cst:atom form)
       (shorthand-function-marker-p (cst:raw form))))

(defun shorthand-function-form-cst (marker body)
  (let* ((marker-span (cst:source marker))
         (body-span (cst:source body))
         (form-span (source-span-union marker-span body-span))
         (fn-name (make-reader-atom-cst (coalton-symbol "FN") marker-span))
         (binders (loop :for binder :in (shorthand-function-marker-binders (cst:raw marker))
                        :collect (make-reader-atom-cst binder marker-span)))
         (binder-list (make-reader-list-cst binders marker-span)))
    (make-reader-list-cst (list fn-name binder-list body) form-span)))

(defun expand-reader-expression-from-list (forms)
  (unless forms
    (reader-syntax-error "Malformed shorthand function: missing body"))
  (let ((form (first forms)))
    (if (shorthand-function-marker-cst-p form)
        (multiple-value-bind (body remaining)
            (expand-reader-expression-from-list (rest forms))
          (values (shorthand-function-form-cst form body) remaining))
        (values (expand-reader-syntax-cst form)
                (rest forms)))))

(defun expand-reader-proper-list (form)
  (loop :with remaining := (cst:listify form)
        :with expanded := nil
        :while remaining
        :do (multiple-value-bind (expr rest)
                (expand-reader-expression-from-list remaining)
              (push expr expanded)
              (setf remaining rest))
        :finally (return (make-reader-list-cst (nreverse expanded)
                                               (cst:source form)))))

(defun expand-reader-syntax-cst (form)
  (cond
    ((cst:atom form)
     form)
    ((cst:proper-list-p form)
     (expand-reader-proper-list form))
    (t
     (cst:cons (expand-reader-syntax-cst (cst:first form))
               (expand-reader-syntax-cst (cst:rest form))
               :source (cst:source form)))))

(defun populate-reader-sources (form &optional inherited-span)
  (let ((span (or (cst:source form) inherited-span)))
    (when (and span
               (null (cst:source form)))
      (setf (cst:source form) span))
    (when (cst:consp form)
      (populate-reader-sources (cst:first form) span)
      (populate-reader-sources (cst:rest form) span))
    form))

(defmacro with-reader-context (stream &rest body)
  "Run the body in the toplevel reader context."
  `(eclector.reader:call-as-top-level-read
    *coalton-eclector-client*
    (lambda ()
      ,@body)
    ,stream
    nil
    'eof
    nil))

(defun maybe-read-form (stream source &optional (eclector-client eclector.base:*client*))
  "Read the next form or return if there is no next form.

Returns (VALUES FORM PRESENTP EOFP)"
  (let ((begin (file-position stream)))
    (handler-case
        (loop :do
          ;; On empty lists report nothing
          (when (eq #\) (peek-char t stream nil))
            (read-char stream)
            (return (values nil nil nil)))

          ;; Otherwise, try to read in the next form
          (multiple-value-call
              (lambda (form type &optional parse-result)

                ;; Return the read form when valid
                (when (eq :object type)
                  (return (values (populate-reader-sources
                                   (expand-reader-syntax-cst
                                    (or parse-result form)))
                                  t
                                  nil)))

                (when (eq :eof type)
                  (return (values nil nil t))))

            (eclector.reader:read-maybe-nothing
             eclector-client
             stream
             nil 'eof)))
      (eclector.reader:unterminated-list ()
        (let ((end (file-position stream)))
          (parse-error "Unterminated form"
                       (source:note (source:make-location source (cons begin end))
                                    "Missing close parenthesis for form starting at offset ~a" begin))))
      (error (condition)
        (let ((end (file-position stream)))
          (parse-error "Reader error"
                       (source:note (source:make-location source (cons begin end))
                                    "reader error: ~a" condition)))))))

(defun maybe-read-expression-form (stream source &optional (eclector-client eclector.base:*client*))
  "Read the next Coalton expression form, combining shorthand lambdas with their bodies.

Returns (VALUES FORM PRESENTP EOFP)"
  (multiple-value-bind (form presentp eofp)
      (maybe-read-form stream source eclector-client)
    (cond
      ((or eofp (not presentp))
       (values form presentp eofp))
      ((shorthand-function-marker-cst-p form)
       (multiple-value-bind (body body-presentp body-eofp)
           (maybe-read-expression-form stream source eclector-client)
         (when (or body-eofp (not body-presentp))
           (parse-error "Malformed shorthand function"
                        (source:note (source:make-location source (cst:source form))
                                     "shorthand function body is missing")))
         (values (shorthand-function-form-cst form body) t nil)))
      (t
       (values form t nil)))))
