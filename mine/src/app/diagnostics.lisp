(defpackage #:mine/app/diagnostics
  (:use #:cl)
  (:export
   #:clear-all-diagnostics
   #:clear-compile-file-remap
   #:clear-diagnostics-for-file
   #:diagnostic-announcement-p
   #:diagnostic-rank-for-file
   #:diagnostic-severity-for-range
   #:diagnostic-stale-p
   #:diagnostics-message-for-position
   #:diagnostics-message-for-range
   #:forget-diagnostic-request
   #:invalidate-diagnostics-for-file
   #:jump-adjacent-diagnostic
   #:line-diagnostic-spans
   #:render-diagnostic-popup-cl
   #:store-diagnostic
   #:track-diagnostic-request
   #:write-temp-for-compile))

(in-package #:mine/app/diagnostics)

;;; CL diagnostics state referenced by lisp escapes in the Coalton code.

(defvar *diagnostics-table* (make-hash-table :test 'equal)
  "Hash table: document key (string) -> list of diagnostic plists.")
(defvar *reported-diagnostic-groups* (make-hash-table :test 'equal)
  "Hash table of (request . group) pairs already announced in the REPL.")
(defvar *diagnostic-file-generations* (make-hash-table :test 'equal)
  "Hash table: document key (string) -> latest edit generation that invalidated diagnostics.")
(defvar *diagnostic-request-generations* (make-hash-table :test 'eql)
  "Hash table: request id (integer) -> diagnostic generation snapshot at request start.")
(defvar *diagnostic-generation-clock* 0
  "Monotonic counter used to invalidate stale diagnostics after edits.")
(defvar *compile-file-remap* nil
  "When non-nil, a cons (temp-document-key . real-document-key) for remapping file diagnostics.")

(defun mine-function (name)
  "Return the MINE/APP/MINE function named NAME."
  (multiple-value-bind (symbol status)
      (find-symbol name "MINE/APP/MINE")
    (unless (and symbol status)
      (error "Missing mine/app/mine function ~A" name))
    (symbol-function symbol)))

(defun call-mine-function (name &rest args)
  "Call the MINE/APP/MINE function NAME with ARGS."
  (apply (mine-function name) args))

(defun coalton-optional-value-or-nil (value)
  "Return NIL for Coalton None, otherwise return the wrapped value."
  (unless (coalton-impl/runtime/optional:cl-none-p value)
    (coalton-impl/runtime/optional:unwrap-cl-some value)))

(defun buffer-document-key-p (document-key)
  "Return T when DOCUMENT-KEY names an unnamed in-memory buffer."
  (and (stringp document-key)
       (>= (length document-key) 9)
       (string= document-key "buffer://" :end1 9 :end2 9)))

(defun normalize-document-key (document-key)
  "Return the canonical document key string used for diagnostics lookup."
  (cond
    ((not (and (stringp document-key)
               (plusp (length document-key))))
     nil)
    ((buffer-document-key-p document-key)
     document-key)
    (t
     (mine/buffer/buffer::%normalize-file-document-key document-key))))

;;; Diagnostics storage

(defun clear-diagnostics-for-file (filepath)
  "Clear all diagnostics for a document before a new compile."
  (let ((document-key (normalize-document-key filepath)))
    (when document-key
      (remhash document-key *diagnostics-table*))))

(defun invalidate-diagnostics-for-file (filepath)
  "Clear FILEPATH diagnostics and mark any older request results as stale."
  (let ((document-key (normalize-document-key filepath)))
    (when document-key
      (clear-diagnostics-for-file document-key)
      (incf *diagnostic-generation-clock*)
      (setf (gethash document-key *diagnostic-file-generations*)
            *diagnostic-generation-clock*))))

(defun clear-all-diagnostics ()
  "Clear every stored diagnostic and reset REPL announcement tracking."
  (clrhash *diagnostics-table*)
  (clrhash *reported-diagnostic-groups*))

(defun clear-compile-file-remap ()
  "Clear the temporary compile-file remap state."
  (setf *compile-file-remap* nil))

(defun track-diagnostic-request (request-id)
  "Record the current edit generation for REQUEST-ID."
  (setf (gethash request-id *diagnostic-request-generations*)
        *diagnostic-generation-clock*))

(defun forget-diagnostic-request (request-id)
  "Drop REQUEST-ID from the diagnostic request tracking table."
  (remhash request-id *diagnostic-request-generations*))

(defun diagnostic-announcement-p (plist)
  "Return T if PLIST should produce a one-line REPL announcement."
  (let ((request (getf plist :request))
        (group (getf plist :group)))
    (cond
      ((and request group)
       (let ((key (cons request group)))
         (unless (gethash key *reported-diagnostic-groups*)
           (setf (gethash key *reported-diagnostic-groups*) t)
           t)))
      (t
       (eq (getf plist :label-kind) :primary)))))

(defun write-temp-for-compile (text original-path)
  "Write TEXT to a temporary file preserving the extension of ORIGINAL-PATH.
Sets *COMPILE-FILE-REMAP* and returns the temporary file path string."
  (let* ((ext (pathname-type (pathname original-path)))
         (base (pathname-name (pathname original-path)))
         (tmp-path (merge-pathnames
                    (make-pathname :name (format nil "mine-beam-~A" base) :type ext)
                    (uiop:temporary-directory))))
    (with-open-file (s tmp-path :direction :output
                       :if-exists :supersede
                       :external-format :utf-8)
      (write-string text s))
    (setf *compile-file-remap*
          (cons (normalize-document-key (namestring tmp-path))
                (normalize-document-key original-path)))
    (namestring tmp-path)))

(defun diagnostic-severity-rank (severity)
  (cond
    ((eq severity :error) 4)
    ((eq severity :warning) 3)
    ((eq severity :style-warning) 2)
    ((eq severity :note) 1)
    (t 0)))

(defun diagnostic-rank-for-file (filepath)
  "Return the worst stored diagnostic severity rank for FILEPATH."
  (let ((document-key (normalize-document-key filepath))
        (best 0))
    (dolist (note (gethash document-key *diagnostics-table*) best)
      (setf best
            (max best
                 (diagnostic-severity-rank (getf note :severity)))))))

(defun remap-diagnostic-filepath (raw-filepath)
  (let ((document-key (normalize-document-key raw-filepath)))
    (if (and document-key
             *compile-file-remap*
             (string= document-key (car *compile-file-remap*)))
        (cdr *compile-file-remap*)
        document-key)))

(defun diagnostic-stale-p (plist)
  "Return T if PLIST belongs to a request older than the file's latest edit."
  (let* ((request-id (getf plist :request))
         (document-key (remap-diagnostic-filepath (getf plist :file))))
    (when (and request-id document-key)
      (multiple-value-bind (request-generation presentp)
          (gethash request-id *diagnostic-request-generations*)
        (and presentp
             (> (gethash document-key *diagnostic-file-generations* 0)
                request-generation))))))

(defun diagnostic-overlaps-range-p (diag-start diag-end range-start range-end)
  (if (= diag-start diag-end)
      (and (<= range-start diag-start)
           (<= diag-start range-end))
      (and (< diag-start range-end)
           (> diag-end range-start))))

(defun diagnostic-contains-position-p (diag-start diag-end pos)
  (if (= diag-start diag-end)
      (= diag-start pos)
      (and (<= diag-start pos)
           (< pos diag-end))))

(defun store-diagnostic (plist)
  "Store a diagnostic plist from the protocol."
  (let* ((raw-filepath (getf plist :file))
         (document-key (remap-diagnostic-filepath raw-filepath))
         (start (or (getf plist :start) 0))
         (end (or (getf plist :end) 0))
         (severity (getf plist :severity))
         (summary (or (getf plist :summary) ""))
         (label (or (getf plist :label) summary))
         (label-kind (getf plist :label-kind))
         (group (getf plist :group))
         (diagnostic (list :file document-key
                           :start start
                           :end end
                           :severity severity
                           :summary summary
                           :label label
                           :label-kind label-kind
                           :group group)))
    (when (and (stringp document-key)
               (plusp (length document-key)))
      (setf (gethash document-key *diagnostics-table*)
            (nconc (gethash document-key *diagnostics-table*)
                   (list diagnostic))))))

(defun diagnostics-for-range (filepath start end)
  "Return diagnostics for FILEPATH overlapping [START, END]."
  (let* ((document-key (normalize-document-key filepath))
         (notes (gethash document-key *diagnostics-table*)))
    (loop :for note :in notes
          :for diag-start = (getf note :start)
          :for diag-end = (getf note :end)
          :when (diagnostic-overlaps-range-p diag-start diag-end start end)
          :collect note)))

(defun line-diagnostic-spans (filepath line-start line-end)
  "Return line-overlapping diagnostics as (start end severity) triples."
  (loop :for note :in (diagnostics-for-range filepath line-start line-end)
        :collect (list (getf note :start)
                       (getf note :end)
                       (getf note :severity))))

(defun diagnostic-severity-for-range (filepath start end)
  "Return the worst diagnostic severity overlapping [START, END], or NIL."
  (let ((best-severity nil)
        (best-rank 0))
    (dolist (note (diagnostics-for-range filepath start end) best-severity)
      (let* ((severity (getf note :severity))
             (rank (diagnostic-severity-rank severity)))
        (when (> rank best-rank)
          (setf best-rank rank)
          (setf best-severity severity))))))

(defun diagnostics-message-for-position (filepath position)
  "Return the first diagnostic summary covering POSITION, or empty string."
  (let* ((document-key (normalize-document-key filepath))
         (notes (gethash document-key *diagnostics-table*)))
    (or
     (loop :for note :in notes
           :for diag-start = (getf note :start)
           :for diag-end = (getf note :end)
           :when (diagnostic-contains-position-p diag-start diag-end position)
           :return (or (getf note :summary)
                       (getf note :label)
                       ""))
     "")))

(defun diagnostics-message-for-range (filepath start end)
  "Return the first diagnostic summary overlapping [START, END], or empty string."
  (let ((notes (diagnostics-for-range filepath start end)))
    (if notes
        (or (getf (first notes) :summary)
            (getf (first notes) :label)
            "")
        "")))

(defun diagnostic-label-kind-rank (kind)
  (case kind
    (:primary 3)
    (:secondary 2)
    (:help 1)
    (t 0)))

(defun diagnostic-span-length (note)
  (max 0 (- (getf note :end 0)
            (getf note :start 0))))

(defun diagnostic-better-at-position-p (candidate best)
  "Return T if CANDIDATE should win over BEST for a cursor-position popup."
  (let ((candidate-rank (diagnostic-severity-rank (getf candidate :severity)))
        (best-rank (diagnostic-severity-rank (getf best :severity))))
    (cond
      ((> candidate-rank best-rank) t)
      ((< candidate-rank best-rank) nil)
      (t
       (let ((candidate-span (diagnostic-span-length candidate))
             (best-span (diagnostic-span-length best)))
         (cond
           ((< candidate-span best-span) t)
           ((> candidate-span best-span) nil)
           (t
            (> (diagnostic-label-kind-rank (getf candidate :label-kind))
               (diagnostic-label-kind-rank (getf best :label-kind))))))))))

(defun diagnostic-at-position (filepath position)
  "Return the best diagnostic plist covering POSITION in FILEPATH, or NIL."
  (let ((document-key (normalize-document-key filepath))
        (best nil))
    (dolist (note (gethash document-key *diagnostics-table*) best)
      (let ((diag-start (getf note :start))
            (diag-end (getf note :end)))
        (when (diagnostic-contains-position-p diag-start diag-end position)
          (when (or (null best)
                    (diagnostic-better-at-position-p note best))
            (setf best note)))))))

;;; Diagnostics navigation

(defun diagnostic-location< (file-a start-a end-a file-b start-b end-b)
  "Return T if (FILE-A START-A END-A) sorts before (FILE-B START-B END-B)."
  (or (string< file-a file-b)
      (and (string= file-a file-b)
           (or (< start-a start-b)
               (and (= start-a start-b)
                    (< end-a end-b))))))

(defun all-diagnostic-locations ()
  "Return a sorted list of unique diagnostic locations as (FILE START END)."
  (let ((seen (make-hash-table :test 'equal))
        (locations nil))
    (maphash
     (lambda (document-key notes)
       (when (and (stringp document-key)
                  (plusp (length document-key)))
         (dolist (note notes)
           (let* ((start (or (getf note :start) 0))
                  (end (or (getf note :end) start))
                  (key (list document-key start end)))
             (unless (gethash key seen)
               (setf (gethash key seen) t)
               (push key locations))))))
     *diagnostics-table*)
    (sort locations
          (lambda (a b)
            (diagnostic-location< (first a) (second a) (third a)
                                  (first b) (second b) (third b))))))

(defun find-next-diagnostic-location (current-file current-pos locations)
  "Return the next diagnostic location after CURRENT-FILE/CURRENT-POS in LOCATIONS."
  (let ((current-note (and current-file
                           (diagnostic-at-position current-file current-pos))))
    (cond
      ((null locations) nil)
      ((null current-file) (first locations))
      (current-note
       (loop :for entry :in locations
             :when (diagnostic-location< current-file
                                         (getf current-note :start 0)
                                         (getf current-note :end 0)
                                         (first entry)
                                         (second entry)
                                         (third entry))
             :return entry))
      (t
       (loop :for entry :in locations
             :when (or (string< current-file (first entry))
                       (and (string= current-file (first entry))
                            (> (second entry) current-pos)))
             :return entry)))))

(defun find-prev-diagnostic-location (current-file current-pos locations)
  "Return the previous diagnostic location before CURRENT-FILE/CURRENT-POS in LOCATIONS."
  (let ((current-note (and current-file
                           (diagnostic-at-position current-file current-pos)))
        (best nil))
    (cond
      ((null locations) nil)
      ((null current-file) (car (last locations)))
      (current-note
       (dolist (entry locations best)
         (when (diagnostic-location< (first entry)
                                     (second entry)
                                     (third entry)
                                     current-file
                                     (getf current-note :start 0)
                                     (getf current-note :end 0))
           (setf best entry))))
      (t
       (dolist (entry locations best)
         (when (or (string< (first entry) current-file)
                   (and (string= (first entry) current-file)
                        (< (second entry) current-pos)))
           (setf best entry)))))))

(defun normalize-diagnostic-scope (scope)
  "Return a scope hash table keyed by normalized document keys."
  (when scope
    (let ((normalized (make-hash-table :test 'equal)))
      (maphash
       (lambda (key value)
         (let ((document-key (normalize-document-key key)))
           (when document-key
             (setf (gethash document-key normalized) value))))
       scope)
      normalized)))

(defun jump-adjacent-diagnostic (st direction &optional scope)
  "Jump to the next (>0) or previous (<0) stored diagnostic.
When SCOPE is a hash-table of document keys, only consider diagnostics in those files."
  (let* ((bm (call-mine-function "GET-BUFMGR" st))
         (cs (call-mine-function "GET-CURSOR-STATE" st))
         (opt-buf (mine/buffer/manager::bufmgr-current bm))
         (buf (coalton-optional-value-or-nil opt-buf))
         (current-file (and buf (mine/buffer/buffer:buffer-document-key buf)))
         (current-pos (mine/edit/cursor:cursor-position cs))
         (all-locs (all-diagnostic-locations))
         (normalized-scope (normalize-diagnostic-scope scope))
         (locations (if normalized-scope
                        (remove-if-not (lambda (loc) (gethash (first loc) normalized-scope))
                                       all-locs)
                        all-locs))
         (cursor-file current-file)
         (cursor-pos current-pos)
         (attempted (make-hash-table :test 'equal)))
    (cond
      ((null locations)
       (mine/pane/status::statusbar-set-message!
        (call-mine-function "GET-STATUS-BAR" st)
        (if scope "No project diagnostics" "No diagnostics")))
      (t
       (loop
         :repeat (length locations)
         :for target := (if (plusp direction)
                            (or (find-next-diagnostic-location cursor-file cursor-pos locations)
                                (first locations))
                            (or (find-prev-diagnostic-location cursor-file cursor-pos locations)
                                (car (last locations))))
         :do (when (or (null target)
                       (gethash target attempted))
               (loop-finish))
             (setf (gethash target attempted) t)
             (when (call-mine-function "%JUMP-TO-DOCUMENT-KEY" st
                                       (first target)
                                       (second target))
               (return-from jump-adjacent-diagnostic t))
             (setf cursor-file (first target)
                   cursor-pos (second target)))
       (mine/pane/status::statusbar-set-message!
        (call-mine-function "GET-STATUS-BAR" st)
        "No reachable diagnostics")))))

;;; Diagnostics popup

(defun split-popup-line (line width)
  "Wrap LINE into a list of strings no wider than WIDTH characters."
  (let ((width (max 1 width)))
    (if (<= (length line) width)
        (list line)
        (let ((result nil)
              (current "")
              (pos 0)
              (len (length line)))
          (labels
              ((emit-current ()
                 (unless (zerop (length current))
                   (push current result)
                   (setf current "")))
               (emit-word (word)
                 (cond
                   ((zerop (length word))
                    nil)
                   ((zerop (length current))
                    (if (<= (length word) width)
                        (setf current word)
                        (loop :for start :from 0 :below (length word) :by width
                              :do (push (subseq word
                                                start
                                                (min (length word)
                                                     (+ start width)))
                                        result))))
                   ((<= (+ (length current) 1 (length word)) width)
                    (setf current (concatenate 'string current " " word)))
                   (t
                    (emit-current)
                    (emit-word word)))))
            (loop
             (when (>= pos len) (return))
             (loop :while (and (< pos len)
                               (char= (char line pos) #\Space))
                   :do (incf pos))
             (when (>= pos len) (return))
             (let ((word-start pos))
               (loop :while (and (< pos len)
                                 (char/= (char line pos) #\Space))
                     :do (incf pos))
               (emit-word (subseq line word-start pos))))
            (emit-current))
          (nreverse result)))))

(defun wrap-popup-text (text width)
  "Split TEXT by newlines and wrap each line to WIDTH."
  (let ((result nil)
        (start 0)
        (len (length text)))
    (dotimes (i len)
      (when (char= (char text i) #\Newline)
        (setf result
              (nconc result (split-popup-line (subseq text start i) width)))
        (setf start (1+ i))))
    (setf result
          (nconc result (split-popup-line (subseq text start len) width)))
    (or result (list ""))))

(defun truncate-popup-line (line width)
  "Clamp LINE to WIDTH characters, appending ASCII ellipsis when possible."
  (cond
    ((<= (length line) width) line)
    ((<= width 3) (subseq "..." 0 width))
    (t (concatenate 'string
                    (subseq line 0 (- width 3))
                    "..."))))

(defun truncate-popup-lines (lines max-lines width)
  "Clamp LINES to MAX-LINES, truncating the last line if needed."
  (if (<= (length lines) max-lines)
      lines
      (let ((kept (loop :for line :in lines
                        :for idx :from 0
                        :while (< idx max-lines)
                        :collect line)))
        (setf (nth (1- max-lines) kept)
              (truncate-popup-line
               (concatenate 'string
                            (nth (1- max-lines) kept)
                            "...")
               width))
        kept)))

(defun diagnostic-popup-title (severity)
  (case severity
    (:error "Error")
    (:warning "Warning")
    (:style-warning "Style Warning")
    (:note "Note")
    (t "Diagnostic")))

(defun diagnostic-popup-border-color (severity)
  (case severity
    (:error mine/term/color:error-fg)
    (:warning mine/term/color:warning-fg)
    (:style-warning mine/term/color:gold)
    (:note mine/term/color:accent-fg)
    (t mine/term/color:border-fg)))

(defun render-diagnostic-popup-cl (st scr screen-w screen-h)
  "Render a non-modal diagnostic popup near the editor caret."
  (let* ((filepath mine/pane/editor::*editor-filepath*)
         (buf (coalton-optional-value-or-nil
               (mine/buffer/manager:bufmgr-current
                (call-mine-function "GET-BUFMGR" st)))))
    (when (and (stringp filepath)
               (plusp (length filepath))
               buf)
      (let* ((cs (call-mine-function "GET-CURSOR-STATE" st))
             (position (mine/edit/cursor:cursor-position cs))
             (diagnostic (diagnostic-at-position filepath position)))
        (when diagnostic
          (let* ((severity (getf diagnostic :severity))
                 (title (diagnostic-popup-title severity))
                 (summary (or (getf diagnostic :summary) ""))
                 (label (or (getf diagnostic :label) ""))
                 (body-text (cond
                              ((and (plusp (length summary))
                                    (plusp (length label))
                                    (not (string= summary label)))
                               (format nil "~A~%~A" summary label))
                              ((plusp (length summary)) summary)
                              (t label)))
                 (max-content-w (max 18 (min 72 (- screen-w 6))))
                 (max-body-lines (max 2 (min 6 (- screen-h 6))))
                 (wrapped-lines (truncate-popup-lines
                                 (wrap-popup-text body-text max-content-w)
                                 max-body-lines
                                 max-content-w))
                 (title-w (mine/text/width:string-cell-width-cl title))
                 (content-w (max title-w
                                 (loop :for line :in wrapped-lines
                                       :maximize (mine/text/width:string-cell-width-cl line))))
                 (box-w (min (+ content-w 2)
                             (max 10 (- screen-w 2))))
                 (box-h (+ (length wrapped-lines) 3))
                 (anchor-col (call-mine-function "%COMPLETION-ANCHOR-COL" st nil ""))
                 (anchor-row (call-mine-function "%COMPLETION-ANCHOR-ROW" st nil))
                 (top-limit 1)
                 (bottom-limit (max 2 (- screen-h 1)))
                 (below-row (1+ anchor-row))
                 (fits-below (<= (+ below-row box-h) bottom-limit))
                 (box-y (if fits-below
                            below-row
                            (max top-limit (- anchor-row box-h))))
                 (box-x (min (+ anchor-col 1)
                             (max 0 (- screen-w box-w))))
                 (border-style (mine/term/color:Style
                                (diagnostic-popup-border-color severity)
                                mine/term/color:panel-bg
                                mine/term/color:AttrNone))
                 (title-style (mine/term/color:Style
                               (diagnostic-popup-border-color severity)
                               mine/term/color:panel-bg
                               mine/term/color:AttrBold))
                 (body-style (mine/term/color:Style
                              mine/term/color:text-fg
                              mine/term/color:panel-bg
                              mine/term/color:AttrNone))
                 (inner-rect (mine/widget/types:Rect
                              (1+ box-x)
                              (1+ box-y)
                              (- box-w 2)
                              (- box-h 2))))
            (mine/widget/render:draw-box scr
                                         (mine/widget/types:Rect box-x box-y box-w box-h)
                                         border-style)
            (mine/widget/render:fill-rect scr inner-rect #\Space body-style)
            (mine/widget/render:draw-text-clipped scr inner-rect title
                                                  (1+ box-y)
                                                  (1+ box-x)
                                                  title-style)
            (loop :for line :in wrapped-lines
                  :for row :from (+ box-y 2)
                  :do (mine/widget/render:draw-text-clipped scr inner-rect line
                                                            row
                                                            (1+ box-x)
                                                            body-style))))))))
