(defpackage #:mine/app/diagnostics
  (:use #:cl)
  (:local-nicknames (#:store #:mine/app/diagnostic-store))
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
   #:forget-all-diagnostic-requests
   #:invalidate-diagnostics-for-file
   #:jump-adjacent-diagnostic
   #:line-diagnostic-spans
   #:render-diagnostic-popup-cl
   #:store-diagnostic
   #:track-diagnostic-request
   #:write-temp-for-compile))

(in-package #:mine/app/diagnostics)

;;; Protocol and rendering adapters around the typed diagnostic store.

(defvar *diagnostic-store* (store:diagnostic-store-new)
  "Typed diagnostics and request lifecycle state for this editor session.")

(defun coalton-optional-value-or-nil (value)
  "Return NIL for Coalton None, otherwise return the wrapped value."
  (unless (coalton-impl/runtime/optional:cl-none-p value)
    (coalton-impl/runtime/optional:unwrap-cl-some value)))

(defun protocol-severity (severity)
  (case severity
    (:error store:DiagnosticError)
    (:warning store:DiagnosticWarning)
    (:style-warning store:DiagnosticStyleWarning)
    (:note store:DiagnosticNote)
    (t store:DiagnosticUnknown)))

(defun severity-keyword (severity)
  (cond ((eq severity store:DiagnosticError) :error)
        ((eq severity store:DiagnosticWarning) :warning)
        ((eq severity store:DiagnosticStyleWarning) :style-warning)
        ((eq severity store:DiagnosticNote) :note)))

(defun protocol-label-kind (kind)
  (case kind
    (:primary store:PrimaryLabel) (:secondary store:SecondaryLabel)
    (:help store:HelpLabel) (t store:UnknownLabel)))

(defun label-kind-keyword (kind)
  (cond ((eq kind store:PrimaryLabel) :primary)
        ((eq kind store:SecondaryLabel) :secondary)
        ((eq kind store:HelpLabel) :help)))

(defun optional-integer (value)
  (if (integerp value) (coalton:Some value) coalton:None))

(defun protocol-offset (value)
  (if (integerp value) (max 0 (min most-positive-fixnum value)) 0))

(defun protocol-string (value &optional (fallback ""))
  (if (stringp value) value fallback))

(defun plist-diagnostic (plist)
  "Decode protocol fields once before crossing into typed storage."
  (let* ((start (protocol-offset (getf plist :start)))
         (end (max start (protocol-offset (getf plist :end))))
         (summary (protocol-string (getf plist :summary))))
    (store:Diagnostic (or (remap-diagnostic-filepath (getf plist :file) (getf plist :request)) "")
                      start end (protocol-severity (getf plist :severity))
                      summary (protocol-string (getf plist :label) summary)
                      (protocol-label-kind (getf plist :label-kind))
                      (optional-integer (getf plist :request))
                      (optional-integer (getf plist :group)))))

(defun diagnostic-plist (diagnostic)
  "Present a diagnostic to legacy rendering code without storing plist state."
  (list :file (store:diagnostic-file diagnostic)
        :start (store:diagnostic-start diagnostic) :end (store:diagnostic-end diagnostic)
        :severity (severity-keyword (store:diagnostic-severity diagnostic))
        :summary (store:diagnostic-summary diagnostic) :label (store:diagnostic-label diagnostic)
        :label-kind (label-kind-keyword (store:diagnostic-label-kind diagnostic))
        :request (coalton-optional-value-or-nil (store:diagnostic-request diagnostic))
        :group (coalton-optional-value-or-nil (store:diagnostic-group diagnostic))))

(defun diagnostics-for-file (filepath)
  (mapcar #'diagnostic-plist
          (store:store-for-file *diagnostic-store* (or (normalize-document-key filepath) ""))))

(defun user-error (st text)
  "Queue a user-facing modal error."
  (coalton/cell:write!
   (mine/app/state:get-pending-error-cell st)
   (coalton:Some text)))

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
      (store:store-clear-file! *diagnostic-store* document-key))))

(defun invalidate-diagnostics-for-file (filepath)
  "Clear FILEPATH diagnostics and mark any older request results as stale."
  (clear-diagnostics-for-file filepath))

(defun clear-all-diagnostics ()
  "Clear every stored diagnostic and reset REPL announcement tracking."
  (store:store-clear-all! *diagnostic-store*))

(defun delete-compile-temporary (file)
  "Remove a private compile input and its default compiled output."
  (ignore-errors (delete-file (compile-file-pathname file)))
  (ignore-errors (delete-file file)))

(defun clear-compile-file-remap ()
  "Discard temporary files not yet attached to a request."
  (dolist (file (store:store-pending-temporary-files *diagnostic-store*))
    (delete-compile-temporary file))
  (store:store-clear-pending-remaps! *diagnostic-store*))

(defun track-diagnostic-request (request-id)
  "Record the current edit generation for REQUEST-ID."
  (store:store-track-request! *diagnostic-store* request-id))

(defun forget-diagnostic-request (request-id)
  "Release a finished request's metadata and private compile files."
  (dolist (file (store:store-request-temporary-files *diagnostic-store* request-id))
    (delete-compile-temporary file))
  (store:store-forget-request! *diagnostic-store* request-id))

(defun forget-all-diagnostic-requests ()
  "Release request metadata and private files when a runtime session ends."
  (dolist (id (store:store-request-ids *diagnostic-store*))
    (forget-diagnostic-request id))
  (clear-compile-file-remap))

(defun diagnostic-announcement-p (plist)
  "Return T if PLIST should produce a one-line REPL announcement."
  (store:store-announcement! *diagnostic-store* (plist-diagnostic plist)))

(defun write-temp-for-compile (text original-path)
  "Write a unique compile input; attach its document mapping to the next request."
  (clear-compile-file-remap)
  (uiop:with-temporary-file (:stream stream :pathname tmp-path
                             :prefix "mine-beam-"
                             :type (pathname-type (pathname original-path))
                             :direction :output :keep t
                             :external-format (coalton-impl/source:source-external-format))
    (write-string text stream)
    (store:store-add-remap! *diagnostic-store*
                            (normalize-document-key (namestring tmp-path))
                            (normalize-document-key original-path))
    (namestring tmp-path)))

(defun diagnostic-severity-rank (severity)
  (store:severity-rank (protocol-severity severity)))

(defun diagnostic-rank-for-file (filepath)
  "Return the worst stored diagnostic severity rank for FILEPATH."
  (store:store-rank-for-file *diagnostic-store* (or (normalize-document-key filepath) "")))

(defun remap-diagnostic-filepath (raw-filepath &optional request-id)
  (let ((document-key (normalize-document-key raw-filepath)))
    (when document-key
      (store:store-remap-file *diagnostic-store* (optional-integer request-id) document-key))))

(defun diagnostic-stale-p (plist)
  "Reject results from an older edit/compile or a finished request."
  (store:store-stale? *diagnostic-store* (plist-diagnostic plist)))

(defun diagnostic-overlaps-range-p (diag-start diag-end range-start range-end)
  (store:overlaps-range? diag-start diag-end range-start range-end))

(defun diagnostic-contains-position-p (diag-start diag-end pos)
  (store:contains-position? diag-start diag-end pos))

(defun store-diagnostic (plist)
  "Decode and store a protocol diagnostic, rejecting stale results."
  (store:store-add! *diagnostic-store* (plist-diagnostic plist)))

(defun diagnostics-for-range (filepath start end)
  "Return diagnostic views overlapping [START, END]."
  (mapcar #'diagnostic-plist
          (store:store-for-range *diagnostic-store* (or (normalize-document-key filepath) "")
                                 start end)))

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
         (notes (diagnostics-for-file document-key)))
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
  (store:label-kind-rank (protocol-label-kind kind)))

(defun diagnostic-better-at-position-p (candidate best)
  (store:better-at-position? (plist-diagnostic candidate) (plist-diagnostic best)))

(defun diagnostic-at-position (filepath position)
  "Return the best diagnostic view covering POSITION in FILEPATH, or NIL."
  (let ((diagnostic (coalton-optional-value-or-nil
                     (store:store-at-position *diagnostic-store*
                                               (or (normalize-document-key filepath) "")
                                               position))))
    (when diagnostic (diagnostic-plist diagnostic))))

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
    (dolist (note (store:store-all *diagnostic-store*))
      (let ((key (list (store:diagnostic-file note)
                       (store:diagnostic-start note)
                       (store:diagnostic-end note))))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push key locations))))
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

(defun jump-adjacent-diagnostic (st direction &optional scope)
  "Jump to the next (>0) or previous (<0) stored diagnostic.
When SCOPE is a Coalton HashMap of document keys, only consider diagnostics in
those files."
  (let* ((bm (mine/app/state:get-bufmgr st))
         (cs (mine/app/state:get-cursor-state st))
         (opt-buf (mine/buffer/manager::bufmgr-current bm))
         (buf (coalton-optional-value-or-nil opt-buf))
         (current-file (and buf (mine/buffer/buffer:buffer-document-key buf)))
         (current-pos (mine/edit/cursor:cursor-position cs))
         (all-locs (all-diagnostic-locations))
         (locations (if scope
                        (remove-if-not
                         (lambda (loc)
                           (let ((document-key (normalize-document-key (first loc))))
                             (and document-key
                                  (mine/app/diagnostic-scope:contains-normalized?
                                   scope
                                   document-key))))
                         all-locs)
                        all-locs))
         (cursor-file current-file)
         (cursor-pos current-pos)
         (attempted (make-hash-table :test 'equal)))
    (cond
      ((null locations)
       (user-error st (if scope "No project diagnostics" "No diagnostics")))
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
             (when (mine/app/navigation:jump-to-document-key st
                                                              (first target)
                                                              (second target))
               (return-from jump-adjacent-diagnostic t))
             (setf cursor-file (first target)
                   cursor-pos (second target)))
       (user-error st "No reachable diagnostics")))))

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
  (let* ((buf (coalton-optional-value-or-nil
               (mine/buffer/manager:bufmgr-current
                (mine/app/state:get-bufmgr st))))
         (filepath (and buf (mine/buffer/buffer:buffer-document-key buf))))
    (when (and (stringp filepath)
               (plusp (length filepath))
               buf)
      (let* ((cs (mine/app/state:get-cursor-state st))
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
                 (anchor-col (mine/app/navigation:completion-anchor-col st nil ""))
                 (anchor-row (mine/app/navigation:completion-anchor-row st nil))
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
