(in-package #:coalton-tests)

;;; Check that wrapping a character input stream with
;;; char-position-stream class allows callers to collect character
;;; offset using 'file-position'. This is for gathering source offsets
;;; that remain compatible with the offsets reported for source parsed
;;; from internal strings.

(deftest test-char-position-stream ()
  (flet ((stream-contents (stream)
           (loop :for char
                   := (read-char stream nil nil)
                 :while char
                 :collect (cons char (file-position stream)))))
    (with-open-file (stream (test-file "tests/test-files/unicode.coal")
                            :direction ':input
                            :element-type 'character
                            :external-format :utf-8)
      (let* ((char-stream (make-instance 'source:char-position-stream :stream stream))
             (chars (stream-contents char-stream)))
        (is (= 86 (length chars))
            (format nil "File expected length 86 != ~A" (length chars)))
        (is (equal 72                 ; byte offset would have been 76
                   (cdr (nth 71 chars)))
            "Second kanji is at char offset, not byte offset")))))

(deftest test-reader-error-reports-character-offsets ()
  "A reader error in a file with multibyte characters must report character offsets.

FILE-POSITION on the stream COMPILE-FILE reads source from counts bytes, so the
offsets have to be converted before they can be used as character offsets."
  (uiop:with-temporary-file (:stream out :pathname path :type "ct"
                             :direction :output
                             :external-format :utf-8)
    (write-string (format nil ";; ~A~%(declare x Char)~%(define x #\\Delete)~%"
                          "中文中文中文中文中文中文中文中文中文中文")
                  out)
    :close-stream
    (let* ((text (with-open-file (in path :external-format :utf-8)
                   (let (chars)
                     (loop :for char := (read-char in nil nil)
                           :while char
                           :do (push char chars))
                     (coerce (nreverse chars) 'string))))
           (token "#\\Delete")
           (token-end (+ (search token text) (length token)))
           (source (source:make-source-file path))
           (span nil))
      (handler-case
          (with-open-stream (stream (source:source-stream source))
            (loop :do (multiple-value-bind (form presentp)
                          (parser:maybe-read-form stream source)
                        (declare (ignore form))
                        (unless presentp (return)))))
        (error (condition)
          (setf span (source:location-span
                      (source:location (first (source:notes condition)))))))
      (is (not (null span)) "Expected reading #\\Delete to signal a reader error")
      (when span
        (is (= token-end (cdr span))
            "Reader error end should be the character offset just past ~A, got ~S"
            token span)
        (is (char= #\( (char text (car span)))
            "Reader error span should begin at the offending form's open paren, got ~S"
            span)))))

(deftest test-source-stream-preserves-crlf-characters ()
  (flet ((stream-contents (stream)
           (loop :for char := (read-char stream nil nil)
                 :while char
                 :collect (cons char (file-position stream)))))
    (uiop:with-temporary-file (:stream out
                               :pathname path
                               :type "coal"
                               :direction :output
                               :element-type '(unsigned-byte 8))
      (write-sequence
       (make-array 4
         :element-type '(unsigned-byte 8)
         :initial-contents '(97 13 10 98))
       out)
      :close-stream
      (let* ((source (source:make-source-file path))
             (char-stream (source:source-stream source))
             (chars (stream-contents char-stream)))
        (unwind-protect
             (progn
               (is (equal (list (cons #\a 1)
                                (cons #\Return 2)
                                (cons #\Newline 3)
                                (cons #\b 4))
                          chars)
                   "Source streams should preserve CRLF as raw CR then LF")
               (is (equal '(0 3)
                          (source::find-line-offsets char-stream))
                   "Line offsets should treat only LF as a line break"))
          (close char-stream))))))

(deftest test-source-line-display-omits-only-crlf-return ()
  (uiop:with-temporary-file (:stream out
                             :pathname path
                             :type "coal"
                             :direction :output
                             :element-type '(unsigned-byte 8))
    (write-sequence
     (make-array 6
       :element-type '(unsigned-byte 8)
       :initial-contents '(97 13 10 98 13 99))
     out)
    :close-stream
    (let* ((source (source:make-source-file path))
           (char-stream (source:source-stream source)))
      (unwind-protect
           (progn
             (is (string= "a" (source::read-source-line char-stream))
                 "Displayed CRLF lines should omit the CR terminator")
             (is (string= (concatenate 'string "b" (string #\Return) "c")
                          (source::read-source-line char-stream))
                 "Displayed lines should preserve bare CR characters"))
        (close char-stream)))))

(deftest test-location ()
  (let* ((source (source:make-source-string "1234567890"))
         (location-a (source:make-location source '(0 . 3)))
         (location-b (source:make-location source '(4 . 7))))
    (is (source:location< location-a location-b))
    (is (not (source:location< location-b location-a)))
    (is (not (source:location< location-a location-a)))))

(deftest test-byte-offset-conversion-follows-external-format ()
  "Byte offsets convert under the declared source external format.

The file holds the octets 97 233 98, which are valid latin-1 but not valid
UTF-8: converting them as UTF-8 signals a decoding error on the 233 octet."
  (uiop:with-temporary-file (:stream out
                             :pathname path
                             :type "coal"
                             :direction :output
                             :element-type '(unsigned-byte 8))
    (write-sequence
     (make-array 3
       :element-type '(unsigned-byte 8)
       :initial-contents '(97 233 98))
     out)
    :close-stream
    (let ((source::*source-external-format* :latin-1))
      (is (= 0 (source:file-byte-offset-to-char-offset path 0)))
      (is (= 3 (source:file-byte-offset-to-char-offset path 3))
          "Three octets are three latin-1 characters")
      (is (= 2 (source:file-byte-offset-to-char-offset path 2))
          "The 233 octet is one latin-1 character, not two")
      (is (= 3 (source:file-byte-offset-to-char-offset path 10))
          "A byte offset past the end of the file converts to the character count"))))

(deftest test-stream-span-to-char-span-converts-both-endpoints ()
  "Both endpoints of a span convert in one pass over a multibyte file.

The file holds a (1 byte), é (2 bytes), 中 (3 bytes), and b (1 byte), so the
byte span 1 to 6 covers é and 中, characters 1 to 3."
  (uiop:with-temporary-file (:stream out
                             :pathname path
                             :type "coal"
                             :direction :output
                             :external-format :utf-8)
    (write-string "aé中b" out)
    :close-stream
    (let ((source (source:make-source-file path)))
      (with-open-file (stream path
                              :direction :input
                              :element-type 'character
                              :external-format :utf-8)
        (is (equal '(0 . 4)
                   (source:stream-span-to-char-span stream source (cons 0 7)))
            "Seven bytes are four characters")
        (is (equal '(1 . 3)
                   (source:stream-span-to-char-span stream source (cons 1 6)))
            "A span that starts and ends inside multibyte characters")
        (is (equal (cons (source:file-byte-offset-to-char-offset path 1)
                         (source:file-byte-offset-to-char-offset path 6))
                   (source:stream-span-to-char-span stream source (cons 1 6)))
            "Converting a whole span agrees with converting each endpoint alone")))))

(deftest test-char-position-stream-seek-past-eof-fails ()
  "A seek past the end of the file fails and leaves the true end position."
  (uiop:with-temporary-file (:stream out
                             :pathname path
                             :type "coal"
                             :direction :output
                             :external-format :utf-8)
    (write-string "abc" out)
    :close-stream
    (let ((source (source:make-source-file path))
          (stream nil))
      (unwind-protect
           (progn
             (setf stream (source:source-stream source))
             (is (= 3 (file-position stream 3))
                 "A seek to the end of the file succeeds and reports the position")
             (is (null (file-position stream 10))
                 "A seek past the end of the file fails")
             (is (= 3 (file-position stream))
                 "A failed seek leaves the true end position")
             (is (null (read-char stream nil nil))
                 "The stream reads end of file after a failed seek")
             (is (= 1 (file-position stream 1))
                 "An in-range seek succeeds and reports the requested position")
             (is (char= #\b (read-char stream nil nil))
                 "Reading after an in-range seek resumes at the requested position"))
        (when stream
          (close stream))))))
