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
