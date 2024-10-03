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

(deftest test-location ()
  (let* ((source (source:make-source-string "1234567890"))
         (location-a (source:make-location source '(0 . 3)))
         (location-b (source:make-location source '(4 . 7))))
    (is (source:location< location-a location-b))
    (is (not (source:location< location-b location-a)))
    (is (not (source:location< location-a location-a)))))
