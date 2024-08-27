(in-package #:coalton-tests)

(deftest test-source-file ()
  (uiop:with-temporary-file (:stream stream
                             :pathname file
                             :suffix "txt"
                             :direction :output)
    (write-string "ABCDE
FGHIJ
KLMNO
PQRST
UVWXY" stream)
    :close-stream
    (let* ((source (source:make-source-file file :name "file"))
           (msg (with-output-to-string (stream)
                  (source:report-source-condition
                   (make-instance 'source:source-error
                     :message "message"
                     :notes (list (source:make-note (source:make-location source '(13 . 16))
                                                    "LMN")))
                   stream))))
      msg)
    (let* ((source (source:make-source-file file :name "file" :offset 6))
           (msg (with-output-to-string (stream)
                  (source:report-source-condition
                   (make-instance 'source:source-error
                     :message "message"
                     :notes (list (source:make-note (source:make-location source '(7 . 10))
                                                    "LMN")))
                   stream))))
      msg))
