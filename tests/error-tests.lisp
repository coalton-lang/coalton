(in-package #:coalton-tests)

;; Test that error messages containing source spans are correctly
;; printed.

(deftest test-error ()
  (uiop:with-temporary-file (:stream output-stream
                             :pathname program-file
                             :suffix "coalton"
                             :direction :output)
    ;; input text
    (write-string "  ;;
  ;; Kinds
  ;;

  (define-type Kind
    Star
    (Kfun Kind Kind))

  (define-instance (Eq Kind)
    (define (== k1 k2)
      (match (Tuple k1 k2)
        ((Tuple (Star) (Star)) True)
        ((Tuple (Kfun a1 a2)
                (Kfun b1 b2))
         (and (== a1 b1)
              (== a2 b2)))
        (_ False))))
"
                  output-stream)
    :close-stream
    (with-open-file (stream program-file)
      (let* ((source-error:*source* (source-error:make-displaced-source-file program-file "file" 0))
             (msg (with-output-to-string (output)
                    ;; an annotating error
                    (source-error:report-source-condition
                     (make-instance 'source-error:source-error
                      :location '(76 . 321)
                      :message "message"
                      :primary-note "define instance form"
                      :notes (list
                              (source-error:make-note
                               :type :secondary
                               :location  '(132 . 319)
                               :message "message 2")
                              (source-error:make-note
                               :type :secondary
                               :location  '(140 . 145)
                               :message "message 3")
                              (source-error:make-note
                               :type :secondary
                               :location  '(170 . 174)
                               :message "message 4"))
                      :help-notes
                      (list
                       (source-error:make-help
                        :location  '(289 . 291)
                        :replacement (lambda (existing)
                                       (concatenate 'string "*" existing "*"))
                        :message "message 5")))
                     output))))

        ;; output text
        (is (string= msg "error: message
  --> file:9:2
    |
 9  |      (define-instance (Eq Kind)
    |  ____^
 10 | |      (define (== k1 k2)
 11 | |        (match (Tuple k1 k2)
    | | _______-
    | ||               ----- message 3
 12 | ||         ((Tuple (Star) (Star)) True)
    | ||                  ---- message 4
 13 | ||         ((Tuple (Kfun a1 a2)
 ...
 16 | ||               (== a2 b2)))
 17 | ||         (_ False))))
    | ||__________________- message 2
    | |_____________________^ define instance form
help: message 5
 16 |               (*==* a2 b2)))
    |                ----
"))))))
