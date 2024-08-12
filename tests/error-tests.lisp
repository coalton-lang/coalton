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
    (let* ((f (se:make-source-file program-file :name "file"))
           (msg (with-output-to-string (output)
                  ;; an annotating error
                  (se:display-source-error
                   output
                   (se:source-error
                    :span '(76 . 321)
                    :file f
                    :message "message"
                    :primary-note "define instance form"
                    :notes (list
                            (se:make-source-error-note
                             :type :secondary
                             :span  '(132 . 319)
                             :message "message 2")
                            (se:make-source-error-note
                             :type :secondary
                             :span  '(140 . 145)
                             :message "message 3")
                            (se:make-source-error-note
                             :type :secondary
                             :span  '(170 . 174)
                             :message "message 4"))
                    :help-notes
                    (list
                     (se:make-source-error-help
                      :span  '(289 . 291)
                      :replacement (lambda (existing)
                                     (concatenate 'string "*" existing "*"))
                      :message "message 5")))))))
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
")))))
