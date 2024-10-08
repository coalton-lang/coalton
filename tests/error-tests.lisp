(in-package #:coalton-tests)

;; Test that error messages containing source spans are correctly
;; printed.

(defvar *error-test-program*
  "  ;;
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
")

(deftest test-error ()
  (let* ((source (source:make-source-string *error-test-program* :name "file"))
         (msg (with-output-to-string (output)
                ;; an annotating error
                (handler-case
                    (source:error "message"
                                  (source:note (source:make-location source '(76 . 321))
                                               "define instance form")
                                  (source:secondary-note (source:make-location source '(132 . 319))
                                                         "message 2")
                                  (source:secondary-note (source:make-location source '(140 . 145))
                                                         "message 3")
                                  (source:secondary-note (source:make-location source '(170 . 174))
                                                         "message 4")
                                  (source:help (source:make-location source '(289 . 291))
                                               (lambda (existing)
                                                 (concatenate 'string "*" existing "*"))
                                               "message 5"))
                  (source:source-error (c)
                    (princ c output))))))
    ;; output text
    (is (check-string= "error printer"
                       msg
                       "error: message
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
"))))
