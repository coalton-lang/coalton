(defpackage #:mine-tests/indent-context
  (:use #:cl)
  (:local-nicknames
   (#:source #:mine/syntax/context)
   (#:indent #:mine/syntax/indent)
   (#:gap #:mine/buffer/gap))
  (:export #:run-indent-context-tests))

(in-package #:mine-tests/indent-context)

(defun check (condition format-control &rest args)
  (unless condition (error (apply #'format nil format-control args))))

(defun optional-value (value)
  (unless (coalton-impl/runtime/optional:cl-none-p value)
    (coalton-impl/runtime/optional:unwrap-cl-some value)))

(defun when-rules ()
  (list (indent:IndentRuleEntry
         "when" (indent:RuleKnown (coalton:Some 2) coalton:None
                                  coalton:False coalton:False coalton:False))))

(defun indentation (text line &optional rules)
  (optional-value
   (indent:compute-prepared-indent
    (indent:prepare-indent-for-line (gap:gap-from-string text) 0 line)
    rules)))

(defun run-indent-context-tests ()
  (check (= 2 (indentation (format nil "(when ~S~%(print :ok))" "x") 1 (when-rules)))
         "A string condition must count as the WHEN test argument")
  (dolist (argument '("x" "\"x\"" "#\\)" "'(a b)" "#'(lambda (x) x)"))
    (let ((text (format nil "(f ~A~%next)" argument)))
      (check (= 3 (indentation text 1))
             "Every complete argument occupies one form slot: ~S" text)))
  (let ((text (format nil "(f [1~%2~%])")))
    (check (= 4 (indentation text 1)) "Bracket elements align with the first element")
    (check (= 3 (indentation text 2)) "Closing brackets align with their opening bracket"))
  (check (= 1 (indentation (format nil "[1~%2]") 1))
         "Top-level bracket literals must retain their own indentation context")
  (check (= 2 (indentation (format nil "'(a~%b)") 1))
         "Quoted lists align data elements rather than function arguments")
  (dolist (comment '("; comment" "#| comment |#"))
    (check (= 2 (indentation (format nil "(f~%~A~%)" comment) 1))
           "Leading whitespace before a comment opener is safe to indent"))
  (dolist (text (list (format nil "(write-line ~Ca~% b~C)" #\" #\")
                     (format nil "(f #| text~% text |#)")
                     (format nil "(f |a~% b|)")))
    (check (null (indentation text 1))
           "Reindentation must preserve text inside literals/comments: ~S" text)
    (check (= 1 (indent:compute-indent-with-rules (gap:gap-from-string text) 0 1 nil))
           "Legacy indentation wrapper must retain literal whitespace"))
  (let* ((text (format nil "(write-line ~Ca~%b~C)" #\" #\"))
         (gb (gap:gap-from-string text))
         (pos (1+ (position #\Newline text))))
    (check (null (optional-value (indent:compute-prepared-indent
                                 (indent:prepare-blank-indent-at-position gb 0 pos) nil)))
           "Enter inside a string must request preservation rather than added indentation"))
  (let* ((text (format nil "(when x~%body)"))
         (context (source:scan-source text))
         (pos (1+ (position #\Newline text)))
         (prepared (indent:prepare-indent-from-source context 1 pos coalton:False)))
    (check (eq context (indent:prepared-source prepared))
           "Prepared indentation must retain the existing parse result")
    (check (equal '("when") (indent:prepared-indent-heads prepared))
           "Prepared indentation must expose its runtime rule heads")
    (check (= 2 (optional-value (indent:compute-prepared-indent prepared (when-rules))))
           "Resolved rules must apply to the prepared context"))
  t)
