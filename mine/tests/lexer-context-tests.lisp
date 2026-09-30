(defpackage #:mine-tests/lexer-context
  (:use #:cl)
  (:local-nicknames (#:source #:mine/syntax/context)
                    (#:lexer #:mine/syntax/lexer)
                    (#:tok #:mine/syntax/token))
  (:export #:run-lexer-context-tests))

(in-package #:mine-tests/lexer-context)

(defun colored-spans (text)
  (mapcar (lambda (token)
            (list (tok:token-kind token)
                  (subseq text (tok:token-start token) (tok:token-end token))))
          (lexer:lex-source (source:scan-source text) 0)))

(defun token-triples (tokens)
  (mapcar (lambda (token)
            (list (tok:token-kind token) (tok:token-start token) (tok:token-end token)))
          tokens))

(defun line-ranges (text)
  "Editor line ranges, each excluding its LF or CRLF terminator."
  (loop :with start := 0
        :for newline := (position #\Newline text :start start)
        :for end := (or newline (length text))
        :collect (cons start (if (and newline (> end start) (char= #\Return (char text (1- end))))
                                 (1- end)
                                 end))
        :while newline
        :do (setf start (1+ newline))))

(defun check-line-highlighting (text &key every-range)
  "Lexing only the tokens of a line must match clipping the whole snapshot."
  (dolist (mode '(0 1))
    (let* ((context (source:scan-source text))
           (tokens (lexer:lex-source context mode))
           (highlight (lexer:source-highlight context mode))
           (remaining tokens))
      (flet ((check-range (start end tokens)
               (assert (equal (token-triples (lexer:tokens-for-line tokens start end))
                              (token-triples (lexer:highlight-line-tokens highlight start end)))
                       () "Line highlighting differs in mode ~D for [~D, ~D) of ~S"
                       mode start end text)))
        ;; Tokens ending before a line are clipped away, so each line can be
        ;; compared against the remaining tail of the whole snapshot.
        (dolist (range (line-ranges text))
          (loop :while (and remaining (<= (tok:token-end (first remaining)) (car range)))
                :do (pop remaining))
          (check-range (car range) (cdr range) remaining))
        (when every-range
          (loop :for start :from 0 :below (length text) :do
            (loop :for end :from (1+ start) :to (length text) :do
              (check-range start end tokens))))))))

(defun fuzz-texts (count max-length)
  "Deterministic texts built from delimiters, literal openers, and escapes."
  (let ((seed 12345)
        (alphabet (format nil "()[]\"';`,#|\\ƒ.:+-aZ1 ~C~C~C" #\Tab #\Return #\Newline)))
    (flet ((next ()
             (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
             (ash seed -16)))
      (loop :repeat count
            :collect (coerce (loop :repeat (mod (next) (1+ max-length))
                                   :collect (char alphabet (mod (next) (length alphabet))))
                             'string)))))

(defun check-line-highlighting-corpus ()
  (dolist (entry mine-tests/source-context:*source-corpus*)
    (check-line-highlighting (second entry) :every-range t))
  (dolist (text (list (format nil "(list \"first~% second\" |third~% fourth|)")
                      (format nil "#| a~%~%b |#~%(f ƒx.x 'y)~%~%  #\\a ; c~%")
                      (format nil "(a~C~C  \"b~C~C\")~C~C" #\Return #\Newline
                              #\Return #\Newline #\Return #\Newline)))
    (check-line-highlighting text :every-range t))
  (dolist (text (fuzz-texts 200 24))
    (check-line-highlighting text :every-range t))
  (check-line-highlighting
   (uiop:read-file-string (asdf:system-relative-pathname "mine" "src/syntax/lexer.ct"))))

(defun run-lexer-context-tests ()
  (assert (equal (list (list tok:TokOpenParen "(")
                      (list tok:TokSymbol "|x) [ y|")
                      (list tok:TokWhitespace " ")
                      (list tok:TokChar "#\\)")
                      (list tok:TokSymbol "foo")
                      (list tok:TokCloseParen ")"))
                 (colored-spans "(|x) [ y| #\\)foo)")))
  (assert (equal (list (list tok:TokCoaltonKeyword "ƒ")
                      (list tok:TokSymbol "x.x"))
                 (colored-spans "ƒx.x")))
  (let* ((text (format nil "(list \"first~% second\" |third~% fourth|)"))
         (tokens (lexer:lex-source (source:scan-source text) 0))
         (start (1+ (position #\Newline text)))
         (end (position #\Newline text :start start))
         (line (lexer:tokens-for-line tokens start end)))
    (assert (= 3 (length line)))
    (assert (eq tok:TokString (tok:token-kind (first line))))
    (assert (zerop (tok:token-start (first line))))
    (assert (= 8 (tok:token-end (first line))))
    (assert (eq tok:TokSymbol (tok:token-kind (third line))))
    (assert (= (- end start) (tok:token-end (third line)))))
  (dolist (entry mine-tests/source-context:*source-corpus*)
    (let* ((text (second entry))
           (tokens (lexer:lex-source (source:scan-source text) 0))
           (next 0))
      (dolist (token tokens)
        (assert (= next (tok:token-start token)))
        (assert (< (tok:token-start token) (tok:token-end token)))
        (assert (<= (tok:token-end token) (length text)))
        (setf next (tok:token-end token)))
      (assert (= next (length text)))))
  (check-line-highlighting-corpus)
  t)
