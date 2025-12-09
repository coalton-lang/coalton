(cl:in-package #:quil-coalton)

(coalton-toplevel
  ;; Opaque type for string views
  (repr :native cl:string)
  (define-type StringView)

  (declare make-string-view (String -> StringView))
  (define (make-string-view str)
    (lisp StringView (str)
       (cl:make-array (cl:length (cl:the (cl:vector cl:character) str))
                      :element-type 'cl:character
                      :displaced-to str
                      :displaced-index-offset 0)))

  (declare next-char (StringView -> (Optional (Tuple coalton:Char StringView))))
  (define (next-char str)
       (lisp (Optional (Tuple coalton:Char StringView)) (str)
         (cl:let* ((arr str))
           (cl:declare (cl:type (cl:vector cl:character) arr)
                       ;; Muffle sbcl wanting to optimize aref. This cannot be optimized.
                       (coalton-compatibility:try-muffle-compiler-note-condition))
           (cl:multiple-value-bind (displaced-to displaced-index-offset)
               (cl:array-displacement arr)
             (cl:if (cl:= 0 (cl:length arr))
                    None
                    (Some (Tuple (cl:aref arr 0)
                                 (cl:if displaced-to
                                                    (cl:make-array (cl:1- (cl:length arr))
                                                                   :element-type 'cl:character
                                                                   :displaced-to displaced-to
                                                                   :displaced-index-offset (cl:1+ displaced-index-offset))
                                                    (cl:make-array (cl:length arr)
                                                                   :element-type 'cl:character
                                                                   :displaced-to arr
                                                                   :displaced-index-offset (cl:1+ displaced-index-offset))))))))))

  (declare string-view-get (StringView -> String))
  (define (string-view-get str)
       (lisp String (str) str))

  (declare string-view-empty-p (StringView -> Boolean))
  (define (string-view-empty-p str)
       (lisp Boolean (str)
         (cl:let* ((arr str))
           (cl:declare (cl:type (cl:vector cl:character) arr))
           (cl:if (cl:= 0 (cl:length arr))
                  True
                  False)))))
