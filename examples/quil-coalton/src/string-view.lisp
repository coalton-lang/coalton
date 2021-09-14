(cl:in-package #:quil-coalton)

(coalton-toplevel
  ;; Opaque type for string views
  (define-type StringView
    (StringView Lisp-Object))

  (declare make-string-view (String -> StringView))
  (define (make-string-view str)
    (lisp StringView (str)
      (StringView
       (cl:make-array (cl:length (cl:the (cl:vector cl:character) str))
                      :element-type 'cl:character
                      :displaced-to str
                      :displaced-index-offset 0))))

  (declare next-char (StringView -> (Optional (Tuple Char StringView))))
  (define (next-char str)
    (lisp (Optional (Tuple Char StringView)) (str)
      (cl:let* ((arr str))
        (cl:declare (cl:type (cl:vector cl:character) arr)
                    ;; Muffle sbcl wanting to optimize aref. This cannot be optimized.
                    #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
        (cl:multiple-value-bind (displaced-to displaced-index-offset)
            (cl:array-displacement arr)
          (cl:if (cl:= 0 (cl:length arr))
                 None
                 (Some (Tuple (cl:aref arr 0)
                              (StringView (cl:if displaced-to
                                                 (cl:make-array (cl:1- (cl:length arr))
                                                                :element-type 'cl:character
                                                                :displaced-to displaced-to
                                                                :displaced-index-offset (cl:1+ displaced-index-offset))
                                                 (cl:make-array (cl:length arr)
                                                                :element-type 'cl:character
                                                                :displaced-to arr
                                                                :displaced-index-offset (cl:1+ displaced-index-offset)))))))))))

  ;; NOTE: Strings in Coalton are declared as simple-string which
  ;; should not be a displaced array. StringViews use displaced
  ;; arrays, on high optimization levels this may miscompile (in sbcl)
  ;; due to pointer aliasing issues
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
