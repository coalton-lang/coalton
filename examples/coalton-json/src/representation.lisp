(cl:in-package #:coalton-json)

(coalton-toplevel
  ;; String-Map support functions
  ;;
  ;; This is a simple, immutable String-Map representation. It's not
  ;; very efficient.

  (declare concat3 (String -> String -> String -> String))
  (define (concat3 str1 str2 str3)
    (lisp String (str1 str2 str3)
      (cl:concatenate 'cl:string str1 str2 str3)))

  (declare concat5 (String -> String -> String -> String -> String -> String))
  (define (concat5 str1 str2 str3 str4 str5)
    (lisp String (str1 str2 str3 str4 str5)
      (cl:concatenate 'cl:string str1 str2 str3 str4 str5)))

  (define (alist-insert alist key val)
    (let ((traverse (fn (current new updated)
                      (match current
                        ((Nil) (if updated
                                   new
                                   (Cons (Tuple key val)
                                         new)))
                        ((Cons x xs)
                         (match x
                           ((Tuple k _)
                            (if (== k key)
                                (traverse xs (Cons (Tuple key val) new) True)
                                (traverse xs (Cons x new) updated)))))))))
      (traverse alist Nil False)))

  (define-type (String-Map :a)
    (SM-Alist (List (Tuple String :a))))

  (declare empty-sm (String-Map :a))
  (define empty-sm (SM-Alist Nil))

  (define (sm-lookup sm key)
    (let ((good (fn (x)
                  (match x
                    ((Tuple k _) (== k key))))))
      (match sm
        ((SM-Alist alist)
         (find good alist)))))

  (define (sm-insert sm key val)
    (match sm
      ((SM-Alist alist)
       (SM-Alist (alist-insert alist key val)))))

  ;; Coalton bug #147
  #+#:ignore
  (define (sm-add sm key val)
    (match sm
      ((SM-Alist alist)
       (let ((traverse (fn (current new updated)
                         (match current
                           ((Nil) (if updated
                                      new
                                      (Cons (Tuple key val)
                                            new)))
                           ((Cons x xs)
                            (match x
                              ((Tuple k _)
                               (if (== k key)
                                   (traverse xs (Cons (Tuple key val) new) True)
                                   (traverse xs (Cons x new) updated)))))))))
         (SM-Alist (traverse alist Nil False))))))

  ;; JSON Representation
  (define-type Json
    "Representation of arbitrary JSON."
    Json-Null
    (Json-Boolean Boolean)
    (Json-String  String)
    (Json-Number  Double-Float)
    (Json-Array   (List Json))
    (Json-Object  (String-Map Json)))

  (declare string-map->string (String-Map json -> String))
  (define (string-map->string sm)
    (match sm
      ((SM-Alist alist)
       (let ((traverse (fn (current acc)
                         (match current
                           ((nil) acc)
                           ((cons x xs)
                            (match x
                              ((tuple k v)
                               (traverse xs (cons 
                                             (concat5 "\"" k "\"" ":" (json->string v))
                                             acc)))))))))
         (let ((pairs (traverse alist (make-list))))
           (lisp string (pairs)
             (cl:format cl:nil "{~{~a~^, ~}}" pairs)))))))

  (declare json->string (Json -> String))
  (define (json->string jsn)
    (match jsn
      ((Json-Null) "nil")
      ((Json-Boolean b) (if b "true" "false"))
      ((Json-Number n) 
       (lisp String (n)
         (cl:format cl:nil "~f" n)))
      ((Json-String s) (concat3 "\"" s "\""))
      ((Json-Object sm)
       (string-map->string sm))
      ((Json-Array l)
       (let ((jl (map json->string l)))
         (lisp String (jl)
           (cl:format cl:nil "[~{~a~^, ~}]" jl)))))))
