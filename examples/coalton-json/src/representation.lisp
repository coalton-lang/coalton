(cl:in-package #:coalton-json)

(coalton-toplevel
  ;; String-Map support functions
  ;;
  ;; This is a simple, immutable String-Map representation. It's not
  ;; very efficient.
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
    (Json-Object  (String-Map Json))))
