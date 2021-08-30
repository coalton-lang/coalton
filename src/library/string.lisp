(in-package #:coalton-user)

(coalton-toplevel
  ;;
  ;; String
  ;;

  (declare concat-string (String -> String -> String))
  (define (concat-string str1 str2)
    (lisp String
      (cl:concatenate 'cl:string str1 str2)))

  (declare unpack-string (String -> (List Char)))
  (define (unpack-string str)
    (lisp (List Char)
      (cl:reduce
       (cl:lambda (x xs) (Cons x xs))
       (cl:coerce str 'cl:list) :from-end cl:t :initial-value Nil)))

  (declare pack-string ((List Char) -> String))
  (define (pack-string xs)
    (lisp String
      (cl:labels ((f (xs)
                    (cl:if (cl:typep xs 'List/Nil)
                           ""
                           (cl:let ((element (cl:slot-value xs 'coalton-user::|_0|))
                                    (rest (cl:slot-value xs 'coalton-user::|_1|)))
                             (cl:declare (cl:type cl:character element))
                             (cl:concatenate
			      'cl:string (cl:string element) (f rest))))))
        (f xs))))

  (declare parse-int (String -> (Optional Int)))
  (define (parse-int str)
    (lisp (Optional Int)
      (cl:let ((x (cl:parse-integer str :junk-allowed cl:t)))
	(cl:if x
	       (Some x)
	       None))))
  ;;
  ;; String Instances
  ;;

  (define-instance (Eq String)
    (define (== a b)
      (lisp Boolean (to-boolean (cl:string= a b))))
    (define (/= a b)
      (not (== a b))))

  (define-instance (Ord String)
    (define (<=> a b)
      (lisp Ord
	 (cl:cond
	   ((cl:string> a b) GT)
	   ((cl:string< a b) LT)
	   (cl:t EQ)))))

  (define-instance (Semigroup String)
    (define (<> a b) (concat-string a b)))

  (define-instance (Monoid String)
    (define mempty "")))
