(in-package coalton-user)
(cl:defun evl (exprs)
  (cl:let ((defns (cl:remove-if-not (cl:function is-definition)  exprs))
           (exs (cl:remove-if (cl:function is-definition)  exprs)))
    (cl:eval `(coalton-toplevel ,@defns))
    (cl:when exs
      (cl:values (cl:eval `(coalton  ,@exs)) (get-type (cl:car exs))))))


(cl:defun is-definition (x)
  (cl:and (cl:listp x)
          (cl:or
           (cl:eq 'DEFINE (cl:car x))
           (cl:eq 'DECLARE (cl:car x)))))

(cl:defun get-type (x)
  (cl:cond
    ((cl:listp x) (type-of (cl:car x)))
    ((cl:symbolp x) (type-of x))))


(cl:defun repl ()
  (cl:let ((x (cl:read-from-string (cl:format cl:nil "(~A)" (cl:read-line)))))
    (cl:multiple-value-bind (fv typ) (evl x)
      (cl:if fv (cl:format cl:t "~A: ~A~%" fv typ)))
    )
  (repl))
