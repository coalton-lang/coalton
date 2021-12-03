(in-package #:coalton-library)

(coalton-toplevel
  ;;
  ;; Hashtable
  ;;

  (define-type (Hashtable :key :value)
    (Hashtable Lisp-Object))

  (declare make-hashtable (Unit -> (Hashtable :key :value)))
  (define (make-hashtable _)
    "Create a new empty hashtable"
    (make-hashtable-capacity 0))

  (declare make-hashtable-capacity (Integer -> (Hashtable :key :value)))
  (define (make-hashtable-capacity capacity)
    "Crate a new empty hashtable with a given capacity"
    (lisp (Hashtable :key :value) (capacity)
      (Hashtable (cl:make-hash-table :size capacity :test #'cl:equalp))))

  (declare hashtable-get (:key -> (Hashtable :key :value) -> (Optional :value)))
  (define (hashtable-get key table)
    "Lookup KEY in TABLE"
    (match table
      ((Hashtable table)
       (lisp (Optional :a) (key table)
         (cl:multiple-value-bind (elem exists?)
             (cl:gethash key table)
           (cl:if exists?
                  (Some elem)
                  None))))))

  (declare hashtable-set (:key -> :value -> (Hashtable :key :value) -> Unit))
  (define (hashtable-set key value table)
    "Set KEY to VALUE in TABLE"
    (match table
      ((Hashtable table)
       (progn
         (lisp Lisp-Object (key value table)
           (cl:setf (cl:gethash key table) value))
         Unit))))

  (declare hashtable-remove (:key -> (Hashtable :key :value) -> Unit))
  (define (hashtable-remove key table)
    "Remove the entry at KEY from TABLE"
    (match table
      ((Hashtable table)
       (progn
         (lisp Lisp-Object (key table)
           (cl:remhash key table))
         Unit))))

  (declare hashtable-count ((Hashtable :key :value) -> Integer))
  (define (hashtable-count table)
    "Returns the number of entries in TABLE"
    (match table
      ((Hashtable table)
       (lisp Integer (table)
         (cl:hash-table-count table)))))

  (declare hashtable-foreach ((:key -> :value -> :a) -> (Hashtable :key :value) -> Unit))
  (define (hashtable-foreach f table)
    "Call F once for each key value pair in TABLE"
    (match table
      ((Hashtable table)
       (progn
         (lisp Lisp-Object (f table)
           (cl:maphash
            (cl:lambda (key value)
              (coalton-impl/codegen::A2 f key value))
            table))
         Unit))))

  (declare hashtable-keys ((Hashtable :key :value) -> (Vector :key)))
  (define (hashtable-keys table)
    "Returns the keys in TABLE as a vector"
    (progn
      (let v = (make-vector-capacity (hashtable-count table)))
      (hashtable-foreach
       (fn (key _)
         (vector-push key v))
       table)
      v))

  (declare hashtable-values ((Hashtable :key :value) -> (Vector :value)))
  (define (hashtable-values table)
    "Returns the values in TABLE as a vector"
    (progn
      (let v = (make-vector-capacity (hashtable-count table)))
      (hashtable-foreach
       (fn (_ value)
         (vector-push value v))
       table)
      v))

  (declare hashtable-entries ((Hashtable :key :value) -> (Vector (Tuple :key :value))))
  (define (hashtable-entries table)
    "Returns the keys and values in TABLE as a vector"
    (progn
      (let v = (make-vector-capacity (hashtable-count table)))
      (hashtable-foreach
       (fn (key value)
         (vector-push (Tuple key value) v))
       table)
      v)))
