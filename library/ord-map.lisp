(coalton-library/utils:defstdlib-package :coalton-library/ord-map
  (:use
   :coalton
   :coalton-library/classes
   :coalton-library/hash
   :coalton-library/tuple
   :coalton-library/functions)
  (:local-nicknames
   (#:tree :coalton-library/ord-tree)
   (#:iter :coalton-library/iterator))
  (:shadow #:Map #:empty)
  (:export
   #:Map
   #:empty
   #:lookup #:contains?
   #:insert
   #:replace
   #:replace-or-insert #:insert-or-replace
   #:remove #:without
   #:keys
   #:values
   #:entries
   #:collect!
   #:collect
   #:update
   #:merge #:union #:intersection #:difference #:sdifference
   #:filter-by-key #:filter-by-value #:filter-by-entry
   #:zip #:zip-with-default))

(in-package :coalton-library/ord-map)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (define-type (MapPair :key :value)
    (MapPair :key :value)

    ;; hack: when searching for an entry, we need a thing that holds just a key, but is of type `MapPair :key
    ;; :any', to pass to `tree:lookup' or `tree:remove'. A `Map' will never hold a `JustKey'.
    (JustKey :key))

  (define (key pair)
    (match pair
      ((MapPair k _) k)
      ((JustKey k) k)))

  (define (value pair)
    (match pair
      ((MapPair _ v) v)
      (_ (error "Misuse of `JustKey': Cannot read the Value of a JustKey"))))

  (define-instance (Into (MapPair :key :value) (Tuple :key :value))
    (define (into pair)
      (Tuple (key pair) (value pair))))

  (define-instance ((Eq :key) => Eq (MapPair :key :value))
    (define (== left right)
      (== (key left) (key right))))

  (define-instance ((Ord :key) => Ord (MapPair :key :value))
    (define (<=> left right)
      (<=> (key left) (key right))))

  (repr :transparent)
  (define-type (Map :key :value)
    "A red-black binary tree which associates each :KEY with a :VALUE, sorted by `<=>' on the keys and unique by `==' on the keys."
    (%Map (tree:Tree (MapPair :key :value))))

  (declare empty (Map :key :value))
  (define empty
    "A Map containing no mappings."
    (%Map tree:Empty))

  (declare lookup ((Ord :key) => ((Map :key :value) -> :key -> (Optional :value))))
  (define (lookup mp k)
    "Retrieve the value associated with K in MP, or None if MP does not contain K."
    (match mp
      ((%Map tre) (coalton-library/classes:map value (tree:lookup tre (JustKey k))))))

  (declare contains? ((Ord :key) => :key -> (Map :key :value) -> Boolean))
  (define (contains? k mp)
    "Does `mp` contain a key `==' to `k`?"
    (match (lookup mp k)
      ((Some _) True)
      ((None) False)))

  (declare insert ((Ord :key) => ((Map :key :value) -> :key -> :value -> (Optional (Map :key :value)))))
  (define (insert mp k v)
    "Associate K with V in MP. If MP already contains a mapping for K, return None."
    (coalton-library/classes:map %Map
                                 (match mp
                                   ((%Map tre) (tree:insert tre (MapPair k v))))))

  (declare replace ((Ord :key) => ((Map :key :value) -> :key -> :value -> (Optional (Tuple (Map :key :value)
                                                                                           :value)))))
  (define (replace mp k v)
    "Change the association of K to V in MP. If MP did not already contain a mapping for K, return None."
    (let (%Map tre) = mp)
    (match (tree:replace tre (MapPair k v))
      ((None) None)
      ((Some (Tuple new-tre removed-pair))
       (Some (Tuple (%Map new-tre)
                    (value removed-pair))))))

  (declare replace-or-insert ((Ord :key) => ((Map :key :value) -> :key -> :value -> (Tuple (Map :key :value)
                                                                                           (Optional :value)))))
  (define (replace-or-insert mp k v)
    "Update MP to associate K with V.

If MP already contains a mapping for K, replace it and return the old value."
    (let (%Map tre) = mp)
    (let (Tuple new-tre removed-pair) = (tree:replace-or-insert tre (MapPair k v)))
    (Tuple (%Map new-tre)
           (coalton-library/classes:map value removed-pair)))

  (declare insert-or-replace ((Ord :key) => ((Map :key :value) -> :key -> :value -> (Map :key :value))))
  (define (insert-or-replace mp k v)
    "Update MP to associate K with V.

If MP already contains a mapping for K, replace it and discard the old value.

Like `replace-or-insert', but prioritizing insertion as a use case."
    (let (%Map tre) = mp)
    (%Map (tree:insert-or-replace tre (MapPair k v))))

  (declare remove ((Ord :key) => ((Map :key :value) -> :key -> (Optional (Map :key :value)))))
  (define (remove mp k)
    "Remove the mapping associated with K in MP. If K does not have a value in MP, return None."
    (coalton-library/classes:map %Map
                                 (match mp
                                   ((%Map tre) (tree:remove tre (JustKey k))))))

  (declare without ((Ord :key) => :key -> (Map :key :value) -> (Map :key :value)))
  (define (without k mp)
    "If `mp` contains a mapping associated with a key `==' to `k`, construct a new Map without that mapping. Return `mp` if it contains no such mapping."
    (with-default mp (remove mp k)))

  (declare entries ((Map :key :value) -> (iter:Iterator (Tuple :key :value))))
  (define (entries mp)
    "Iterate over the (key value) pairs in MP, sorted by the keys in least-to-greatest order."
    (match mp
      ((%Map tre) (coalton-library/classes:map into (tree:increasing-order tre)))))

  (define-instance (iter:IntoIterator (Map :key :value) (Tuple :key :value))
    (define iter:into-iter entries))

  (declare keys ((Map :key :value) -> (iter:Iterator :key)))
  (define (keys mp)
    "Iterate over the keys in MP, sorted least-to-greatest."
    (match mp
      ((%Map tre) (coalton-library/classes:map key (tree:increasing-order tre)))))

  (declare values ((Map :key :value) -> (iter:Iterator :value)))
  (define (values mp)
    "Iterate over the values in MP, sorted by their corresponding keys in least-to-greatest order."
    (match mp
      ((%Map tre) (coalton-library/classes:map value (tree:increasing-order tre)))))

  (define-instance ((Eq :key) (Eq :value) => Eq (Map :key :value))
    (define (== left right)
      (iter:elementwise-match! == (entries left) (entries right))))

  (define-instance ((Hash :key) (Hash :value) => Hash (Map :key :value))
    (define (hash mp)
      (iter:elementwise-hash! (entries mp))))

  (declare collect! ((Ord :key) => ((iter:Iterator (Tuple :key :value)) -> (Map :key :value))))
  (define (collect! iter)
    "Construct a `Map` containing all the `(key value)` pairs in `iter`.

If `iter` contains duplicate keys, later values will overwrite earlier values."
    (iter:fold! (fn (mp tpl)
                  (uncurry (insert-or-replace mp) tpl))
                empty
                iter))

  (define-instance (Ord :key => iter:FromIterator (Map :key :value) (Tuple :key :value))
    (define iter:collect! collect!))

  (declare collect ((Ord :key) (Foldable :collection) => ((:collection (Tuple :key :value)) -> (Map :key :value))))
  (define (collect coll)
    "Construct a `Map` containing all the `(key value)` pairs in `coll`.

If `coll` contains duplicate keys, later values will overwrite earlier values."
    (fold (fn (mp tpl)
            (uncurry (insert-or-replace mp) tpl))
          empty
          coll))

  (define-instance ((Ord :key) (Foldable :collection) => (Into (:collection (Tuple :key :value)) (Map :key :value)))
    (define into collect))

  (declare update ((Ord :key) => (:value -> :value) -> (Map :key :value) -> :key -> (Optional (Map :key :value))))
  (define (update func mp key)
    "Apply FUNC to the value corresponding to KEY in MP, returning a new `Map' which maps KEY to the result of the function."
    (let (%Map tre) = mp)
    (let ((helper
            (fn (subtree)
              (match subtree
                ((tree:Empty) None)
                ;; `Branch' intentionally unexported from the tree package, but needed here because this
                ;; operation does tree search but is meaningless on trees that aren't maps.
                ((tree::Branch clr less (MapPair pivot val) more)
                 (match (<=> key pivot)
                   ((LT) (coalton-library/classes:map (fn (new-less)
                                                        (tree::Branch clr new-less (MapPair pivot val) more))
                                                      (helper less)))
                   ((Eq) (Some (tree::Branch clr less (MapPair pivot (func val)) more)))
                   ((GT) (coalton-library/classes:map (tree::Branch clr less (MapPair pivot val))
                                                      (helper more)))))
                ((tree::Branch _ _ (JustKey _) _) (error "Map contains `JustKey` rather than `MapPair`"))
                ((tree::DoubleBlackEmpty) (error "Encountered double-black node in ordered map outside of removal operation"))))))
      (coalton-library/classes:map %Map
                                   (helper tre))))

  (declare merge (Ord :key => Map :key :value -> Map :key :value -> Map :key :value))
  (define (merge a b)
    "Construct a Tree containing all the mappings of both A and B.

If A and B contain mappings X -> A' and X -> B', it is undefined whether the result maps X to A' or B'.

Because of the possibility that A and B will map the same X to different A' and B', this is not an associative
operation, and therefore Map cannot implement Monoid."
    (let (%Map a) = a)
    (let (%Map b) = b)
    (%Map (tree:merge a b)))

  (declare union (Ord :key => Map :key :value -> Map :key :value -> Map :key :value))
  (define union
    "Same as `merge`."
    merge)

  (declare intersection (Ord :key => Map :key :value -> Map :key :value -> Map :key :value))
  (define (intersection a b)
    "Construct a Map containing only those mappings from `b` that contain a key `==' to at least one key in `a`."
    (let (%Map a) = a)
    (let (%Map b) = b)
    (%Map (tree:intersection a b)))

  (declare difference (Ord :key => Map :key :value -> Map :key :value -> Map :key :value))
  (define (difference a b)
    "Construct a Map containing only those mappings from `b` which contain a key not `==' to any key in `a`."
    (let (%Map a) = a)
    (let (%Map b) = b)
    (%Map (tree:difference a b)))

  (declare sdifference (Ord :key => Map :key :value -> Map :key :value -> Map :key :value))
  (define (sdifference a b)
    "Symmetric difference.

Construct a Map containing only those mappings of `a` and `b` which do not associate keys `==' keys in the other."
    (let (%Map a) = a)
    (let (%Map b) = b)
    (%Map (tree:sdifference a b)))

  (declare filter-by-key (Ord :key => (:key -> Boolean) -> Map :key :value -> Map :key :value))
  (define (filter-by-key keep? mp)
    "Construct a Map containing only those entries of `mp` which contain keys which satisfy `keep?`."
    (let (%Map mp) = mp)
    (%Map (tree:filter (compose keep? key) mp)))

  (declare filter-by-value (Ord :key => (:value -> Boolean) -> Map :key :value -> Map :key :value))
  (define (filter-by-value keep? mp)
    "Construct a Map containing only those entries of `mp` which contain values which satisfy `keep?`."
    (let (%Map mp) = mp)
    (%Map (tree:filter (compose keep? value) mp)))

  (declare filter-by-entry (Ord :key => ((Tuple :key :value) -> Boolean) -> Map :key :value -> Map :key :value))
  (define (filter-by-entry keep? mp)
    "Construct a Map containing only those entries of `mp` which satisfy `keep?`."
    (let (%Map mp) = mp)
    (%Map (tree:filter (compose keep? into) mp)))

  (declare zip (Ord :key => Map :key :value1 -> Map :key :value2 -> Map :key (Tuple :value1 :value2)))
  (define (zip a b)
    "Construct a Map associating only those keys included in both `a` and `b` to the Tuple containing their respective values."
    (fold (fn (mp k)
            (insert-or-replace mp k (Tuple (unwrap (lookup a k))
                                           (unwrap (lookup b k)))))
          Empty
          (tree:intersection (tree:collect! (keys a))
                             (tree:collect! (keys b)))))

  (declare zip-with-default ((Ord :key) (Default :value1) (Default :value2)
                             => Map :key :value1 -> Map :key :value2 -> Map :key (Tuple :value1 :value2)))
  (define (zip-with-default a b)
    "Construct a Map associating all keys `a` and `b` to the Tuple containing their respective values, using the default value of the respectie types where a key is not associated."
    (fold (fn (mp k)
            (insert-or-replace mp k (Tuple (defaulting-unwrap (lookup a k))
                                           (defaulting-unwrap (lookup b k)))))
          Empty
          (tree:intersection (tree:collect! (keys a))
                             (tree:collect! (keys b)))))

  (define-instance (Ord :key => Semigroup (Map :key :value))
    (define <> merge))

  (define-instance (Ord :key => (Monoid (Map :key :value)))
    (define mempty Empty))

  (define-instance (Functor (Map :key))
    (define (coalton-library/classes:map func mp)
      (let (%Map tre) = mp)
      (let ((helper (fn (subtree)
                      (match subtree
                        ((tree::Empty) tree:Empty)
                        ((tree::Branch clr left (MapPair key value) right)
                         (tree::Branch clr
                                       (helper left)
                                       (MapPair key (func value))
                                       (helper right)))
                        ((tree::Branch _ _ (JustKey _) _) (error "Map contains `JustKey` rather than `MapPair`"))
                        ((tree::DoubleBlackEmpty) (error "Encountered double-black node in ordered map outside of removal operation"))))))
        (%Map (helper tre)))))

  ;; As with `tree:Tree', `Map' should probably implement `Traversable'.
  )
