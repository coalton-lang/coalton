(coalton/utils:defstdlib-package :coalton/ordmap
  (:use
   :coalton
   :coalton/classes
   :coalton/hash
   :coalton/tuple
   :coalton/functions)
  (:local-nicknames
   (#:tree :coalton/ordtree)
   (#:iter :coalton/iterator))
  (:shadow #:empty)
  (:export
   #:OrdMap
   #:empty
   #:empty?
   #:lookup
   #:lookup-neighbors
   #:max-key-entry
   #:min-key-entry
   #:insert
   #:adjoin
   #:replace
   #:remove
   #:update
   #:keys
   #:values
   #:entries
   #:collect!
   #:collect
   #:union
   #:intersection
   #:difference
   #:xor))

(in-package :coalton/ordmap)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (define-type (MapPair :key :value)
    (MapPair :key :value)

    ;; hack: when searching for an entry, we need a thing that holds just a key, but is of type `MapPair :key
    ;; :any', to pass to `tree:lookup' or `tree:remove'. A `OrdMap' will never hold a `JustKey'.
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
  (define-type (OrdMap :key :value)
    "A binary tree which associates each :KEY with a :VALUE, sorted by `<=>' on the keys and unique by `==' on the keys."
    (%Map (tree:OrdTree (MapPair :key :value))))

  ;; Mapping API
  (declare empty (OrdMap :key :value))
  (define empty
    "A OrdMap containing no mappings."
    (%Map tree:Empty))

  (declare empty? (OrdMap :key :value -> Boolean))
  (define (empty? m)
    "Returns True iff the given OrdMap is empty."
    (let (%Map t) = m)
    (tree:empty? t))

  ;; Mapping API
  (declare lookup ((Ord :key) => OrdMap :key :value -> :key -> Optional :value))
  (define (lookup mp k)
    "Retrieve the value associated with K in MP, or None if MP does not contain K."
    (let (%Map tre) = mp)
    (map value (tree:lookup tre (JustKey k))))

  (declare max-key-entry ((Ord :key) => OrdMap :key :value -> (Optional (Tuple :key :value))))
  (define (max-key-entry mp)
    "Returns the entry (Tuple :key :value) with the maximum key in the map `mp`. If the map is empty, None is returned."
    (let (%Map tre) = mp)
    (map into (tree:max-element tre)))

  (declare min-key-entry ((Ord :key) => OrdMap :key :value -> (Optional (Tuple :key :value))))
  (define (min-key-entry mp)
    "Returns the entry (Tuple :key :value) with the minimum key in the map `mp`. If the map is empty, None is returned."
    (let (%Map tre) = mp)
    (map into (tree:min-element tre)))

  (declare lookup-neighbors ((Ord :key) => OrdMap :key :value -> :key
                             -> (Tuple3 (Optional (Tuple :key :value))
                                        (Optional (Tuple :key :value))
                                        (Optional (Tuple :key :value)))))
  (define (lookup-neighbors mp k)
    "Returns elements LO, ON, and HI, such that LO has the closest
key that is strictly less than `k`, ON is the entry with `k`,
and HI has the closest key that is strictly greater than 'k'.
Any of these values can be None if there's no such entry."
    (let (%Map tre) = mp)
    (let ((declare convert ((Optional (MapPair :key :value))
                            -> Optional (Tuple :key :value)))
          (convert (fn (p)
                     (do (x <- p)
                         (Some (into x))))))
      (match (tree:lookup-neighbors tre (JustKey k))
        ((Tuple3 lo on hi)
         (Tuple3 (convert lo) (convert on) (convert hi))))))

  ;; Mapping API
  (declare insert (Ord :key => OrdMap :key :value -> :key -> :value -> OrdMap :key :value))
  (define (insert mp k v)
    "Returns an OrdMap in which the key `k` is associated with `v` added
to the `mp`.  If `mp` already contains mapping for `k`, it is replaced."
    (let (%Map tre) = mp)
    (%Map (tree:insert tre (MapPair k v))))

  ;; Mapping API
  (declare adjoin (Ord :key => OrdMap :key :value -> :key -> :value -> OrdMap :key :value))
  (define (adjoin mp k v)
    "Returns an OrdMap in which the key `k` is associated with `v` added
to the `mp`, only when `mp` doesn't have an association with `k`.
If `mp` already contains an association with `k`, `mp` is returned as is."
    (let (%Map tre) = mp)
    (%Map (tree:adjoin tre (MapPair k v))))

  ;; Mapping API
  (declare replace (Ord :key => OrdMap :key :value -> :key -> :value -> OrdMap :key :value))
  (define (replace mp k v)
    "Returns an OrdMap in which the key `k` is associated with `v` replaced
from `mp`, when `mp` already has an association with `k`.
If `mp` doesn't has an association with `k`, `mp` is returned as is."
    (let (%Map tre) = mp)
    (%Map (tree:replace tre (MapPair k v))))

  ;; Mapping API
  (declare remove (Ord :key => OrdMap :key :value -> :key -> OrdMap :key :value))
  (define (remove mp k)
    "Returns an OrdMap in which the association with key 'k' is removed from
`mp`.  If `mp` doesn't have an association with `k`, it is returned as is."
    (let (%Map tre) = mp)
    (%Map (tree:remove tre (JustKey k))))

  (declare entries ((OrdMap :key :value) -> (iter:Iterator (Tuple :key :value))))
  (define (entries mp)
    "Iterate over the (key value) pairs in MP, sorted by the keys in least-to-greatest order."
    (match mp
      ((%Map tre) (map into (tree:increasing-order tre)))))

  (define-instance (iter:IntoIterator (OrdMap :key :value) (Tuple :key :value))
    (define iter:into-iter entries))

  (declare keys ((OrdMap :key :value) -> (iter:Iterator :key)))
  (define (keys mp)
    "Iterate over the keys in MP, sorted least-to-greatest."
    (match mp
      ((%Map tre) (map key (tree:increasing-order tre)))))

  (declare values ((OrdMap :key :value) -> (iter:Iterator :value)))
  (define (values mp)
    "Iterate over the values in MP, sorted by their corresponding keys in least-to-greatest order."
    (match mp
      ((%Map tre) (map value (tree:increasing-order tre)))))

  (define-instance ((Eq :key) (Eq :value) => Eq (OrdMap :key :value))
    (define (== left right)
      (iter:elementwise-match! == (entries left) (entries right))))

  (define-instance ((Hash :key) (Hash :value) => Hash (OrdMap :key :value))
    (define (hash mp)
      (iter:elementwise-hash! (entries mp))))

  (declare collect! ((Ord :key) => ((iter:Iterator (Tuple :key :value)) -> (OrdMap :key :value))))
  (define (collect! iter)
    "Construct a `OrdMap` containing all the `(key value)` pairs in `iter`.

If `iter` contains duplicate keys, later values will overwrite earlier values."
    (iter:fold! (fn (mp tpl)
                  (uncurry (insert mp) tpl))
                empty
                iter))

  (declare collect ((Ord :key) (Foldable :collection) => ((:collection (Tuple :key :value)) -> (OrdMap :key :value))))
  (define (collect coll)
    "Construct a `OrdMap` containing all the `(key value)` pairs in `coll`.

If `coll` contains duplicate keys, later values will overwrite earlier values."
    (fold (fn (mp tpl)
            (uncurry (insert mp) tpl))
          empty
          coll))

  (define-instance (Ord :key => iter:FromIterator (OrdMap :key :value) (Tuple :key :value))
    (define iter:collect! collect!))


  ;; Mapping API
  (declare update (Ord :key => OrdMap :key :value -> :key
                       -> (Optional :value -> Tuple (Optional :value) :a)
                       -> Tuple (OrdMap :key :value) :a))
  (define (update mp k f)
    "Lookup an association with `k` in `mp`.  If there's an entry, call `f`
with its value wrapped with Some.  If there isn't an entry, call 'f' with
None.  `f` must return a tuple of possible new value and an auxiliary
result.
If the fst of `f`'s return value is Some, its content is inserted into
`mp` in association with `k`.   If the fst of `f`'s return value is None,
an association with `k` in `mp` is removed.  A possibly updated mapping
is returned as the fst element of the tuple.
The auxiliary result from `f` is returnd as the snd result.

This can be used for the caller to obtain the previous state along
updated map.  For example, the following code inserts an entry (k, v)
into mp, and obtain (Some v') or None in the second value of the
result, where v' is the previous value associated with k.

```
(update mp k (Tuple v))
```
"
    (let (%Map tre) = mp)
    (let call-f = (fn (e) (match e
                            ((None) (f None))
                            ((Some (MapPair _ v)) (f (Some v)))
                            ((Some _) (error "Stray JustKey")))))
    (let (Tuple tre2 aux) =
      (tree:update tre (JustKey k)
                   (fn (e)
                     (let (Tuple vv aux) = (call-f e))
                     (match vv
                       ((None)
                        (Tuple (the (Optional (MapPair :a :b)) None) aux))
                       ((Some v)
                        (Tuple (Some (MapPair k v)) aux))))))
    (Tuple (%Map tre2) aux))


  (declare union (Ord :key => OrdMap :key :value -> OrdMap :key :value -> OrdMap :key :value))
  (define (union a b)
    "Construct an OrdMap containing all the mappings of both A and B.

If A and B contain mappings X -> A' and X -> B', the former mapping is kept.

The operation is associative, but not commutative."
    (let (%Map ta) = a)
    (let (%Map tb) = b)
    (%Map (tree:union ta tb)))

  (declare intersection (Ord :key => OrdMap :key :value -> OrdMap :key :value -> OrdMap :key :value))
  (define (intersection a b)
    "Construct an OrdMap contaning elements whose key appears in both `a` and `b`.
The resulting values are from `a`."
    (let (%Map ta) = a)
    (let (%Map tb) = b)
    (%Map (tree:intersection ta tb)))

  (declare difference (Ord :key => OrdMap :key :value -> OrdMap :key :value -> OrdMap :key :value))
  (define (difference a b)
    "Returns an OrdMap that contains mappings in `a` but not in `b`."
    (let (%Map ta) = a)
    (let (%Map tb) = b)
    (%Map (tree:difference ta tb)))

  (declare xor (Ord :key => OrdMap :key :value -> OrdMap :key :value -> OrdMap :key :value))
  (define (xor a b)
    "Returns an OrdMap that contains mappings either in `a` or in `b`,
but not in both."
    (let (%Map ta) = a)
    (let (%Map tb) = b)
    (%Map (tree:xor ta tb)))

  (define-instance (Ord :key => Semigroup (OrdMap :key :value))
    (define <> union))

  (define-instance (Ord :key => Monoid (OrdMap :key :value))
    (define mempty Empty))

  (define-instance (Functor (OrdMap :key))
    (define (map func mp)
      (let (%Map tre) = mp)
      (%Map (tree:transform-elements (fn (e)
                                       (match e
                                         ((MapPair k v) (MapPair k (func v)))
                                         ((JustKey _) (error "Stray JustKey"))))
                                     tre))))

  ;; As with `tree:Tree', `OrdMap' should probably implement `Traversable'.
  )
