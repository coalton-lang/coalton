# Reference for COALTON-USER

## File: [types.lisp](types.lisp)

### Types

#### `LIST :A`<a name="LIST"></a>
- `(CONS :A (LIST :A))`
- `NIL`

Constructors:
`CONS :: (:A → (LIST :A) → (LIST :A))`
`NIL :: (LIST :A)`

Instances:
- [`EQ :A`](#EQ) `=>` [`EQ`](#EQ) [`(LIST :A)`](#LIST)
- [`ISO`](#ISO) [`(VECTOR :A)`](#VECTOR) [`(LIST :A)`](#LIST)
- [`ISO`](#ISO) [`(LIST CHAR)`](#LIST) [`STRING`](#STRING)
- [`INTO`](#INTO) [`(VECTOR :A)`](#VECTOR) [`(LIST :A)`](#LIST)
- [`INTO`](#INTO) [`(LIST :A)`](#LIST) [`(VECTOR :A)`](#VECTOR)
- [`INTO`](#INTO) [`(LIST CHAR)`](#LIST) [`STRING`](#STRING)
- [`INTO`](#INTO) [`STRING`](#STRING) [`(LIST CHAR)`](#LIST)
- [`MONAD`](#MONAD) [`LIST`](#LIST)
- [`MONOID`](#MONOID) [`(LIST :A)`](#LIST)
- [`FUNCTOR`](#FUNCTOR) [`LIST`](#LIST)
- [`SEMIGROUP`](#SEMIGROUP) [`(LIST :A)`](#LIST)
- [`ALTERNATIVE`](#ALTERNATIVE) [`LIST`](#LIST)
- [`APPLICATIVE`](#APPLICATIVE) [`LIST`](#LIST)


#### `TUPLE :A :B`<a name="TUPLE"></a>
- `(TUPLE :A :B)`

Constructors:
`TUPLE :: (:A → :B → (TUPLE :A :B))`

Instances:
- [`EQ :A`](#EQ) [`EQ :B`](#EQ) `=>` [`EQ`](#EQ) [`(TUPLE :A :B)`](#TUPLE)
- [`ISO`](#ISO) [`(TUPLE :A :B)`](#TUPLE) [`(TUPLE :B :A)`](#TUPLE)
- [`ORD :A`](#ORD) [`ORD :B`](#ORD) `=>` [`ORD`](#ORD) [`(TUPLE :A :B)`](#TUPLE)
- [`INTO`](#INTO) [`(TUPLE :A :B)`](#TUPLE) [`(TUPLE :B :A)`](#TUPLE)


#### `RESULT :A :B`<a name="RESULT"></a>
- `(ERR :A)`
- `(OK :B)`

Constructors:
`ERR :: (:A → (RESULT :A :B))`
`OK :: (:B → (RESULT :A :B))`

Instances:
- [`EQ :A`](#EQ) [`EQ :B`](#EQ) `=>` [`EQ`](#EQ) [`(RESULT :A :B)`](#RESULT)
- [`ISO`](#ISO) [`(RESULT UNIT :A)`](#RESULT) [`(OPTIONAL :A)`](#OPTIONAL)
- [`ORD :A`](#ORD) [`ORD :B`](#ORD) `=>` [`ORD`](#ORD) [`(RESULT :A :B)`](#RESULT)
- [`INTO`](#INTO) [`(OPTIONAL :A)`](#OPTIONAL) [`(RESULT UNIT :A)`](#RESULT)
- [`INTO`](#INTO) [`(RESULT :A :B)`](#RESULT) [`(OPTIONAL :B)`](#OPTIONAL)
- [`MONAD`](#MONAD) [`(RESULT :A)`](#RESULT)
- [`MONOID :A`](#MONOID) `=>` [`MONOID`](#MONOID) [`(RESULT :B :A)`](#RESULT)
- [`FUNCTOR`](#FUNCTOR) [`(RESULT :A)`](#RESULT)
- [`SEMIGROUP :A`](#SEMIGROUP) `=>` [`SEMIGROUP`](#SEMIGROUP) [`(RESULT :B :A)`](#RESULT)
- [`APPLICATIVE`](#APPLICATIVE) [`(RESULT :A)`](#RESULT)
- [`WITHDEFAULT`](#WITHDEFAULT) [`(RESULT :A)`](#RESULT)


#### `BOOLEAN`
- `FALSE`
- `TRUE`

Constructors:
`FALSE :: BOOLEAN`
`TRUE :: BOOLEAN`

Instances:
- [`EQ`](#EQ) [`BOOLEAN`](#BOOLEAN)
- [`ORD`](#ORD) [`BOOLEAN`](#BOOLEAN)
- [`SHOW`](#SHOW) [`BOOLEAN`](#BOOLEAN)


#### `OPTIONAL :A`<a name="OPTIONAL"></a>
- `(SOME :A)`
- `NONE`

Constructors:
`SOME :: (:A → (OPTIONAL :A))`
`NONE :: (OPTIONAL :A)`

Instances:
- [`EQ :A`](#EQ) `=>` [`EQ`](#EQ) [`(OPTIONAL :A)`](#OPTIONAL)
- [`ISO`](#ISO) [`(RESULT UNIT :A)`](#RESULT) [`(OPTIONAL :A)`](#OPTIONAL)
- [`ORD :A`](#ORD) `=>` [`ORD`](#ORD) [`(OPTIONAL :A)`](#OPTIONAL)
- [`INTO`](#INTO) [`(OPTIONAL :A)`](#OPTIONAL) [`(RESULT UNIT :A)`](#RESULT)
- [`INTO`](#INTO) [`(RESULT :A :B)`](#RESULT) [`(OPTIONAL :B)`](#OPTIONAL)
- [`SHOW :A`](#SHOW) `=>` [`SHOW`](#SHOW) [`(OPTIONAL :A)`](#OPTIONAL)
- [`MONAD`](#MONAD) [`OPTIONAL`](#OPTIONAL)
- [`MONOID :A`](#MONOID) `=>` [`MONOID`](#MONOID) [`(OPTIONAL :A)`](#OPTIONAL)
- [`FUNCTOR`](#FUNCTOR) [`OPTIONAL`](#OPTIONAL)
- [`MONADFAIL`](#MONADFAIL) [`OPTIONAL`](#OPTIONAL)
- [`SEMIGROUP :A`](#SEMIGROUP) `=>` [`SEMIGROUP`](#SEMIGROUP) [`(OPTIONAL :A)`](#OPTIONAL)
- [`ALTERNATIVE`](#ALTERNATIVE) [`OPTIONAL`](#OPTIONAL)
- [`APPLICATIVE`](#APPLICATIVE) [`OPTIONAL`](#OPTIONAL)
- [`WITHDEFAULT`](#WITHDEFAULT) [`OPTIONAL`](#OPTIONAL)


### Functions

#### `OR`
`(BOOLEAN → BOOLEAN → BOOLEAN)`

#### `AND`
`(BOOLEAN → BOOLEAN → BOOLEAN)`

#### `NOT`
`(BOOLEAN → BOOLEAN)`

#### `XOR`
`(BOOLEAN → BOOLEAN → BOOLEAN)`

#### `UNDEFINED`
`∀ :A :B. (:A → :B)`


## File: [classes.lisp](classes.lisp)

### Types

#### `ORD`
- `LT`
- `GT`
- `EQ`

Constructors:
`LT :: ORD`
`GT :: ORD`
`EQ :: ORD`



### Functions

#### `<`
`∀ :A. ORD :A ⇒ (:A → :A → BOOLEAN)`

#### `>`
`∀ :A. ORD :A ⇒ (:A → :A → BOOLEAN)`

#### `<=`
`∀ :A. ORD :A ⇒ (:A → :A → BOOLEAN)`

#### `>=`
`∀ :A. ORD :A ⇒ (:A → :A → BOOLEAN)`

#### `MAX`
`∀ :A. ORD :A ⇒ (:A → :A → :A)`

#### `MIN`
`∀ :A. ORD :A ⇒ (:A → :A → :A)`


## File: [builtin.lisp](builtin.lisp)

### Functions

#### `GCD`
`(INT → INT → INT)`

#### `LCM`
`(INT → INT → INT)`

#### `MOD`
`(INT → INT → INT)`

#### `ODD`
`(INT → BOOLEAN)`

#### `EVEN`
`(INT → BOOLEAN)`

#### `EXPT`
`(INT → INT → INT)`


## File: [string.lisp](string.lisp)

### Functions

#### `PARSE-INT`
`(STRING → (OPTIONAL INT))`

#### `PACK-STRING`
`((LIST CHAR) → STRING)`

#### `CONCAT-STRING`
`(STRING → STRING → STRING)`

#### `UNPACK-STRING`
`(STRING → (LIST CHAR))`


## File: [optional.lisp](optional.lisp)

### Functions

#### `ISNONE`
`∀ :A. ((OPTIONAL :A) → BOOLEAN)`

#### `ISSOME`
`∀ :A. ((OPTIONAL :A) → BOOLEAN)`

#### `FROMSOME`
`∀ :A. (STRING → (OPTIONAL :A) → :A)`


## File: [list.lisp](list.lisp)

### Functions

#### `ALL`
`∀ :A. ((:A → BOOLEAN) → (LIST :A) → BOOLEAN)`

Returns TRUE if every element in XS matches F.


#### `ANY`
`∀ :A. ((:A → BOOLEAN) → (LIST :A) → BOOLEAN)`

Returns TRUE if at least one element in XS matches F.


#### `SUM`
`∀ :A. NUM :A ⇒ ((LIST :A) → :A)`

Returns the sum of XS


#### `ZIP`
`∀ :A :B. ((LIST :A) → (LIST :B) → (LIST (TUPLE :A :B)))`

Builds a list of tuples with the elements of XS and YS.


#### `FIND`
`∀ :A. ((:A → BOOLEAN) → (LIST :A) → (OPTIONAL :A))`

Returns the first element in a list matching the predicate function F.


#### `FOLD`
`∀ :A :B. ((:A → :B → :B) → :B → (LIST :A) → :B)`

Tail recursive left fold on lists.


#### `HEAD`
`∀ :A. ((LIST :A) → (OPTIONAL :A))`

Returns the first element of a list.


#### `NULL`
`∀ :A. ((LIST :A) → BOOLEAN)`

Returns TRUE if XS is an empty list.


#### `SORT`
`∀ :A. ORD :A ⇒ ((LIST :A) → (LIST :A))`

Performs a stable sort of XS.


#### `TAIL`
`∀ :A. ((LIST :A) → (OPTIONAL (LIST :A)))`

Returns every element but the first in a list.


#### `FOLDR`
`∀ :A :B. ((:A → :B → :B) → :B → (LIST :A) → :B)`

Right fold on lists. Is short circuiting but is not tail recursive.


#### `INDEX`
`∀ :A. ((LIST :A) → INT → (OPTIONAL :A))`

Returns the Ith element of XS.


#### `RANGE`
`(INT → INT → (LIST INT))`

Returns a list containing the numbers from START to END inclusive.


#### `UNION`
`∀ :A. EQ :A ⇒ ((LIST :A) → (LIST :A) → (LIST :A))`

Returns a new list with the elements from both XS and YS and without duplicates.


#### `APPEND`
`∀ :A. ((LIST :A) → (LIST :A) → (LIST :A))`

Appends two lists together and returns a new list.


#### `CONCAT`
`∀ :A. ((LIST (LIST :A)) → (LIST :A))`

Appends a list of lists together into a single new list.


#### `DELETE`
`∀ :A. EQ :A ⇒ (:A → (LIST :A) → (LIST :A))`

Return a new list with the first element equal to X removed.


#### `FILTER`
`∀ :A. ((:A → BOOLEAN) → (LIST :A) → (LIST :A))`

Returns a new list containing every element of XS that matches the predicate function F in the same order.


#### `INSERT`
`∀ :A. ORD :A ⇒ (:A → (LIST :A) → (LIST :A))`

Inserts an element into a list at the first place it is less than or equal to the next element.


#### `LENGTH`
`∀ :A. ((LIST :A) → INT)`

Returns the length of a list.


#### `LOOKUP`
`∀ :A :B. EQ :A ⇒ (:A → (LIST (TUPLE :A :B)) → (OPTIONAL :B))`

Returns the value of the first (key, value) tuple in XS where the key matches E.


#### `MEMBER`
`∀ :A. EQ :A ⇒ (:A → (LIST :A) → BOOLEAN)`

Returns true if any element of XS is equal to E.


#### `REPEAT`
`∀ :A. (INT → :A → (LIST :A))`

Returns a list with X repeated N times.


#### `SORTBY`
`∀ :A. ((:A → :A → ORD) → (LIST :A) → (LIST :A))`

Generic version of sort


#### `MAXIMUM`
`∀ :A. ORD :A ⇒ ((LIST :A) → (OPTIONAL :A))`

Returns the greatest element in XS.


#### `MINIMUM`
`∀ :A. ORD :A ⇒ ((LIST :A) → (OPTIONAL :A))`

Returns the least element in XS.


#### `PRODUCT`
`∀ :A. NUM :A ⇒ ((LIST :A) → :A)`

Returns the product of XS


#### `REVERSE`
`∀ :A. ((LIST :A) → (LIST :A))`

Returns a new list containing the same elements in reverse order.


#### `ZIPWITH`
`∀ :A :B :C. ((:A → :B → :C) → (LIST :A) → (LIST :B) → (LIST :C))`

Builds a new list by calling F with elements of XS and YS.


#### `INSERTBY`
`∀ :A. ((:A → :A → ORD) → :A → (LIST :A) → (LIST :A))`

Generic version of insert


#### `CONCATMAP`
`∀ :A :B. ((:A → (LIST :B)) → (LIST :A) → (LIST :B))`

Apply F to each element in XS and concatenate the results.


#### `ELEMINDEX`
`∀ :A. EQ :A ⇒ (:A → (LIST :A) → (OPTIONAL INT))`

#### `FINDINDEX`
`∀ :A. ((:A → BOOLEAN) → (LIST :A) → (OPTIONAL INT))`

#### `PARTITION`
`∀ :A. ((:A → BOOLEAN) → (LIST :A) → (TUPLE (LIST :A) (LIST :A)))`

Splits a list into two new lists. The first list contains elements matching predicate F.


#### `SINGLETON`
`∀ :A. (:A → (LIST :A))`

Returns a single element list containg only X.


#### `TRANSPOSE`
`∀ :A. ((LIST (LIST :A)) → (LIST (LIST :A)))`

Transposes a matrix represented by a list of lists.


#### `INTERCALATE`
`∀ :A. ((LIST :A) → (LIST (LIST :A)) → (LIST :A))`

Intersperses XS into XSS and then concatenates the result.


#### `INTERSPERSE`
`∀ :A. (:A → (LIST :A) → (LIST :A))`

Returns a new list where every other element is E.


#### `INTERSECTION`
`∀ :A. EQ :A ⇒ ((LIST :A) → (LIST :A) → (LIST :A))`

Returns elements which occur in both lists. Does not return duplicates and does not guarantee order.


#### `LIST-DIFFERENCE`
`∀ :A. EQ :A ⇒ ((LIST :A) → (LIST :A) → (LIST :A))`

Returns a new list with the first occurence of each element in YS deleted from XS.


#### `REMOVE-DUPLICATES`
`∀ :A. EQ :A ⇒ ((LIST :A) → (LIST :A))`

Returns a new list without duplicate elements.



## File: [tuple.lisp](tuple.lisp)

### Functions

#### `FST`
`∀ :A :B. ((TUPLE :A :B) → :A)`

#### `SND`
`∀ :A :B. ((TUPLE :A :B) → :B)`


## File: [result.lisp](result.lisp)

### Functions

#### `ISOK`
`∀ :A :B. ((RESULT :A :B) → BOOLEAN)`

Returns TRUE if X is ERR


#### `ISERR`
`∀ :A :B. ((RESULT :A :B) → BOOLEAN)`

Returns TRUE if X is ERR


#### `MAPERR`
`∀ :A :B :C. ((:A → :B) → (RESULT :A :C) → (RESULT :B :C))`

Map over the ERR case


#### `FLATTENRESULT`
`∀ :A. ((RESULT :A :A) → :A)`


## File: [functions.lisp](functions.lisp)

### Functions

#### `ID`
`∀ :A. (:A → :A)`

A function that always returns its argument


#### `FIX`
`∀ :A :B. (((:A → :B) → :A → :B) → :A → :B)`

The factorial function can be written
    ```
    (define fact
      (fix 
        (fn (f n)
          (if (== n 0)
            1
            (* n (f (- n 1)))))))    
    ```


#### `ASUM`
`∀ :A :B. ALTERNATIVE :A ⇒ ((LIST (:A :B)) → (:A :B))`

Fold over a list using alt


#### `FLIP`
`∀ :A :B :C. ((:A → :B → :C) → :B → :A → :C)`

FLIP reverses the arguments to F


#### `MSUM`
`∀ :A. MONOID :A ⇒ ((LIST :A) → :A)`

Fold over a list using <>


#### `CONST`
`∀ :A :B. (:A → :B → :A)`

A function that always returns its first argument


#### `ERROR`
`∀ :A. (STRING → :A)`

Signal an error by calling CL:ERROR


#### `TRACE`
`(STRING → UNIT)`

Print a line to *STANDARD-OUTPUT*


#### `COMPOSE`
`∀ :A :B :C. ((:A → :B) → (:C → :A) → :C → :B)`

#### `SEQUENCE`
`∀ :A :B. APPLICATIVE :A ⇒ ((LIST (:A :B)) → (:A (LIST :B)))`

#### `TRAVERSE`
`∀ :A :B :C. APPLICATIVE :B ⇒ ((:A → (:B :C)) → (LIST :A) → (:B (LIST :C)))`

Map the elements of XS with F then collect the results.


#### `TRACEOBJECT`
`∀ :A. (STRING → :A → UNIT)`

Print a line to *STANDARD-OUTPUT* in the form "{STR}: {ITEM}"



## File: [cell.lisp](cell.lisp)

### Types

#### `CELL :A`<a name="CELL"></a>
- `(CELL LISP-OBJECT)`

Internally mutable cell

Constructors:
`CELL :: (LISP-OBJECT → (CELL :A))`

Instances:
- [`EQ :A`](#EQ) `=>` [`EQ`](#EQ) [`(CELL :A)`](#CELL)
- [`NUM :A`](#NUM) `=>` [`NUM`](#NUM) [`(CELL :A)`](#CELL)
- [`SHOW :A`](#SHOW) `=>` [`SHOW`](#SHOW) [`(CELL :A)`](#CELL)
- [`FUNCTOR`](#FUNCTOR) [`CELL`](#CELL)
- [`SEMIGROUP :A`](#SEMIGROUP) `=>` [`SEMIGROUP`](#SEMIGROUP) [`(CELL :A)`](#CELL)
- [`APPLICATIVE`](#APPLICATIVE) [`CELL`](#CELL)


### Functions

#### `CELL-READ`
`∀ :A. ((CELL :A) → :A)`

Read the value of a mutable cell


#### `CELL-SWAP`
`∀ :A. (:A → (CELL :A) → :A)`

Replace the value of a mutable cell with a new value, then return the old value


#### `MAKE-CELL`
`∀ :A. (:A → (CELL :A))`

Create a new mutable cell


#### `CELL-WRITE`
`∀ :A. (:A → (CELL :A) → UNIT)`

Set the value of a mutable cell



## File: [vector.lisp](vector.lisp)

### Types

#### `VECTOR :A`<a name="VECTOR"></a>
- `(VECTOR LISP-OBJECT)`

Constructors:
`VECTOR :: (LISP-OBJECT → (VECTOR :A))`

Instances:
- [`EQ :A`](#EQ) `=>` [`EQ`](#EQ) [`(VECTOR :A)`](#VECTOR)
- [`ISO`](#ISO) [`(VECTOR :A)`](#VECTOR) [`(LIST :A)`](#LIST)
- [`INTO`](#INTO) [`(VECTOR :A)`](#VECTOR) [`(LIST :A)`](#LIST)
- [`INTO`](#INTO) [`(LIST :A)`](#LIST) [`(VECTOR :A)`](#VECTOR)
- [`FUNCTOR`](#FUNCTOR) [`VECTOR`](#VECTOR)
- [`SEMIGROUP`](#SEMIGROUP) [`(VECTOR :A)`](#VECTOR)


### Functions

#### `VECTOR-POP`
`∀ :A. ((VECTOR :A) → (OPTIONAL :A))`

Remove and return the first item of V


#### `VECTOR-SET`
`∀ :A. (INT → :A → (VECTOR :A) → UNIT)`

Set the INDEXth element of V to ITEM. This function left intentionally unsafe because it does not have a return value to check.


#### `MAKE-VECTOR`
`∀ :A. (UNIT → (VECTOR :A))`

Create a new empty vector


#### `VECTOR-HEAD`
`∀ :A. ((VECTOR :A) → (OPTIONAL :A))`

Return the first item of V


#### `VECTOR-LAST`
`∀ :A. ((VECTOR :A) → (OPTIONAL :A))`

Return the last element of V


#### `VECTOR-PUSH`
`∀ :A. (:A → (VECTOR :A) → UNIT)`

Append ITEM to V and resize V if necessary


#### `VECTOR-EMPTY`
`∀ :A. ((VECTOR :A) → BOOLEAN)`

Returns TRUE if V is empty


#### `VECTOR-INDEX`
`∀ :A. (INT → (VECTOR :A) → (OPTIONAL :A))`

Return the INDEXth element of V


#### `VECTOR-APPEND`
`∀ :A. ((VECTOR :A) → (VECTOR :A) → (VECTOR :A))`

Create a new VECTOR containing the elements of v1 followed by the elements of v2


#### `VECTOR-LENGTH`
`∀ :A. ((VECTOR :A) → INT)`

Returns the length of V


#### `VECTOR-FOREACH`
`∀ :A :B. ((:A → :B) → (VECTOR :A) → UNIT)`

Call the function F once for each item in V


#### `VECTOR-TO-LIST`
`∀ :A. ((VECTOR :A) → (LIST :A))`

Create a list containing the same elements in the same order as V


#### `VECTOR-CAPACITY`
`∀ :A. ((VECTOR :A) → INT)`

Returns the number of elements that V can store without resizing


#### `VECTOR-FOREACH2`
`∀ :A :B. ((:A → :A → :B) → (VECTOR :A) → (VECTOR :A) → UNIT)`

Like vector-foreach but twice as good


#### `VECTOR-POP-UNSAFE`
`∀ :A. ((VECTOR :A) → :A)`

Remove and return the first item of V without checking if the vector is empty


#### `VECTOR-HEAD-UNSAFE`
`∀ :A. ((VECTOR :A) → :A)`

Return the first item of V without first checking if V is empty


#### `VECTOR-LAST-UNSAFE`
`∀ :A. ((VECTOR :A) → :A)`

Return the last element of V without first checking if V is empty


#### `VECTOR-SWAP-REMOVE`
`∀ :A. (INT → (VECTOR :A) → (OPTIONAL :A))`

Remove the element IDX from VEC and replace it with the last element in VEC. Then return the removed element.


#### `VECTOR-INDEX-UNSAFE`
`∀ :A. (INT → (VECTOR :A) → :A)`

Return the INDEXth element of V without checking if the element exists


#### `MAKE-VECTOR-CAPACITY`
`∀ :A. (INT → (VECTOR :A))`

Create a new vector with N elements preallocated


#### `VECTOR-FOREACH-INDEX`
`∀ :A :B. ((INT → :A → :B) → (VECTOR :A) → UNIT)`

Call the function F once for each item in V with its index


#### `VECTOR-SWAP-REMOVE-UNSAFE`
`∀ :A. (INT → (VECTOR :A) → :A)`

Remove the element IDX from VEC and replace it with the last element in VEC without bounds checking. Then return the removed element.



## File: [graph.lisp](graph.lisp)

### Types

#### `EDGE :A`<a name="EDGE"></a>
- `(EDGE :A (INDEXPAIR (OPTIONAL EDGEINDEX)) (INDEXPAIR NODEINDEX))`

Constructors:
`EDGE :: (:A → (INDEXPAIR (OPTIONAL EDGEINDEX)) → (INDEXPAIR NODEINDEX) → (EDGE :A))`



#### `NODE :A`<a name="NODE"></a>
- `(NODE :A (INDEXPAIR (OPTIONAL EDGEINDEX)))`

Constructors:
`NODE :: (:A → (INDEXPAIR (OPTIONAL EDGEINDEX)) → (NODE :A))`



#### `GRAPH :A :B`<a name="GRAPH"></a>
- `(GRAPH GRAPHTYPE (VECTOR (NODE :A)) (VECTOR (EDGE :B)))`

A graph using adjacency list representation

Constructors:
`GRAPH :: (GRAPHTYPE → (VECTOR (NODE :A)) → (VECTOR (EDGE :B)) → (GRAPH :A :B))`



#### `EDGEINDEX`
- `(EDGEINDEX INT)`

Constructors:
`EDGEINDEX :: (INT → EDGEINDEX)`

Instances:
- [`EQ`](#EQ) [`EDGEINDEX`](#EDGEINDEX)
- [`INTO`](#INTO) [`EDGEINDEX`](#EDGEINDEX) [`INT`](#INT)
- [`SHOW`](#SHOW) [`EDGEINDEX`](#EDGEINDEX)


#### `GRAPHTYPE`
- `UNDIRECTED`
- `DIRECTED`

Constructors:
`UNDIRECTED :: GRAPHTYPE`
`DIRECTED :: GRAPHTYPE`



#### `INDEXPAIR :A`<a name="INDEXPAIR"></a>
- `(INDEXPAIR :A :A)`

A pair of indices representing the incoming and outgoing edges on a node.

Constructors:
`INDEXPAIR :: (:A → :A → (INDEXPAIR :A))`



#### `NODEINDEX`
- `(NODEINDEX INT)`

Constructors:
`NODEINDEX :: (INT → NODEINDEX)`

Instances:
- [`EQ`](#EQ) [`NODEINDEX`](#NODEINDEX)
- [`INTO`](#INTO) [`NODEINDEX`](#NODEINDEX) [`INT`](#INT)
- [`SHOW`](#SHOW) [`NODEINDEX`](#NODEINDEX)


#### `TARJANNODE`
- `(TARJANNODE (OPTIONAL INT) INT BOOLEAN)`

Constructors:
`TARJANNODE :: ((OPTIONAL INT) → INT → BOOLEAN → TARJANNODE)`



### Functions

#### `EDGE-DATA`
`∀ :A. ((EDGE :A) → :A)`

#### `GRAPH-VIZ`
`∀ :A :B. SHOW :A ⇒ ((GRAPH :A :B) → STRING)`

#### `NODE-DATA`
`∀ :A. ((NODE :A) → :A)`

#### `MAKE-GRAPH`
`∀ :A :B. (UNIT → (GRAPH :A :B))`

Create a new empty undirected graph


#### `TARJAN-SCC`
`∀ :A :B. ((GRAPH :A :B) → (LIST (LIST NODEINDEX)))`

#### `GRAPH-EDGES`
`∀ :A :B. ((GRAPH :A :B) → (VECTOR (EDGE :B)))`

Returns the edges in a graph


#### `GRAPH-NODES`
`∀ :A :B. ((GRAPH :A :B) → (VECTOR (NODE :A)))`

Returns the nodes in a graph


#### `MAKE-DIGRAPH`
`∀ :A :B. (UNIT → (GRAPH :A :B))`

Create a new directed graph


#### `EDGE-TO-INDEX`
`∀ :A. ((EDGE :A) → NODEINDEX)`

#### `EDGE-NEXT-PAIR`
`∀ :A. ((EDGE :A) → (INDEXPAIR (OPTIONAL EDGEINDEX)))`

#### `EDGE-NODE-PAIR`
`∀ :A. ((EDGE :A) → (INDEXPAIR NODEINDEX))`

#### `GRAPH-ADD-EDGE`
`∀ :A :B. (:A → NODEINDEX → NODEINDEX → (GRAPH :B :A) → EDGEINDEX)`

Add an edge with associated data from node FROM to node TO in the graph.


#### `GRAPH-ADD-NODE`
`∀ :A :B. (:A → (GRAPH :A :B) → NODEINDEX)`

Add a node with associated data to the graph, returning the index of the new node.


#### `NODE-EDGE-PAIR`
`∀ :A. ((NODE :A) → (INDEXPAIR (OPTIONAL EDGEINDEX)))`

Gets the IndexPair of the first incoming and outgoing edge for the node.


#### `EDGE-FROM-INDEX`
`∀ :A. ((EDGE :A) → NODEINDEX)`

#### `EDGE-INDEX-VALUE`
`(EDGEINDEX → INT)`

#### `GRAPH-EDGE-COUNT`
`∀ :A :B. ((GRAPH :A :B) → INT)`

Returns the number of edges in a graph


#### `GRAPH-NODE-COUNT`
`∀ :A :B. ((GRAPH :A :B) → INT)`

Returns the number of nodes in a graph


#### `NODE-INDEX-VALUE`
`(NODEINDEX → INT)`

#### `CHANGE-EDGE-LINKS`
`∀ :A :B. ((INDEXPAIR NODEINDEX) → EDGEINDEX → (INDEXPAIR (OPTIONAL EDGEINDEX)) → (GRAPH :A :B) → UNIT)`

#### `GRAPH-IS-DIRECTED`
`∀ :A :B. ((GRAPH :A :B) → BOOLEAN)`

#### `GRAPH-LOOKUP-EDGE`
`∀ :A :B. (EDGEINDEX → (GRAPH :A :B) → (OPTIONAL (EDGE :B)))`

Lookup a node with index IDX in graph G


#### `GRAPH-LOOKUP-NODE`
`∀ :A :B. (NODEINDEX → (GRAPH :A :B) → (OPTIONAL (NODE :A)))`

Lookup a node with index IDX in graph G


#### `GRAPH-REMOVE-EDGE`
`∀ :A :B. (EDGEINDEX → (GRAPH :A :B) → (OPTIONAL :B))`

Remove an edge from GRAPH


#### `GRAPH-REMOVE-NODE`
`∀ :A :B. (NODEINDEX → (GRAPH :A :B) → (OPTIONAL :A))`

Remove a node and all edges connecting to it from GRAPH


#### `TARJAN-NODE-INDEX`
`(TARJANNODE → (OPTIONAL INT))`

#### `INDEX-PAIR-INCOMING`
`∀ :A. ((INDEXPAIR :A) → :A)`

#### `INDEX-PAIR-OUTGOING`
`∀ :A. ((INDEXPAIR :A) → :A)`

#### `TARJAN-NODE-LOW-LINK`
`(TARJANNODE → INT)`

#### `TARJAN-NODE-ON-STACK`
`(TARJANNODE → BOOLEAN)`

#### `REMOVE-EDGE-ADJUST-INDICES`
`∀ :A :B. (EDGEINDEX → (GRAPH :A :B) → (OPTIONAL :B))`


