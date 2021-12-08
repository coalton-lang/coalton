# Reference for COALTON

## File: [types.lisp](../src/library/types.lisp)

### Types

#### <code>LIST :A</code> <sup><sub>[TYPE]</sub></sup><a name="LIST"></a>
- <code>CONS</code>
- <code>NIL</code>

Constructors:
- <code>CONS :: (LIST :B)</code>
- <code>NIL :: (LIST :B)</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ :C</a> => <a href="#EQ">EQ</a> <a href="#LIST">(LIST :C)</a></code>
- <code><a href="#ISO">ISO</a> <a href="#COALTON-LIBRARY:VECTOR">(COALTON-LIBRARY:VECTOR :C)</a> <a href="#LIST">(LIST :C)</a></code>
- <code><a href="#ISO">ISO</a> <a href="#LIST">(LIST CHAR)</a> <a href="#STRING">STRING</a></code>
- <code><a href="#INTO">INTO</a> <a href="#COALTON-LIBRARY:VECTOR">(COALTON-LIBRARY:VECTOR :C)</a> <a href="#LIST">(LIST :C)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#LIST">(LIST :C)</a> <a href="#COALTON-LIBRARY:VECTOR">(COALTON-LIBRARY:VECTOR :C)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#LIST">(LIST CHAR)</a> <a href="#STRING">STRING</a></code>
- <code><a href="#INTO">INTO</a> <a href="#STRING">STRING</a> <a href="#LIST">(LIST CHAR)</a></code>
- <code><a href="#MONAD">MONAD</a> <a href="#LIST">LIST</a></code>
- <code><a href="#MONOID">MONOID</a> <a href="#LIST">(LIST :C)</a></code>
- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#LIST">LIST</a></code>
- <code><a href="#SEMIGROUP">SEMIGROUP</a> <a href="#LIST">(LIST :C)</a></code>
- <code><a href="#ALTERNATIVE">ALTERNATIVE</a> <a href="#LIST">LIST</a></code>
- <code><a href="#APPLICATIVE">APPLICATIVE</a> <a href="#LIST">LIST</a></code>

</details>


***

#### <code>UNIT</code> <sup><sub>[TYPE]</sub></sup><a name="UNIT"></a>
- <code>UNIT</code>

Constructors:
- <code>UNIT :: UNIT</code>


***

#### <code>BOOLEAN</code> <sup><sub>[TYPE]</sub></sup><a name="BOOLEAN"></a>
- <code>FALSE</code>
- <code>TRUE</code>

Constructors:
- <code>FALSE :: BOOLEAN</code>
- <code>TRUE :: BOOLEAN</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> <a href="#BOOLEAN">BOOLEAN</a></code>
- <code><a href="#ORD">ORD</a> <a href="#BOOLEAN">BOOLEAN</a></code>

</details>


***

### Functions

#### <code>NIL</code> <sup><sub>[FUNCTION]</sub></sup><a name="NIL"></a>
<code>∀ :A. (LIST :A)</code>

***

#### <code>CONS</code> <sup><sub>[FUNCTION]</sub></sup><a name="CONS"></a>
<code>∀ :A. (:A → (LIST :A) → (LIST :A))</code>

***

#### <code>TRUE</code> <sup><sub>[FUNCTION]</sub></sup><a name="TRUE"></a>
<code>BOOLEAN</code>

***

#### <code>FALSE</code> <sup><sub>[FUNCTION]</sub></sup><a name="FALSE"></a>
<code>BOOLEAN</code>

***

# Reference for COALTON-LIBRARY

## File: [types.lisp](../src/library/types.lisp)

### Types

#### <code>TUPLE :A :B</code> <sup><sub>[TYPE]</sub></sup><a name="TUPLE"></a>
- <code>(TUPLE :A :B)</code>

A heterogeneous collection of items.

Constructors:
- <code>TUPLE :: (:A → :B → (TUPLE :A :B))</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ :C</a> <a href="#EQ">EQ :D</a> => <a href="#EQ">EQ</a> <a href="#TUPLE">(TUPLE :C :D)</a></code>
- <code><a href="#ISO">ISO</a> <a href="#TUPLE">(TUPLE :C :D)</a> <a href="#TUPLE">(TUPLE :D :C)</a></code>
- <code><a href="#ORD">ORD :C</a> <a href="#ORD">ORD :D</a> => <a href="#ORD">ORD</a> <a href="#TUPLE">(TUPLE :C :D)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#TUPLE">(TUPLE :C :D)</a> <a href="#TUPLE">(TUPLE :D :C)</a></code>

</details>


***

#### <code>RESULT :A :B</code> <sup><sub>[TYPE]</sub></sup><a name="RESULT"></a>
- <code>(ERR :A)</code>
- <code>(OK :B)</code>

Represents something that may have failed.

Constructors:
- <code>ERR :: (:A → (RESULT :A :B))</code>
- <code>OK :: (:B → (RESULT :A :B))</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ :C</a> <a href="#EQ">EQ :D</a> => <a href="#EQ">EQ</a> <a href="#RESULT">(RESULT :C :D)</a></code>
- <code><a href="#ISO">ISO</a> <a href="#RESULT">(RESULT UNIT :C)</a> <a href="#OPTIONAL">(OPTIONAL :C)</a></code>
- <code><a href="#ORD">ORD :C</a> <a href="#ORD">ORD :D</a> => <a href="#ORD">ORD</a> <a href="#RESULT">(RESULT :C :D)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#RESULT">(RESULT :C :D)</a> <a href="#OPTIONAL">(OPTIONAL :D)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#OPTIONAL">(OPTIONAL :C)</a> <a href="#RESULT">(RESULT UNIT :C)</a></code>
- <code><a href="#MONAD">MONAD</a> <a href="#RESULT">(RESULT :C)</a></code>
- <code><a href="#MONOID">MONOID :C</a> => <a href="#MONOID">MONOID</a> <a href="#RESULT">(RESULT :D :C)</a></code>
- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#RESULT">(RESULT :C)</a></code>
- <code><a href="#SEMIGROUP">SEMIGROUP :C</a> => <a href="#SEMIGROUP">SEMIGROUP</a> <a href="#RESULT">(RESULT :D :C)</a></code>
- <code><a href="#APPLICATIVE">APPLICATIVE</a> <a href="#RESULT">(RESULT :C)</a></code>
- <code><a href="#UNWRAPPABLE">UNWRAPPABLE</a> <a href="#RESULT">(RESULT :C)</a></code>

</details>


***

#### <code>TUPLE3 :A :B :C</code> <sup><sub>[TYPE]</sub></sup><a name="TUPLE3"></a>
- <code>(TUPLE3 :A :B :C)</code>

Constructors:
- <code>TUPLE3 :: (:A → :B → :C → (TUPLE3 :A :B :C))</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ :D</a> <a href="#EQ">EQ :E</a> <a href="#EQ">EQ :F</a> => <a href="#EQ">EQ</a> <a href="#TUPLE3">(TUPLE3 :D :E :F)</a></code>

</details>


***

#### <code>TUPLE4 :A :B :C :D</code> <sup><sub>[TYPE]</sub></sup><a name="TUPLE4"></a>
- <code>(TUPLE4 :A :B :C :D)</code>

Constructors:
- <code>TUPLE4 :: (:A → :B → :C → :D → (TUPLE4 :A :B :C :D))</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ :E</a> <a href="#EQ">EQ :F</a> <a href="#EQ">EQ :G</a> <a href="#EQ">EQ :H</a> => <a href="#EQ">EQ</a> <a href="#TUPLE4">(TUPLE4 :E :F :G :H)</a></code>

</details>


***

#### <code>TUPLE5 :A :B :C :D :E</code> <sup><sub>[TYPE]</sub></sup><a name="TUPLE5"></a>
- <code>(TUPLE5 :A :B :C :D :E)</code>

Constructors:
- <code>TUPLE5 :: (:A → :B → :C → :D → :E → (TUPLE5 :A :B :C :D :E))</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ :F</a> <a href="#EQ">EQ :G</a> <a href="#EQ">EQ :H</a> <a href="#EQ">EQ :I</a> <a href="#EQ">EQ :J</a> => <a href="#EQ">EQ</a> <a href="#TUPLE5">(TUPLE5 :F :G :H :I :J)</a></code>

</details>


***

#### <code>FRACTION</code> <sup><sub>[TYPE]</sub></sup><a name="FRACTION"></a>
- <code>(%FRACTION INTEGER INTEGER)</code>

A ratio of integers always in reduced form.

Constructors:
- <code>%FRACTION :: (INTEGER → INTEGER → FRACTION)</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> <a href="#FRACTION">FRACTION</a></code>
- <code><a href="#NUM">NUM</a> <a href="#FRACTION">FRACTION</a></code>
- <code><a href="#ORD">ORD</a> <a href="#FRACTION">FRACTION</a></code>
- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#INTEGER">INTEGER</a> <a href="#FRACTION">FRACTION</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#FRACTION">FRACTION</a></code>

</details>


***

#### <code>OPTIONAL :A</code> <sup><sub>[TYPE]</sub></sup><a name="OPTIONAL"></a>
- <code>(SOME :A)</code>
- <code>NONE</code>

Represents something that may not have a value.

Constructors:
- <code>SOME :: (:A → (OPTIONAL :A))</code>
- <code>NONE :: (OPTIONAL :A)</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ :B</a> => <a href="#EQ">EQ</a> <a href="#OPTIONAL">(OPTIONAL :B)</a></code>
- <code><a href="#ISO">ISO</a> <a href="#RESULT">(RESULT UNIT :B)</a> <a href="#OPTIONAL">(OPTIONAL :B)</a></code>
- <code><a href="#ORD">ORD :B</a> => <a href="#ORD">ORD</a> <a href="#OPTIONAL">(OPTIONAL :B)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#RESULT">(RESULT :B :C)</a> <a href="#OPTIONAL">(OPTIONAL :C)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#OPTIONAL">(OPTIONAL :B)</a> <a href="#RESULT">(RESULT UNIT :B)</a></code>
- <code><a href="#MONAD">MONAD</a> <a href="#OPTIONAL">OPTIONAL</a></code>
- <code><a href="#MONOID">MONOID :B</a> => <a href="#MONOID">MONOID</a> <a href="#OPTIONAL">(OPTIONAL :B)</a></code>
- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#OPTIONAL">OPTIONAL</a></code>
- <code><a href="#MONADFAIL">MONADFAIL</a> <a href="#OPTIONAL">OPTIONAL</a></code>
- <code><a href="#SEMIGROUP">SEMIGROUP :B</a> => <a href="#SEMIGROUP">SEMIGROUP</a> <a href="#OPTIONAL">(OPTIONAL :B)</a></code>
- <code><a href="#ALTERNATIVE">ALTERNATIVE</a> <a href="#OPTIONAL">OPTIONAL</a></code>
- <code><a href="#APPLICATIVE">APPLICATIVE</a> <a href="#OPTIONAL">OPTIONAL</a></code>
- <code><a href="#UNWRAPPABLE">UNWRAPPABLE</a> <a href="#OPTIONAL">OPTIONAL</a></code>

</details>


***

### Functions

#### <code>NOT</code> <sup><sub>[FUNCTION]</sub></sup><a name="NOT"></a>
<code>(BOOLEAN → BOOLEAN)</code>

Synonym for BOOLEAN-NOT.


***

#### <code>XOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="XOR"></a>
<code>(BOOLEAN → BOOLEAN → BOOLEAN)</code>

Synonym for BOOLEAN-XOR.


***

#### <code>BOOLEAN-OR</code> <sup><sub>[FUNCTION]</sub></sup><a name="BOOLEAN-OR"></a>
<code>(BOOLEAN → BOOLEAN → BOOLEAN)</code>

Is X or Y True? Note that this is a *function* which means both X and Y will be evaluated. Use the OR macro for short-circuiting behavior.


***

#### <code>BOOLEAN-AND</code> <sup><sub>[FUNCTION]</sub></sup><a name="BOOLEAN-AND"></a>
<code>(BOOLEAN → BOOLEAN → BOOLEAN)</code>

Are X and Y True? Note that this is a *function* which means both X and Y will be evaluated. Use the AND macro for short-circuiting behavior.


***

#### <code>BOOLEAN-NOT</code> <sup><sub>[FUNCTION]</sub></sup><a name="BOOLEAN-NOT"></a>
<code>(BOOLEAN → BOOLEAN)</code>

Is X False?


***

#### <code>BOOLEAN-XOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="BOOLEAN-XOR"></a>
<code>(BOOLEAN → BOOLEAN → BOOLEAN)</code>

Are X or Y True, but not both?


***

## File: [classes.lisp](../src/library/classes.lisp)

### Types

#### <code>ORD</code> <sup><sub>[TYPE]</sub></sup><a name="ORD"></a>
- <code>LT</code>
- <code>GT</code>
- <code>EQ</code>

Constructors:
- <code>LT :: ORD</code>
- <code>GT :: ORD</code>
- <code>EQ :: ORD</code>


***

#### <code>QUANTIZATION :A</code> <sup><sub>[TYPE]</sub></sup><a name="QUANTIZATION"></a>
- <code>(QUANTIZATION :A INTEGER :A INTEGER :A)</code>

Represents an integer quantization of `:t`. See the `Quantizable` typeclass.

The fields are defined as follows:

1. A value of type `:t`.

2. The greatest integer less than or equal to a particular value.

3. The remainder of this as a value of type `:t`.

4. The least integer greater than or equal to a particular value.

5. The remainder of this as a value of type `:t`.


Constructors:
- <code>QUANTIZATION :: (:A → INTEGER → :A → INTEGER → :A → (QUANTIZATION :A))</code>


***

### Classes

#### <code>EQ</code> <sup><sub>[CLASS]</sub></sup><a name="EQ"></a>
<code><a href="#EQ">EQ</a> <a href="#:A">:A</a></code>

Types which have equality defined.

Methods:
- <code>== :: (:A → :A → BOOLEAN)</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> <a href="#BOOLEAN">BOOLEAN</a></code>
- <code><a href="#EQ">EQ</a> <a href="#I32">I32</a></code>
- <code><a href="#EQ">EQ</a> <a href="#I64">I64</a></code>
- <code><a href="#EQ">EQ</a> <a href="#U8">U8</a></code>
- <code><a href="#EQ">EQ</a> <a href="#U32">U32</a></code>
- <code><a href="#EQ">EQ</a> <a href="#U64">U64</a></code>
- <code><a href="#EQ">EQ</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#EQ">EQ</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#EQ">EQ</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#EQ">EQ</a> <a href="#FRACTION">FRACTION</a></code>
- <code><a href="#EQ">EQ</a> <a href="#CHAR">CHAR</a></code>
- <code><a href="#EQ">EQ</a> <a href="#STRING">STRING</a></code>
- <code><a href="#EQ">EQ :A</a> => <a href="#EQ">EQ</a> <a href="#OPTIONAL">(OPTIONAL :A)</a></code>
- <code><a href="#EQ">EQ :A</a> => <a href="#EQ">EQ</a> <a href="#LIST">(LIST :A)</a></code>
- <code><a href="#EQ">EQ :A</a> <a href="#EQ">EQ :B</a> => <a href="#EQ">EQ</a> <a href="#TUPLE">(TUPLE :A :B)</a></code>
- <code><a href="#EQ">EQ :A</a> <a href="#EQ">EQ :B</a> <a href="#EQ">EQ :C</a> => <a href="#EQ">EQ</a> <a href="#TUPLE3">(TUPLE3 :A :B :C)</a></code>
- <code><a href="#EQ">EQ :A</a> <a href="#EQ">EQ :B</a> <a href="#EQ">EQ :C</a> <a href="#EQ">EQ :D</a> <a href="#EQ">EQ :E</a> => <a href="#EQ">EQ</a> <a href="#TUPLE5">(TUPLE5 :A :B :C :D :E)</a></code>
- <code><a href="#EQ">EQ :A</a> <a href="#EQ">EQ :B</a> <a href="#EQ">EQ :C</a> <a href="#EQ">EQ :D</a> => <a href="#EQ">EQ</a> <a href="#TUPLE4">(TUPLE4 :A :B :C :D)</a></code>
- <code><a href="#EQ">EQ :A</a> <a href="#EQ">EQ :B</a> => <a href="#EQ">EQ</a> <a href="#RESULT">(RESULT :A :B)</a></code>
- <code><a href="#EQ">EQ :A</a> => <a href="#EQ">EQ</a> <a href="#CELL">(CELL :A)</a></code>
- <code><a href="#EQ">EQ :A</a> => <a href="#EQ">EQ</a> <a href="#VECTOR">(VECTOR :A)</a></code>
- <code><a href="#EQ">EQ :A</a> => <a href="#EQ">EQ</a> <a href="#SLICE">(SLICE :A)</a></code>
- <code><a href="#EQ">EQ</a> <a href="#EDGEINDEX">EDGEINDEX</a></code>
- <code><a href="#EQ">EQ</a> <a href="#NODEINDEX">NODEINDEX</a></code>

</details>


***

#### <code>NUM</code> <sup><sub>[CLASS]</sub></sup><a name="NUM"></a>
<code><a href="#EQ">EQ :A</a> => <a href="#NUM">NUM</a> <a href="#:A">:A</a></code>

Types which have numeric operations defined.

Methods:
- <code>+ :: (:A → :A → :A)</code>
- <code>- :: (:A → :A → :A)</code>
- <code>* :: (:A → :A → :A)</code>
- <code>FROMINT :: (INTEGER → :A)</code>

<details>
<summary>Instances</summary>

- <code><a href="#NUM">NUM</a> <a href="#I64">I64</a></code>
- <code><a href="#NUM">NUM</a> <a href="#I32">I32</a></code>
- <code><a href="#NUM">NUM</a> <a href="#U8">U8</a></code>
- <code><a href="#NUM">NUM</a> <a href="#U32">U32</a></code>
- <code><a href="#NUM">NUM</a> <a href="#U64">U64</a></code>
- <code><a href="#NUM">NUM</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#NUM">NUM</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#NUM">NUM</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#NUM">NUM</a> <a href="#FRACTION">FRACTION</a></code>
- <code><a href="#NUM">NUM :A</a> => <a href="#NUM">NUM</a> <a href="#CELL">(CELL :A)</a></code>

</details>


***

#### <code>ORD</code> <sup><sub>[CLASS]</sub></sup><a name="ORD"></a>
<code><a href="#EQ">EQ :A</a> => <a href="#ORD">ORD</a> <a href="#:A">:A</a></code>

Types whose values can be ordered.

Methods:
- <code><=> :: (:A → :A → ORD)</code>

<details>
<summary>Instances</summary>

- <code><a href="#ORD">ORD</a> <a href="#BOOLEAN">BOOLEAN</a></code>
- <code><a href="#ORD">ORD</a> <a href="#I32">I32</a></code>
- <code><a href="#ORD">ORD</a> <a href="#I64">I64</a></code>
- <code><a href="#ORD">ORD</a> <a href="#U8">U8</a></code>
- <code><a href="#ORD">ORD</a> <a href="#U32">U32</a></code>
- <code><a href="#ORD">ORD</a> <a href="#U64">U64</a></code>
- <code><a href="#ORD">ORD</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#ORD">ORD</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#ORD">ORD</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#ORD">ORD</a> <a href="#FRACTION">FRACTION</a></code>
- <code><a href="#ORD">ORD</a> <a href="#CHAR">CHAR</a></code>
- <code><a href="#ORD">ORD</a> <a href="#STRING">STRING</a></code>
- <code><a href="#ORD">ORD :A</a> => <a href="#ORD">ORD</a> <a href="#OPTIONAL">(OPTIONAL :A)</a></code>
- <code><a href="#ORD">ORD :A</a> <a href="#ORD">ORD :B</a> => <a href="#ORD">ORD</a> <a href="#TUPLE">(TUPLE :A :B)</a></code>
- <code><a href="#ORD">ORD :A</a> <a href="#ORD">ORD :B</a> => <a href="#ORD">ORD</a> <a href="#RESULT">(RESULT :A :B)</a></code>

</details>


***

#### <code>INTO</code> <sup><sub>[CLASS]</sub></sup><a name="INTO"></a>
<code><a href="#INTO">INTO</a> <a href="#:A">:A</a> <a href="#:B">:B</a></code>

INTO imples *every* element of :FROM can be represented by an element of :TO. This conversion might not be injective (i.e., there may be elements in :TO that don't correspond to any in :FROM).

Methods:
- <code>INTO :: (:A → :B)</code>

<details>
<summary>Instances</summary>

- <code><a href="#INTO">INTO</a> <a href="#:A">:A</a> <a href="#:A">:A</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#I32">I32</a></code>
- <code><a href="#INTO">INTO</a> <a href="#I32">I32</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#I64">I64</a></code>
- <code><a href="#INTO">INTO</a> <a href="#I64">I64</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U8">U8</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U8">U8</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#U8">U8</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U8">U8</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U32">U32</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U32">U32</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U32">U32</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#U32">U32</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U64">U64</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#U64">U64</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U64">U64</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U64">U64</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#STRING">STRING</a></code>
- <code><a href="#INTO">INTO</a> <a href="#STRING">STRING</a> <a href="#LIST">(LIST CHAR)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#LIST">(LIST CHAR)</a> <a href="#STRING">STRING</a></code>
- <code><a href="#INTO">INTO</a> <a href="#TUPLE">(TUPLE :A :B)</a> <a href="#TUPLE">(TUPLE :B :A)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#OPTIONAL">(OPTIONAL :A)</a> <a href="#RESULT">(RESULT UNIT :A)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#RESULT">(RESULT :A :B)</a> <a href="#OPTIONAL">(OPTIONAL :B)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#:A">:A</a> <a href="#CELL">(CELL :A)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#CELL">(CELL :A)</a> <a href="#:A">:A</a></code>
- <code><a href="#INTO">INTO</a> <a href="#LIST">(LIST :A)</a> <a href="#VECTOR">(VECTOR :A)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#VECTOR">(VECTOR :A)</a> <a href="#LIST">(LIST :A)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#SLICE">(SLICE :A)</a> <a href="#VECTOR">(VECTOR :A)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#VECTOR">(VECTOR :A)</a> <a href="#SLICE">(SLICE :A)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#EDGEINDEX">EDGEINDEX</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#NODEINDEX">NODEINDEX</a> <a href="#INTEGER">INTEGER</a></code>

</details>


***

#### <code>MONAD</code> <sup><sub>[CLASS]</sub></sup><a name="MONAD"></a>
<code><a href="#APPLICATIVE">APPLICATIVE :A</a> => <a href="#MONAD">MONAD</a> <a href="#:A">:A</a></code>

Types which are monads as defined in Haskell. See https://wiki.haskell.org/Monad for more information.

Methods:
- <code>>>= :: ∀ :B :C. ((:A :B) → (:B → (:A :C)) → (:A :C))</code>
- <code>>> :: ∀ :B :C. ((:A :B) → (:A :C) → (:A :C))</code>

<details>
<summary>Instances</summary>

- <code><a href="#MONAD">MONAD</a> <a href="#OPTIONAL">OPTIONAL</a></code>
- <code><a href="#MONAD">MONAD</a> <a href="#LIST">LIST</a></code>
- <code><a href="#MONAD">MONAD</a> <a href="#RESULT">(RESULT :A)</a></code>

</details>


***

#### <code>MONOID</code> <sup><sub>[CLASS]</sub></sup><a name="MONOID"></a>
<code><a href="#SEMIGROUP">SEMIGROUP :A</a> => <a href="#MONOID">MONOID</a> <a href="#:A">:A</a></code>

Types with an associative binary operation and identity defined.

Methods:
- <code>MEMPTY :: :A</code>

<details>
<summary>Instances</summary>

- <code><a href="#MONOID">MONOID</a> <a href="#STRING">STRING</a></code>
- <code><a href="#MONOID">MONOID :A</a> => <a href="#MONOID">MONOID</a> <a href="#OPTIONAL">(OPTIONAL :A)</a></code>
- <code><a href="#MONOID">MONOID</a> <a href="#LIST">(LIST :A)</a></code>
- <code><a href="#MONOID">MONOID :A</a> => <a href="#MONOID">MONOID</a> <a href="#RESULT">(RESULT :B :A)</a></code>

</details>


***

#### <code>FUNCTOR</code> <sup><sub>[CLASS]</sub></sup><a name="FUNCTOR"></a>
<code><a href="#FUNCTOR">FUNCTOR</a> <a href="#:A">:A</a></code>

Types which can map an inner type where the mapping adheres to the identity and composition laws.

Methods:
- <code>MAP :: ∀ :B :C. ((:B → :C) → (:A :B) → (:A :C))</code>

<details>
<summary>Instances</summary>

- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#OPTIONAL">OPTIONAL</a></code>
- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#LIST">LIST</a></code>
- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#RESULT">(RESULT :A)</a></code>
- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#CELL">CELL</a></code>
- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#VECTOR">VECTOR</a></code>

</details>


***

#### <code>TRYINTO</code> <sup><sub>[CLASS]</sub></sup><a name="TRYINTO"></a>
<code><a href="#TRYINTO">TRYINTO</a> <a href="#:A">:A</a> <a href="#:B">:B</a></code>

TRY-INTO implies *most* elements of :FROM can be represented exactly by an element of :TO, but sometimes not. If not, an error string is returned.

Methods:
- <code>TRYINTO :: (:A → (RESULT STRING :B))</code>

<details>
<summary>Instances</summary>

- <code><a href="#TRYINTO">TRYINTO</a> <a href="#STRING">STRING</a> <a href="#INTEGER">INTEGER</a></code>

</details>


***

#### <code>DIVIDABLE</code> <sup><sub>[CLASS]</sub></sup><a name="DIVIDABLE"></a>
<code><a href="#NUM">NUM :A</a> <a href="#NUM">NUM :B</a> => <a href="#DIVIDABLE">DIVIDABLE</a> <a href="#:A">:A</a> <a href="#:B">:B</a></code>

The representation of a type such that division within that type possibly results in another type. For instance,


    (Dividable Integer Fraction)


establishes that division of two `Integer`s can result in a `Fraction`, whereas


    (Dividable Single-Float Single-Float)


establishes that division of two `Single-Float`s can result in a `Single-Float`.

Note that `Dividable` does *not* establish a default result type; you must constrain the result type yourself.

The function / is partial, and will error produce a run-time error if the divisor is zero.


Methods:
- <code>/ :: (:A → :A → :B)</code>

<details>
<summary>Instances</summary>

- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#INTEGER">INTEGER</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#INTEGER">INTEGER</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#INTEGER">INTEGER</a> <a href="#FRACTION">FRACTION</a></code>
- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>

</details>


***

#### <code>MONADFAIL</code> <sup><sub>[CLASS]</sub></sup><a name="MONADFAIL"></a>
<code><a href="#MONAD">MONAD :A</a> => <a href="#MONADFAIL">MONADFAIL</a> <a href="#:A">:A</a></code>

Methods:
- <code>FAIL :: ∀ :B. (STRING → (:A :B))</code>

<details>
<summary>Instances</summary>

- <code><a href="#MONADFAIL">MONADFAIL</a> <a href="#OPTIONAL">OPTIONAL</a></code>

</details>


***

#### <code>SEMIGROUP</code> <sup><sub>[CLASS]</sub></sup><a name="SEMIGROUP"></a>
<code><a href="#SEMIGROUP">SEMIGROUP</a> <a href="#:A">:A</a></code>

Types with an associative binary operation defined.

Methods:
- <code><> :: (:A → :A → :A)</code>

<details>
<summary>Instances</summary>

- <code><a href="#SEMIGROUP">SEMIGROUP</a> <a href="#STRING">STRING</a></code>
- <code><a href="#SEMIGROUP">SEMIGROUP :A</a> => <a href="#SEMIGROUP">SEMIGROUP</a> <a href="#OPTIONAL">(OPTIONAL :A)</a></code>
- <code><a href="#SEMIGROUP">SEMIGROUP</a> <a href="#LIST">(LIST :A)</a></code>
- <code><a href="#SEMIGROUP">SEMIGROUP :A</a> => <a href="#SEMIGROUP">SEMIGROUP</a> <a href="#RESULT">(RESULT :B :A)</a></code>
- <code><a href="#SEMIGROUP">SEMIGROUP :A</a> => <a href="#SEMIGROUP">SEMIGROUP</a> <a href="#CELL">(CELL :A)</a></code>
- <code><a href="#SEMIGROUP">SEMIGROUP</a> <a href="#VECTOR">(VECTOR :A)</a></code>

</details>


***

#### <code>APPLICATIVE</code> <sup><sub>[CLASS]</sub></sup><a name="APPLICATIVE"></a>
<code><a href="#FUNCTOR">FUNCTOR :A</a> => <a href="#APPLICATIVE">APPLICATIVE</a> <a href="#:A">:A</a></code>

Types which are a functor which can embed pure expressions and sequence operations.

Methods:
- <code>PURE :: ∀ :B. (:B → (:A :B))</code>
- <code>LIFTA2 :: ∀ :B :C :D. ((:B → :C → :D) → (:A :B) → (:A :C) → (:A :D))</code>

<details>
<summary>Instances</summary>

- <code><a href="#APPLICATIVE">APPLICATIVE</a> <a href="#OPTIONAL">OPTIONAL</a></code>
- <code><a href="#APPLICATIVE">APPLICATIVE</a> <a href="#LIST">LIST</a></code>
- <code><a href="#APPLICATIVE">APPLICATIVE</a> <a href="#RESULT">(RESULT :A)</a></code>
- <code><a href="#APPLICATIVE">APPLICATIVE</a> <a href="#CELL">CELL</a></code>

</details>


***

#### <code>QUANTIZABLE</code> <sup><sub>[CLASS]</sub></sup><a name="QUANTIZABLE"></a>
<code><a href="#ORD">ORD :A</a> <a href="#NUM">NUM :A</a> => <a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#:A">:A</a></code>

The representation of a type that allows "quantizing", "snapping to integers", or "rounding." (All of these concepts are roughly equivalent.)


Methods:
- <code>QUANTIZE :: (:A → (QUANTIZATION :A))</code>

<details>
<summary>Instances</summary>

- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#I32">I32</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#I64">I64</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#U64">U64</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#U32">U32</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#U8">U8</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#FRACTION">FRACTION</a></code>

</details>


***

### Functions

#### <code><</code> <sup><sub>[FUNCTION]</sub></sup><a name="<"></a>
<code>∀ :A. ORD :A ⇒ (:A → :A → BOOLEAN)</code>

Is X less than Y?


***

#### <code>></code> <sup><sub>[FUNCTION]</sub></sup><a name=">"></a>
<code>∀ :A. ORD :A ⇒ (:A → :A → BOOLEAN)</code>

Is X greater than Y?


***

#### <code>/=</code> <sup><sub>[FUNCTION]</sub></sup><a name="/="></a>
<code>∀ :A. EQ :A ⇒ (:A → :A → BOOLEAN)</code>

***

#### <code><=</code> <sup><sub>[FUNCTION]</sub></sup><a name="<="></a>
<code>∀ :A. ORD :A ⇒ (:A → :A → BOOLEAN)</code>

Is X less than or equal to Y?


***

#### <code>>=</code> <sup><sub>[FUNCTION]</sub></sup><a name=">="></a>
<code>∀ :A. ORD :A ⇒ (:A → :A → BOOLEAN)</code>

Is X greater than or equal to Y?


***

#### <code>MAX</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAX"></a>
<code>∀ :A. ORD :A ⇒ (:A → :A → :A)</code>

Returns the greater element of X and Y.


***

#### <code>MIN</code> <sup><sub>[FUNCTION]</sub></sup><a name="MIN"></a>
<code>∀ :A. ORD :A ⇒ (:A → :A → :A)</code>

Returns the lesser element of X and Y.


***

## File: [builtin.lisp](../src/library/builtin.lisp)

### Functions

#### <code>ERROR</code> <sup><sub>[FUNCTION]</sub></sup><a name="ERROR"></a>
<code>∀ :A. (STRING → :A)</code>

Signal an error by calling CL:ERROR


***

#### <code>UNDEFINED</code> <sup><sub>[FUNCTION]</sub></sup><a name="UNDEFINED"></a>
<code>∀ :A :B. (:A → :B)</code>

A function which can be used in place of any value, throwing an error at runtime.


***

## File: [fraction.lisp](../src/library/fraction.lisp)

### Functions

#### <code>NUMERATOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="NUMERATOR"></a>
<code>(FRACTION → INTEGER)</code>

The numerator of a fraction Q.


***

#### <code>DENOMINATOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="DENOMINATOR"></a>
<code>(FRACTION → INTEGER)</code>

The denominator of a fraction Q.


***

## File: [arith.lisp](../src/library/arith.lisp)

### Functions

#### <code>ABS</code> <sup><sub>[FUNCTION]</sub></sup><a name="ABS"></a>
<code>∀ :A. (NUM :A) (ORD :A) ⇒ (:A → :A)</code>

Absolute value of X.


***

#### <code>ASH</code> <sup><sub>[FUNCTION]</sub></sup><a name="ASH"></a>
<code>(INTEGER → INTEGER → INTEGER)</code>

Compute the "arithmetic shift" of X by N. 


***

#### <code>GCD</code> <sup><sub>[FUNCTION]</sub></sup><a name="GCD"></a>
<code>(INTEGER → INTEGER → INTEGER)</code>

Compute the greatest common divisor of A and B.


***

#### <code>LCM</code> <sup><sub>[FUNCTION]</sub></sup><a name="LCM"></a>
<code>(INTEGER → INTEGER → INTEGER)</code>

Compute the least common multiple of A and B.


***

#### <code>MOD</code> <sup><sub>[FUNCTION]</sub></sup><a name="MOD"></a>
<code>(INTEGER → INTEGER → INTEGER)</code>

Compute NUM modulo BASE.


***

#### <code>ODD</code> <sup><sub>[FUNCTION]</sub></sup><a name="ODD"></a>
<code>(INTEGER → BOOLEAN)</code>

Is N odd?


***

#### <code>EVEN</code> <sup><sub>[FUNCTION]</sub></sup><a name="EVEN"></a>
<code>(INTEGER → BOOLEAN)</code>

Is N even?


***

#### <code>EXPT</code> <sup><sub>[FUNCTION]</sub></sup><a name="EXPT"></a>
<code>(INTEGER → INTEGER → INTEGER)</code>

Exponentiate BASE to a non-negative POWER.


***

#### <code>SIGN</code> <sup><sub>[FUNCTION]</sub></sup><a name="SIGN"></a>
<code>∀ :A. (NUM :A) (ORD :A) ⇒ (:A → INTEGER)</code>

The sign of X.


***

#### <code>NEGATE</code> <sup><sub>[FUNCTION]</sub></sup><a name="NEGATE"></a>
<code>∀ :A. NUM :A ⇒ (:A → :A)</code>

***

#### <code>INTEGER->STRING</code> <sup><sub>[FUNCTION]</sub></sup><a name="INTEGER->STRING"></a>
<code>(INTEGER → STRING)</code>

***

#### <code>DOUBLE-FLOAT->INTEGER</code> <sup><sub>[FUNCTION]</sub></sup><a name="DOUBLE-FLOAT->INTEGER"></a>
<code>(DOUBLE-FLOAT → (OPTIONAL INTEGER))</code>

Round a Double-Float to the nearest Integer.


***

#### <code>INTEGER->DOUBLE-FLOAT</code> <sup><sub>[FUNCTION]</sub></sup><a name="INTEGER->DOUBLE-FLOAT"></a>
<code>(INTEGER → DOUBLE-FLOAT)</code>

***

#### <code>INTEGER->SINGLE-FLOAT</code> <sup><sub>[FUNCTION]</sub></sup><a name="INTEGER->SINGLE-FLOAT"></a>
<code>(INTEGER → SINGLE-FLOAT)</code>

***

#### <code>SINGLE-FLOAT->INTEGER</code> <sup><sub>[FUNCTION]</sub></sup><a name="SINGLE-FLOAT->INTEGER"></a>
<code>(SINGLE-FLOAT → (OPTIONAL INTEGER))</code>

Round a Single-Float to the nearest Integer.


***

## File: [string.lisp](../src/library/string.lisp)

### Functions

#### <code>PARSE-INT</code> <sup><sub>[FUNCTION]</sub></sup><a name="PARSE-INT"></a>
<code>(STRING → (OPTIONAL INTEGER))</code>

Parse the integer in string STR.


***

#### <code>PACK-STRING</code> <sup><sub>[FUNCTION]</sub></sup><a name="PACK-STRING"></a>
<code>((LIST CHAR) → STRING)</code>

Pack a list of charactes into a string.


***

#### <code>CONCAT-STRING</code> <sup><sub>[FUNCTION]</sub></sup><a name="CONCAT-STRING"></a>
<code>(STRING → STRING → STRING)</code>

Concatenate STR1 and STR2 together, returning a new string.


***

#### <code>UNPACK-STRING</code> <sup><sub>[FUNCTION]</sub></sup><a name="UNPACK-STRING"></a>
<code>(STRING → (LIST CHAR))</code>

Unpack a string into a list of characters.


***

## File: [optional.lisp](../src/library/optional.lisp)

### Functions

#### <code>ISNONE</code> <sup><sub>[FUNCTION]</sub></sup><a name="ISNONE"></a>
<code>∀ :A. ((OPTIONAL :A) → BOOLEAN)</code>

Is X None?


***

#### <code>ISSOME</code> <sup><sub>[FUNCTION]</sub></sup><a name="ISSOME"></a>
<code>∀ :A. ((OPTIONAL :A) → BOOLEAN)</code>

Is X Some?


***

#### <code>FROMSOME</code> <sup><sub>[FUNCTION]</sub></sup><a name="FROMSOME"></a>
<code>∀ :A. (STRING → (OPTIONAL :A) → :A)</code>

Get the value of OPT, erroring with the provided string if it is None.


***

## File: [list.lisp](../src/library/list.lisp)

### Functions

#### <code>ALL</code> <sup><sub>[FUNCTION]</sub></sup><a name="ALL"></a>
<code>∀ :A. ((:A → BOOLEAN) → (LIST :A) → BOOLEAN)</code>

Returns TRUE if every element in XS matches F.


***

#### <code>ANY</code> <sup><sub>[FUNCTION]</sub></sup><a name="ANY"></a>
<code>∀ :A. ((:A → BOOLEAN) → (LIST :A) → BOOLEAN)</code>

Returns TRUE if at least one element in XS matches F.


***

#### <code>SUM</code> <sup><sub>[FUNCTION]</sub></sup><a name="SUM"></a>
<code>∀ :A. NUM :A ⇒ ((LIST :A) → :A)</code>

Returns the sum of XS


***

#### <code>ZIP</code> <sup><sub>[FUNCTION]</sub></sup><a name="ZIP"></a>
<code>∀ :A :B. ((LIST :A) → (LIST :B) → (LIST (TUPLE :A :B)))</code>

Builds a list of tuples with the elements of XS and YS.


***

#### <code>DROP</code> <sup><sub>[FUNCTION]</sub></sup><a name="DROP"></a>
<code>∀ :A. (INTEGER → (LIST :A) → (LIST :A))</code>

Returns a list with the first N elements of XS removed


***

#### <code>FIND</code> <sup><sub>[FUNCTION]</sub></sup><a name="FIND"></a>
<code>∀ :A. ((:A → BOOLEAN) → (LIST :A) → (OPTIONAL :A))</code>

Returns the first element in a list matching the predicate function F.


***

#### <code>FOLD</code> <sup><sub>[FUNCTION]</sub></sup><a name="FOLD"></a>
<code>∀ :A :B. ((:A → :B → :B) → :B → (LIST :A) → :B)</code>

Tail recursive left fold on lists.


***

#### <code>HEAD</code> <sup><sub>[FUNCTION]</sub></sup><a name="HEAD"></a>
<code>∀ :A. ((LIST :A) → (OPTIONAL :A))</code>

Returns the first element of a list.


***

#### <code>INIT</code> <sup><sub>[FUNCTION]</sub></sup><a name="INIT"></a>
<code>∀ :A. ((LIST :A) → (LIST :A))</code>

Returns every element except the last in a list.


***

#### <code>LAST</code> <sup><sub>[FUNCTION]</sub></sup><a name="LAST"></a>
<code>∀ :A. ((LIST :A) → (OPTIONAL :A))</code>

Returns the last element of a list.


***

#### <code>NULL</code> <sup><sub>[FUNCTION]</sub></sup><a name="NULL"></a>
<code>∀ :A. ((LIST :A) → BOOLEAN)</code>

Returns TRUE if XS is an empty list.


***

#### <code>SORT</code> <sup><sub>[FUNCTION]</sub></sup><a name="SORT"></a>
<code>∀ :A. ORD :A ⇒ ((LIST :A) → (LIST :A))</code>

Performs a stable sort of XS.


***

#### <code>TAIL</code> <sup><sub>[FUNCTION]</sub></sup><a name="TAIL"></a>
<code>∀ :A. ((LIST :A) → (OPTIONAL (LIST :A)))</code>

Returns every element except the first in a list.


***

#### <code>TAKE</code> <sup><sub>[FUNCTION]</sub></sup><a name="TAKE"></a>
<code>∀ :A. (INTEGER → (LIST :A) → (LIST :A))</code>

Returns the first N elements of XS


***

#### <code>FOLDR</code> <sup><sub>[FUNCTION]</sub></sup><a name="FOLDR"></a>
<code>∀ :A :B. ((:A → :B → :B) → :B → (LIST :A) → :B)</code>

Right fold on lists. Is not tail recursive.


***

#### <code>INDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="INDEX"></a>
<code>∀ :A. ((LIST :A) → INTEGER → (OPTIONAL :A))</code>

Returns the Ith element of XS.


***

#### <code>RANGE</code> <sup><sub>[FUNCTION]</sub></sup><a name="RANGE"></a>
<code>(INTEGER → INTEGER → (LIST INTEGER))</code>

Returns a list containing the numbers from START to END inclusive.


***

#### <code>SPLIT</code> <sup><sub>[FUNCTION]</sub></sup><a name="SPLIT"></a>
<code>(CHAR → STRING → (LIST STRING))</code>

***

#### <code>UNION</code> <sup><sub>[FUNCTION]</sub></sup><a name="UNION"></a>
<code>∀ :A. EQ :A ⇒ ((LIST :A) → (LIST :A) → (LIST :A))</code>

Returns a new list with the elements from both XS and YS and without duplicates.


***

#### <code>APPEND</code> <sup><sub>[FUNCTION]</sub></sup><a name="APPEND"></a>
<code>∀ :A. ((LIST :A) → (LIST :A) → (LIST :A))</code>

Appends two lists together and returns a new list.


***

#### <code>CONCAT</code> <sup><sub>[FUNCTION]</sub></sup><a name="CONCAT"></a>
<code>∀ :A. ((LIST (LIST :A)) → (LIST :A))</code>

Appends a list of lists together into a single new list.


***

#### <code>DELETE</code> <sup><sub>[FUNCTION]</sub></sup><a name="DELETE"></a>
<code>∀ :A. EQ :A ⇒ (:A → (LIST :A) → (LIST :A))</code>

Return a new list with the first element equal to X removed.


***

#### <code>FILTER</code> <sup><sub>[FUNCTION]</sub></sup><a name="FILTER"></a>
<code>∀ :A. ((:A → BOOLEAN) → (LIST :A) → (LIST :A))</code>

Returns a new list containing every element of XS that matches the predicate function F in the same order.


***

#### <code>INSERT</code> <sup><sub>[FUNCTION]</sub></sup><a name="INSERT"></a>
<code>∀ :A. ORD :A ⇒ (:A → (LIST :A) → (LIST :A))</code>

Inserts an element into a list at the first place it is less than or equal to the next element.


***

#### <code>LENGTH</code> <sup><sub>[FUNCTION]</sub></sup><a name="LENGTH"></a>
<code>∀ :A. ((LIST :A) → INTEGER)</code>

Returns the length of a list.


***

#### <code>LOOKUP</code> <sup><sub>[FUNCTION]</sub></sup><a name="LOOKUP"></a>
<code>∀ :A :B. EQ :A ⇒ (:A → (LIST (TUPLE :A :B)) → (OPTIONAL :B))</code>

Returns the value of the first (key, value) tuple in XS where the key matches E.


***

#### <code>MEMBER</code> <sup><sub>[FUNCTION]</sub></sup><a name="MEMBER"></a>
<code>∀ :A. EQ :A ⇒ (:A → (LIST :A) → BOOLEAN)</code>

Returns true if any element of XS is equal to E.


***

#### <code>REPEAT</code> <sup><sub>[FUNCTION]</sub></sup><a name="REPEAT"></a>
<code>∀ :A. (INTEGER → :A → (LIST :A))</code>

Returns a list with X repeated N times.


***

#### <code>SORTBY</code> <sup><sub>[FUNCTION]</sub></sup><a name="SORTBY"></a>
<code>∀ :A. ((:A → :A → ORD) → (LIST :A) → (LIST :A))</code>

Generic version of sort


***

#### <code>MAXIMUM</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAXIMUM"></a>
<code>∀ :A. ORD :A ⇒ ((LIST :A) → (OPTIONAL :A))</code>

Returns the greatest element in XS.


***

#### <code>MINIMUM</code> <sup><sub>[FUNCTION]</sub></sup><a name="MINIMUM"></a>
<code>∀ :A. ORD :A ⇒ ((LIST :A) → (OPTIONAL :A))</code>

Returns the least element in XS.


***

#### <code>PRODUCT</code> <sup><sub>[FUNCTION]</sub></sup><a name="PRODUCT"></a>
<code>∀ :A. NUM :A ⇒ ((LIST :A) → :A)</code>

Returns the product of XS


***

#### <code>REVERSE</code> <sup><sub>[FUNCTION]</sub></sup><a name="REVERSE"></a>
<code>∀ :A. ((LIST :A) → (LIST :A))</code>

Returns a new list containing the same elements in reverse order.


***

#### <code>ZIPWITH</code> <sup><sub>[FUNCTION]</sub></sup><a name="ZIPWITH"></a>
<code>∀ :A :B :C. ((:A → :B → :C) → (LIST :A) → (LIST :B) → (LIST :C))</code>

Builds a new list by calling F with elements of XS and YS.


***

#### <code>INSERTBY</code> <sup><sub>[FUNCTION]</sub></sup><a name="INSERTBY"></a>
<code>∀ :A. ((:A → :A → ORD) → :A → (LIST :A) → (LIST :A))</code>

Generic version of insert


***

#### <code>ZIPWITH3</code> <sup><sub>[FUNCTION]</sub></sup><a name="ZIPWITH3"></a>
<code>∀ :A :B :C :D. ((:A → :B → :C → :D) → (LIST :A) → (LIST :B) → (LIST :C) → (LIST :D))</code>

Build a new list by calling F with elements of XS, YS and ZS


***

#### <code>ZIPWITH4</code> <sup><sub>[FUNCTION]</sub></sup><a name="ZIPWITH4"></a>
<code>∀ :A :B :C :D :E. ((:A → :B → :C → :D → :E) → (LIST :A) → (LIST :B) → (LIST :C) → (LIST :D) → (LIST :E))</code>

Build a new list by calling F with elements of AS, BS, CS and DS


***

#### <code>ZIPWITH5</code> <sup><sub>[FUNCTION]</sub></sup><a name="ZIPWITH5"></a>
<code>∀ :A :B :C :D :E :F. ((:A → :B → :C → :D → :E → :F) → (LIST :A) → (LIST :B) → (LIST :C) → (LIST :D) → (LIST :E) → (LIST :F))</code>

Build a new list by calling F with elements of AS, BS, CS, DS and ES


***

#### <code>CONCATMAP</code> <sup><sub>[FUNCTION]</sub></sup><a name="CONCATMAP"></a>
<code>∀ :A :B. ((:A → (LIST :B)) → (LIST :A) → (LIST :B))</code>

Apply F to each element in XS and concatenate the results.


***

#### <code>ELEMINDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="ELEMINDEX"></a>
<code>∀ :A. EQ :A ⇒ (:A → (LIST :A) → (OPTIONAL INTEGER))</code>

***

#### <code>FINDINDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="FINDINDEX"></a>
<code>∀ :A. ((:A → BOOLEAN) → (LIST :A) → (OPTIONAL INTEGER))</code>

***

#### <code>PARTITION</code> <sup><sub>[FUNCTION]</sub></sup><a name="PARTITION"></a>
<code>∀ :A. ((:A → BOOLEAN) → (LIST :A) → (TUPLE (LIST :A) (LIST :A)))</code>

Splits a list into two new lists. The first list contains elements matching predicate F.


***

#### <code>SINGLETON</code> <sup><sub>[FUNCTION]</sub></sup><a name="SINGLETON"></a>
<code>∀ :A. (:A → (LIST :A))</code>

Returns a single element list containg only X.


***

#### <code>TRANSPOSE</code> <sup><sub>[FUNCTION]</sub></sup><a name="TRANSPOSE"></a>
<code>∀ :A. ((LIST (LIST :A)) → (LIST (LIST :A)))</code>

Transposes a matrix represented by a list of lists.


***

#### <code>INTERCALATE</code> <sup><sub>[FUNCTION]</sub></sup><a name="INTERCALATE"></a>
<code>∀ :A. ((LIST :A) → (LIST (LIST :A)) → (LIST :A))</code>

Intersperses XS into XSS and then concatenates the result.


***

#### <code>INTERSPERSE</code> <sup><sub>[FUNCTION]</sub></sup><a name="INTERSPERSE"></a>
<code>∀ :A. (:A → (LIST :A) → (LIST :A))</code>

Returns a new list where every other element is E.


***

#### <code>INTERSECTION</code> <sup><sub>[FUNCTION]</sub></sup><a name="INTERSECTION"></a>
<code>∀ :A. EQ :A ⇒ ((LIST :A) → (LIST :A) → (LIST :A))</code>

Returns elements which occur in both lists. Does not return duplicates and does not guarantee order.


***

#### <code>LIST-DIFFERENCE</code> <sup><sub>[FUNCTION]</sub></sup><a name="LIST-DIFFERENCE"></a>
<code>∀ :A. EQ :A ⇒ ((LIST :A) → (LIST :A) → (LIST :A))</code>

Returns a new list with the first occurence of each element in YS deleted from XS.


***

#### <code>REMOVE-DUPLICATES</code> <sup><sub>[FUNCTION]</sub></sup><a name="REMOVE-DUPLICATES"></a>
<code>∀ :A. EQ :A ⇒ ((LIST :A) → (LIST :A))</code>

Returns a new list without duplicate elements.


***

## File: [tuple.lisp](../src/library/tuple.lisp)

### Functions

#### <code>FST</code> <sup><sub>[FUNCTION]</sub></sup><a name="FST"></a>
<code>∀ :A :B. ((TUPLE :A :B) → :A)</code>

Get the first element of a tuple.


***

#### <code>SND</code> <sup><sub>[FUNCTION]</sub></sup><a name="SND"></a>
<code>∀ :A :B. ((TUPLE :A :B) → :B)</code>

Get the second element of a tuple.


***

## File: [result.lisp](../src/library/result.lisp)

### Functions

#### <code>ISOK</code> <sup><sub>[FUNCTION]</sub></sup><a name="ISOK"></a>
<code>∀ :A :B. ((RESULT :A :B) → BOOLEAN)</code>

Returns TRUE if X is ERR


***

#### <code>ISERR</code> <sup><sub>[FUNCTION]</sub></sup><a name="ISERR"></a>
<code>∀ :A :B. ((RESULT :A :B) → BOOLEAN)</code>

Returns TRUE if X is ERR


***

#### <code>MAPERR</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAPERR"></a>
<code>∀ :A :B :C. ((:A → :B) → (RESULT :A :C) → (RESULT :B :C))</code>

Map over the ERR case


***

## File: [functions.lisp](../src/library/functions.lisp)

### Functions

#### <code>ID</code> <sup><sub>[FUNCTION]</sub></sup><a name="ID"></a>
<code>∀ :A. (:A → :A)</code>

A function that always returns its argument


***

#### <code>FIX</code> <sup><sub>[FUNCTION]</sub></sup><a name="FIX"></a>
<code>∀ :A :B. (((:A → :B) → :A → :B) → :A → :B)</code>

The factorial function can be written
    ```
    (define fact
      (fix
        (fn (f n)
          (if (== n 0)
            1
            (* n (f (- n 1)))))))
    ```


***

#### <code>ASUM</code> <sup><sub>[FUNCTION]</sub></sup><a name="ASUM"></a>
<code>∀ :A :B. ALTERNATIVE :A ⇒ ((LIST (:A :B)) → (:A :B))</code>

Fold over a list using alt


***

#### <code>FLIP</code> <sup><sub>[FUNCTION]</sub></sup><a name="FLIP"></a>
<code>∀ :A :B :C. ((:A → :B → :C) → :B → :A → :C)</code>

FLIP reverses the arguments to F


***

#### <code>CONST</code> <sup><sub>[FUNCTION]</sub></sup><a name="CONST"></a>
<code>∀ :A :B. (:A → :B → :A)</code>

A function that always returns its first argument


***

#### <code>TRACE</code> <sup><sub>[FUNCTION]</sub></sup><a name="TRACE"></a>
<code>(STRING → UNIT)</code>

Print a line to *STANDARD-OUTPUT*


***

#### <code>COMPOSE</code> <sup><sub>[FUNCTION]</sub></sup><a name="COMPOSE"></a>
<code>∀ :A :B :C. ((:A → :B) → (:C → :A) → :C → :B)</code>

***

#### <code>SEQUENCE</code> <sup><sub>[FUNCTION]</sub></sup><a name="SEQUENCE"></a>
<code>∀ :A :B. APPLICATIVE :A ⇒ ((LIST (:A :B)) → (:A (LIST :B)))</code>

***

#### <code>TRAVERSE</code> <sup><sub>[FUNCTION]</sub></sup><a name="TRAVERSE"></a>
<code>∀ :A :B :C. APPLICATIVE :B ⇒ ((:A → (:B :C)) → (LIST :A) → (:B (LIST :C)))</code>

Map the elements of XS with F then collect the results.


***

#### <code>TRACEOBJECT</code> <sup><sub>[FUNCTION]</sub></sup><a name="TRACEOBJECT"></a>
<code>∀ :A. (STRING → :A → UNIT)</code>

Print a line to *STANDARD-OUTPUT* in the form "{STR}: {ITEM}"


***

## File: [quantize.lisp](../src/library/quantize.lisp)

### Functions

#### <code>FLOOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="FLOOR"></a>
<code>∀ :A. QUANTIZABLE :A ⇒ (:A → INTEGER)</code>

Return the greatest integer less than or equal to X.


***

#### <code>ROUND</code> <sup><sub>[FUNCTION]</sub></sup><a name="ROUND"></a>
<code>∀ :A. QUANTIZABLE :A ⇒ (:A → INTEGER)</code>

Return the nearest integer to X, with ties breaking toward positive infinity.


***

#### <code>SAFE/</code> <sup><sub>[FUNCTION]</sub></sup><a name="SAFE/"></a>
<code>∀ :A :B. DIVIDABLE :A :B ⇒ (:A → :A → (OPTIONAL :B))</code>

Safely divide X by Y, returning None if Y is zero.


***

#### <code>EXACT/</code> <sup><sub>[FUNCTION]</sub></sup><a name="EXACT/"></a>
<code>(INTEGER → INTEGER → FRACTION)</code>

Exactly divide two integers and produce a fraction.


***

#### <code>FLOOR/</code> <sup><sub>[FUNCTION]</sub></sup><a name="FLOOR/"></a>
<code>(INTEGER → INTEGER → INTEGER)</code>

Divide two integers and compute the floor of the quotient.


***

#### <code>ROUND/</code> <sup><sub>[FUNCTION]</sub></sup><a name="ROUND/"></a>
<code>(INTEGER → INTEGER → INTEGER)</code>

Divide two integers and round the quotient.


***

#### <code>CEILING</code> <sup><sub>[FUNCTION]</sub></sup><a name="CEILING"></a>
<code>∀ :A. QUANTIZABLE :A ⇒ (:A → INTEGER)</code>

Return the least integer greater than or equal to X.


***

#### <code>DOUBLE/</code> <sup><sub>[FUNCTION]</sub></sup><a name="DOUBLE/"></a>
<code>(DOUBLE-FLOAT → DOUBLE-FLOAT → DOUBLE-FLOAT)</code>

Compute the quotient of single-precision floats A and B as a single-precision float.


***

#### <code>SINGLE/</code> <sup><sub>[FUNCTION]</sub></sup><a name="SINGLE/"></a>
<code>(SINGLE-FLOAT → SINGLE-FLOAT → SINGLE-FLOAT)</code>

Compute the quotient of single-precision floats A and B as a single-precision float.


***

#### <code>CEILING/</code> <sup><sub>[FUNCTION]</sub></sup><a name="CEILING/"></a>
<code>(INTEGER → INTEGER → INTEGER)</code>

Divide two integers and compute the ceiling of the quotient.


***

#### <code>INEXACT/</code> <sup><sub>[FUNCTION]</sub></sup><a name="INEXACT/"></a>
<code>(INTEGER → INTEGER → DOUBLE-FLOAT)</code>

Compute the quotient of integers A and B as a double-precision float.

Note: This does *not* divide double-float arguments.


***

## File: [cell.lisp](../src/library/cell.lisp)

### Types

#### <code>CELL :A</code> <sup><sub>[TYPE]</sub></sup><a name="CELL"></a>
- <code>(CELL LISP-OBJECT)</code>

Internally mutable cell

Constructors:
- <code>CELL :: (LISP-OBJECT → (CELL :A))</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ :B</a> => <a href="#EQ">EQ</a> <a href="#CELL">(CELL :B)</a></code>
- <code><a href="#NUM">NUM :B</a> => <a href="#NUM">NUM</a> <a href="#CELL">(CELL :B)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#CELL">(CELL :B)</a> <a href="#:B">:B</a></code>
- <code><a href="#INTO">INTO</a> <a href="#:B">:B</a> <a href="#CELL">(CELL :B)</a></code>
- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#CELL">CELL</a></code>
- <code><a href="#SEMIGROUP">SEMIGROUP :B</a> => <a href="#SEMIGROUP">SEMIGROUP</a> <a href="#CELL">(CELL :B)</a></code>
- <code><a href="#APPLICATIVE">APPLICATIVE</a> <a href="#CELL">CELL</a></code>

</details>


***

### Functions

#### <code>CELL-READ</code> <sup><sub>[FUNCTION]</sub></sup><a name="CELL-READ"></a>
<code>∀ :A. ((CELL :A) → :A)</code>

Read the value of a mutable cell


***

#### <code>CELL-SWAP</code> <sup><sub>[FUNCTION]</sub></sup><a name="CELL-SWAP"></a>
<code>∀ :A. (:A → (CELL :A) → :A)</code>

Replace the value of a mutable cell with a new value, then return the old value


***

#### <code>MAKE-CELL</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-CELL"></a>
<code>∀ :A. (:A → (CELL :A))</code>

Create a new mutable cell


***

#### <code>CELL-WRITE</code> <sup><sub>[FUNCTION]</sub></sup><a name="CELL-WRITE"></a>
<code>∀ :A. (:A → (CELL :A) → UNIT)</code>

Set the value of a mutable cell


***

#### <code>CELL-UPDATE</code> <sup><sub>[FUNCTION]</sub></sup><a name="CELL-UPDATE"></a>
<code>∀ :A. ((:A → :A) → (CELL :A) → UNIT)</code>

***

## File: [vector.lisp](../src/library/vector.lisp)

### Types

#### <code>VECTOR :A</code> <sup><sub>[TYPE]</sub></sup><a name="VECTOR"></a>
- <code>(VECTOR LISP-OBJECT)</code>

Constructors:
- <code>VECTOR :: (LISP-OBJECT → (VECTOR :A))</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ :B</a> => <a href="#EQ">EQ</a> <a href="#VECTOR">(VECTOR :B)</a></code>
- <code><a href="#ISO">ISO</a> <a href="#SLICE">(SLICE :B)</a> <a href="#VECTOR">(VECTOR :B)</a></code>
- <code><a href="#ISO">ISO</a> <a href="#VECTOR">(VECTOR :B)</a> <a href="#LIST">(LIST :B)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#VECTOR">(VECTOR :B)</a> <a href="#SLICE">(SLICE :B)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#SLICE">(SLICE :B)</a> <a href="#VECTOR">(VECTOR :B)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#VECTOR">(VECTOR :B)</a> <a href="#LIST">(LIST :B)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#LIST">(LIST :B)</a> <a href="#VECTOR">(VECTOR :B)</a></code>
- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#VECTOR">VECTOR</a></code>
- <code><a href="#SEMIGROUP">SEMIGROUP</a> <a href="#VECTOR">(VECTOR :B)</a></code>

</details>


***

### Functions

#### <code>VECTOR-POP</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-POP"></a>
<code>∀ :A. ((VECTOR :A) → (OPTIONAL :A))</code>

Remove and return the first item of V


***

#### <code>VECTOR-SET</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-SET"></a>
<code>∀ :A. (INTEGER → :A → (VECTOR :A) → UNIT)</code>

Set the INDEXth element of V to ITEM. This function left intentionally unsafe because it does not have a return value to check.


***

#### <code>MAKE-VECTOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-VECTOR"></a>
<code>∀ :A. (UNIT → (VECTOR :A))</code>

Create a new empty vector


***

#### <code>VECTOR-COPY</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-COPY"></a>
<code>∀ :A. ((VECTOR :A) → (VECTOR :A))</code>

Return a new vector containing the same elements as V


***

#### <code>VECTOR-HEAD</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-HEAD"></a>
<code>∀ :A. ((VECTOR :A) → (OPTIONAL :A))</code>

Return the first item of V


***

#### <code>VECTOR-LAST</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-LAST"></a>
<code>∀ :A. ((VECTOR :A) → (OPTIONAL :A))</code>

Return the last element of V


***

#### <code>VECTOR-PUSH</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-PUSH"></a>
<code>∀ :A. (:A → (VECTOR :A) → INTEGER)</code>

Append ITEM to V and resize V if necessary


***

#### <code>VECTOR-SORT</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-SORT"></a>
<code>∀ :A. ORD :A ⇒ ((VECTOR :A) → UNIT)</code>

Sort a vector inplace


***

#### <code>VECTOR-EMPTY</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-EMPTY"></a>
<code>∀ :A. ((VECTOR :A) → BOOLEAN)</code>

Returns TRUE if V is empty


***

#### <code>VECTOR-INDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-INDEX"></a>
<code>∀ :A. (INTEGER → (VECTOR :A) → (OPTIONAL :A))</code>

Return the INDEXth element of V


***

#### <code>VECTOR-APPEND</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-APPEND"></a>
<code>∀ :A. ((VECTOR :A) → (VECTOR :A) → (VECTOR :A))</code>

Create a new VECTOR containing the elements of v1 followed by the elements of v2


***

#### <code>VECTOR-LENGTH</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-LENGTH"></a>
<code>∀ :A. ((VECTOR :A) → INTEGER)</code>

Returns the length of V


***

#### <code>VECTOR-FOREACH</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-FOREACH"></a>
<code>∀ :A :B. ((:A → :B) → (VECTOR :A) → UNIT)</code>

Call the function F once for each item in V


***

#### <code>VECTOR-SORT-BY</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-SORT-BY"></a>
<code>∀ :A. ((:A → :A → BOOLEAN) → (VECTOR :A) → UNIT)</code>

Sort a vector with predicate function F


***

#### <code>VECTOR-TO-LIST</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-TO-LIST"></a>
<code>∀ :A. ((VECTOR :A) → (LIST :A))</code>

***

#### <code>VECTOR-CAPACITY</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-CAPACITY"></a>
<code>∀ :A. ((VECTOR :A) → INTEGER)</code>

Returns the number of elements that V can store without resizing


***

#### <code>VECTOR-FOREACH2</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-FOREACH2"></a>
<code>∀ :A :B :C. ((:A → :B → :C) → (VECTOR :A) → (VECTOR :B) → UNIT)</code>

Like vector-foreach but twice as good


***

#### <code>VECTOR-POP-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-POP-UNSAFE"></a>
<code>∀ :A. ((VECTOR :A) → :A)</code>

Remove and return the first item of V without checking if the vector is empty


***

#### <code>VECTOR-HEAD-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-HEAD-UNSAFE"></a>
<code>∀ :A. ((VECTOR :A) → :A)</code>

Return the first item of V without first checking if V is empty


***

#### <code>VECTOR-LAST-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-LAST-UNSAFE"></a>
<code>∀ :A. ((VECTOR :A) → :A)</code>

Return the last element of V without first checking if V is empty


***

#### <code>VECTOR-SWAP-REMOVE</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-SWAP-REMOVE"></a>
<code>∀ :A. (INTEGER → (VECTOR :A) → (OPTIONAL :A))</code>

Remove the element IDX from VEC and replace it with the last element in VEC. Then return the removed element.


***

#### <code>VECTOR-INDEX-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-INDEX-UNSAFE"></a>
<code>∀ :A. (INTEGER → (VECTOR :A) → :A)</code>

Return the INDEXth element of V without checking if the element exists


***

#### <code>MAKE-VECTOR-CAPACITY</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-VECTOR-CAPACITY"></a>
<code>∀ :A. (INTEGER → (VECTOR :A))</code>

Create a new vector with N elements preallocated


***

#### <code>VECTOR-FOREACH-INDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-FOREACH-INDEX"></a>
<code>∀ :A :B. ((INTEGER → :A → :B) → (VECTOR :A) → UNIT)</code>

Call the function F once for each item in V with its index


***

#### <code>VECTOR-SWAP-REMOVE-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-SWAP-REMOVE-UNSAFE"></a>
<code>∀ :A. (INTEGER → (VECTOR :A) → :A)</code>

Remove the element IDX from VEC and replace it with the last element in VEC without bounds checking. Then return the removed element.


***

## File: [slice.lisp](../src/library/slice.lisp)

### Types

#### <code>SLICE :A</code> <sup><sub>[TYPE]</sub></sup><a name="SLICE"></a>
- <code>(SLICE LISP-OBJECT)</code>

Constructors:
- <code>SLICE :: (LISP-OBJECT → (SLICE :A))</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ :B</a> => <a href="#EQ">EQ</a> <a href="#SLICE">(SLICE :B)</a></code>
- <code><a href="#ISO">ISO</a> <a href="#SLICE">(SLICE :B)</a> <a href="#VECTOR">(VECTOR :B)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#VECTOR">(VECTOR :B)</a> <a href="#SLICE">(SLICE :B)</a></code>
- <code><a href="#INTO">INTO</a> <a href="#SLICE">(SLICE :B)</a> <a href="#VECTOR">(VECTOR :B)</a></code>

</details>


***

### Functions

#### <code>SLICE-SET</code> <sup><sub>[FUNCTION]</sub></sup><a name="SLICE-SET"></a>
<code>∀ :A. (INTEGER → :A → (SLICE :A) → UNIT)</code>

Set the element at INDEX in S to ITEM


***

#### <code>MAKE-SLICE</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-SLICE"></a>
<code>∀ :A. (INTEGER → INTEGER → (VECTOR :A) → (SLICE :A))</code>

***

#### <code>SLICE-COPY</code> <sup><sub>[FUNCTION]</sub></sup><a name="SLICE-COPY"></a>
<code>∀ :A. ((SLICE :A) → (SLICE :A))</code>

Returns a new slice containg the same elements as S


***

#### <code>SLICE-INDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="SLICE-INDEX"></a>
<code>∀ :A. (INTEGER → (SLICE :A) → (OPTIONAL :A))</code>

Lookup the element at INDEX in S


***

#### <code>SLICE-LENGTH</code> <sup><sub>[FUNCTION]</sub></sup><a name="SLICE-LENGTH"></a>
<code>∀ :A. ((SLICE :A) → INTEGER)</code>

Returns the length of S


***

#### <code>SLICE-FOREACH</code> <sup><sub>[FUNCTION]</sub></sup><a name="SLICE-FOREACH"></a>
<code>∀ :A :B. ((:A → :B) → (SLICE :A) → UNIT)</code>

Call the function F once for each item in S


***

#### <code>SLICE-FOREACH2</code> <sup><sub>[FUNCTION]</sub></sup><a name="SLICE-FOREACH2"></a>
<code>∀ :A :B :C. ((:A → :B → :C) → (SLICE :A) → (SLICE :B) → UNIT)</code>

Iterate over S1 and S2 calling F once on each iteration


***

#### <code>VECTOR-CHUNKED</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-CHUNKED"></a>
<code>∀ :A :B. (((SLICE :A) → :B) → INTEGER → (VECTOR :A) → UNIT)</code>

Chunked iteration over a vector. Ignores elements at the end if the vector does not evenly divide by the chunk size.


***

#### <code>VECTOR-SLIDING</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-SLIDING"></a>
<code>∀ :A :B. (((SLICE :A) → :B) → INTEGER → (VECTOR :A) → UNIT)</code>

Sliding iteration over a vector


***

#### <code>SLICE-INDEX-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="SLICE-INDEX-UNSAFE"></a>
<code>∀ :A. (INTEGER → (SLICE :A) → :A)</code>

Lookup the element at INDEX in S without bounds checking


***

#### <code>SLICE-FOREACH-INDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="SLICE-FOREACH-INDEX"></a>
<code>∀ :A :B. ((INTEGER → :A → :B) → (SLICE :A) → UNIT)</code>

Call the function F once for each item in S with its index


***

## File: [hashtable.lisp](../src/library/hashtable.lisp)

### Types

#### <code>HASHTABLE :A :B</code> <sup><sub>[TYPE]</sub></sup><a name="HASHTABLE"></a>
- <code>(HASHTABLE LISP-OBJECT)</code>

Constructors:
- <code>HASHTABLE :: (LISP-OBJECT → (HASHTABLE :A :B))</code>


***

### Functions

#### <code>HASHTABLE-GET</code> <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-GET"></a>
<code>∀ :A :B. (:A → (HASHTABLE :A :B) → (OPTIONAL :B))</code>

Lookup KEY in TABLE


***

#### <code>HASHTABLE-SET</code> <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-SET"></a>
<code>∀ :A :B. (:A → :B → (HASHTABLE :A :B) → UNIT)</code>

Set KEY to VALUE in TABLE


***

#### <code>HASHTABLE-KEYS</code> <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-KEYS"></a>
<code>∀ :A :B. ((HASHTABLE :A :B) → (VECTOR :A))</code>

Returns the keys in TABLE as a vector


***

#### <code>MAKE-HASHTABLE</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-HASHTABLE"></a>
<code>∀ :A :B. (UNIT → (HASHTABLE :A :B))</code>

Create a new empty hashtable


***

#### <code>HASHTABLE-COUNT</code> <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-COUNT"></a>
<code>∀ :A :B. ((HASHTABLE :A :B) → INTEGER)</code>

Returns the number of entries in TABLE


***

#### <code>HASHTABLE-REMOVE</code> <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-REMOVE"></a>
<code>∀ :A :B. (:A → (HASHTABLE :A :B) → UNIT)</code>

Remove the entry at KEY from TABLE


***

#### <code>HASHTABLE-VALUES</code> <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-VALUES"></a>
<code>∀ :A :B. ((HASHTABLE :A :B) → (VECTOR :B))</code>

Returns the values in TABLE as a vector


***

#### <code>HASHTABLE-ENTRIES</code> <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-ENTRIES"></a>
<code>∀ :A :B. ((HASHTABLE :A :B) → (VECTOR (TUPLE :A :B)))</code>

Returns the keys and values in TABLE as a vector


***

#### <code>HASHTABLE-FOREACH</code> <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-FOREACH"></a>
<code>∀ :A :B :C. ((:A → :B → :C) → (HASHTABLE :A :B) → UNIT)</code>

Call F once for each key value pair in TABLE


***

#### <code>MAKE-HASHTABLE-CAPACITY</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-HASHTABLE-CAPACITY"></a>
<code>∀ :A :B. (INTEGER → (HASHTABLE :A :B))</code>

Crate a new empty hashtable with a given capacity


***

## File: [graph.lisp](../src/library/graph.lisp)

### Types

#### <code>GRAPH :A :B</code> <sup><sub>[TYPE]</sub></sup><a name="GRAPH"></a>
- <code>(GRAPH GRAPHTYPE (VECTOR (NODE :A)) (VECTOR (EDGE :B)))</code>

A graph using adjacency list representation

Constructors:
- <code>GRAPH :: (GRAPHTYPE → (VECTOR (NODE :A)) → (VECTOR (EDGE :B)) → (GRAPH :A :B))</code>


***

#### <code>EDGEINDEX</code> <sup><sub>[TYPE]</sub></sup><a name="EDGEINDEX"></a>
- <code>(EDGEINDEX INTEGER)</code>

Constructors:
- <code>EDGEINDEX :: (INTEGER → EDGEINDEX)</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> <a href="#EDGEINDEX">EDGEINDEX</a></code>
- <code><a href="#INTO">INTO</a> <a href="#EDGEINDEX">EDGEINDEX</a> <a href="#INTEGER">INTEGER</a></code>

</details>


***

#### <code>GRAPHTYPE</code> <sup><sub>[TYPE]</sub></sup><a name="GRAPHTYPE"></a>
- <code>UNDIRECTED</code>
- <code>DIRECTED</code>

Constructors:
- <code>UNDIRECTED :: GRAPHTYPE</code>
- <code>DIRECTED :: GRAPHTYPE</code>


***

#### <code>NODEINDEX</code> <sup><sub>[TYPE]</sub></sup><a name="NODEINDEX"></a>
- <code>(NODEINDEX INTEGER)</code>

Constructors:
- <code>NODEINDEX :: (INTEGER → NODEINDEX)</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> <a href="#NODEINDEX">NODEINDEX</a></code>
- <code><a href="#INTO">INTO</a> <a href="#NODEINDEX">NODEINDEX</a> <a href="#INTEGER">INTEGER</a></code>

</details>


***

### Functions

#### <code>MAKE-GRAPH</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-GRAPH"></a>
<code>∀ :A :B. (UNIT → (GRAPH :A :B))</code>

Create a new empty undirected graph


***

#### <code>GRAPH-EDGES</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-EDGES"></a>
<code>∀ :A :B. ((GRAPH :A :B) → (VECTOR (EDGE :B)))</code>

Returns the edges in a graph


***

#### <code>GRAPH-NODES</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-NODES"></a>
<code>∀ :A :B. ((GRAPH :A :B) → (VECTOR (NODE :A)))</code>

Returns the nodes in a graph


***

#### <code>MAKE-DIGRAPH</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-DIGRAPH"></a>
<code>∀ :A :B. (UNIT → (GRAPH :A :B))</code>

Create a new directed graph


***

#### <code>GRAPH-ADD-EDGE</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-ADD-EDGE"></a>
<code>∀ :A :B. (:A → NODEINDEX → NODEINDEX → (GRAPH :B :A) → EDGEINDEX)</code>

Add an edge with associated data from node FROM to node TO in the graph.


***

#### <code>GRAPH-ADD-NODE</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-ADD-NODE"></a>
<code>∀ :A :B. (:A → (GRAPH :A :B) → NODEINDEX)</code>

Add a node with associated data to the graph, returning the index of the new node.


***

#### <code>GRAPH-EDGE-COUNT</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-EDGE-COUNT"></a>
<code>∀ :A :B. ((GRAPH :A :B) → INTEGER)</code>

Returns the number of edges in a graph


***

#### <code>GRAPH-LOOKUP-EDGE</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-LOOKUP-EDGE"></a>
<code>∀ :A :B. (EDGEINDEX → (GRAPH :A :B) → (OPTIONAL (EDGE :B)))</code>

Lookup a node with index IDX in graph G


***

#### <code>GRAPH-LOOKUP-NODE</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-LOOKUP-NODE"></a>
<code>∀ :A :B. (NODEINDEX → (GRAPH :A :B) → (OPTIONAL (NODE :A)))</code>

Lookup a node with index IDX in graph G


***

#### <code>GRAPH-REMOVE-EDGE</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-REMOVE-EDGE"></a>
<code>∀ :A :B. (EDGEINDEX → (GRAPH :A :B) → (OPTIONAL :B))</code>

Remove an edge from GRAPH


***

#### <code>GRAPH-REMOVE-NODE</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-REMOVE-NODE"></a>
<code>∀ :A :B. (NODEINDEX → (GRAPH :A :B) → (OPTIONAL :A))</code>

Remove a node and all edges connecting to it from GRAPH


***

