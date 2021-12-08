# Reference for COALTON

### Types

#### <code>U8</code> <sup><sub>[TYPE]</sub></sup><a name="U8"></a>


<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> <a href="#U8">U8</a></code>
- <code><a href="#NUM">NUM</a> <a href="#U8">U8</a></code>
- <code><a href="#ORD">ORD</a> <a href="#U8">U8</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U8">U8</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U8">U8</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U8">U8</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#U8">U8</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#U8">U8</a></code>

</details>


***

#### <code>I32</code> <sup><sub>[TYPE]</sub></sup><a name="I32"></a>


<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> <a href="#I32">I32</a></code>
- <code><a href="#NUM">NUM</a> <a href="#I32">I32</a></code>
- <code><a href="#ORD">ORD</a> <a href="#I32">I32</a></code>
- <code><a href="#INTO">INTO</a> <a href="#I32">I32</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#I32">I32</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#I32">I32</a></code>

</details>


***

#### <code>I64</code> <sup><sub>[TYPE]</sub></sup><a name="I64"></a>


<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> <a href="#I64">I64</a></code>
- <code><a href="#NUM">NUM</a> <a href="#I64">I64</a></code>
- <code><a href="#ORD">ORD</a> <a href="#I64">I64</a></code>
- <code><a href="#INTO">INTO</a> <a href="#I64">I64</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#I64">I64</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#I64">I64</a></code>

</details>


***

#### <code>U32</code> <sup><sub>[TYPE]</sub></sup><a name="U32"></a>


<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> <a href="#U32">U32</a></code>
- <code><a href="#NUM">NUM</a> <a href="#U32">U32</a></code>
- <code><a href="#ORD">ORD</a> <a href="#U32">U32</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U32">U32</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U32">U32</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U32">U32</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#U32">U32</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#U32">U32</a></code>

</details>


***

#### <code>U64</code> <sup><sub>[TYPE]</sub></sup><a name="U64"></a>


<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> <a href="#U64">U64</a></code>
- <code><a href="#NUM">NUM</a> <a href="#U64">U64</a></code>
- <code><a href="#ORD">ORD</a> <a href="#U64">U64</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U64">U64</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U64">U64</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U64">U64</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#U64">U64</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#U64">U64</a></code>

</details>


***

#### <code>CHAR</code> <sup><sub>[TYPE]</sub></sup><a name="CHAR"></a>


<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> <a href="#CHAR">CHAR</a></code>
- <code><a href="#ORD">ORD</a> <a href="#CHAR">CHAR</a></code>

</details>


***

#### <code>ARROW :A :B</code> <sup><sub>[TYPE]</sub></sup><a name="ARROW"></a>



***

#### <code>STRING</code> <sup><sub>[TYPE]</sub></sup><a name="STRING"></a>


<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> <a href="#STRING">STRING</a></code>
- <code><a href="#ISO">ISO</a> (<a href="#LIST">LIST</a> <a href="#CHAR">CHAR</a>) <a href="#STRING">STRING</a></code>
- <code><a href="#ORD">ORD</a> <a href="#STRING">STRING</a></code>
- <code><a href="#INTO">INTO</a> (<a href="#LIST">LIST</a> <a href="#CHAR">CHAR</a>) <a href="#STRING">STRING</a></code>
- <code><a href="#INTO">INTO</a> <a href="#STRING">STRING</a> (<a href="#LIST">LIST</a> <a href="#CHAR">CHAR</a>)</code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#STRING">STRING</a></code>
- <code><a href="#MONOID">MONOID</a> <a href="#STRING">STRING</a></code>
- <code><a href="#TRYINTO">TRYINTO</a> <a href="#STRING">STRING</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#SEMIGROUP">SEMIGROUP</a> <a href="#STRING">STRING</a></code>

</details>


***

#### <code>INTEGER</code> <sup><sub>[TYPE]</sub></sup><a name="INTEGER"></a>


<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#NUM">NUM</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#ORD">ORD</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#NODEINDEX">NODEINDEX</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#EDGEINDEX">EDGEINDEX</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#STRING">STRING</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U64">U64</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#U64">U64</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U32">U32</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#U32">U32</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U8">U8</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#U8">U8</a></code>
- <code><a href="#INTO">INTO</a> <a href="#I64">I64</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#I64">I64</a></code>
- <code><a href="#INTO">INTO</a> <a href="#I32">I32</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#I32">I32</a></code>
- <code><a href="#TRYINTO">TRYINTO</a> <a href="#STRING">STRING</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#INTEGER">INTEGER</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#INTEGER">INTEGER</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#INTEGER">INTEGER</a> <a href="#FRACTION">FRACTION</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#INTEGER">INTEGER</a></code>

</details>


***

#### <code>LISP-OBJECT</code> <sup><sub>[TYPE]</sub></sup><a name="LISP-OBJECT"></a>



***

#### <code>DOUBLE-FLOAT</code> <sup><sub>[TYPE]</sub></sup><a name="DOUBLE-FLOAT"></a>


<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#NUM">NUM</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#ORD">ORD</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U64">U64</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U32">U32</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U8">U8</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#INTEGER">INTEGER</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>

</details>


***

#### <code>SINGLE-FLOAT</code> <sup><sub>[TYPE]</sub></sup><a name="SINGLE-FLOAT"></a>


<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#NUM">NUM</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#ORD">ORD</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U64">U64</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U32">U32</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U8">U8</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#INTEGER">INTEGER</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>

</details>


***

## File: [types.lisp](../src/library/types.lisp)

### Types

#### <code>LIST :A</code> <sup><sub>[TYPE]</sub></sup><a name="LIST"></a>
- <code>CONS</code>
- <code>NIL</code>

Constructors:
- <code>CONS :: (<a href="#LIST">LIST</a> :B)</code>
- <code>NIL :: (<a href="#LIST">LIST</a> :B)</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> :C => <a href="#EQ">EQ</a> (<a href="#LIST">LIST</a> :C)</code>
- <code><a href="#ISO">ISO</a> (<a href="#VECTOR">VECTOR</a> :C) (<a href="#LIST">LIST</a> :C)</code>
- <code><a href="#ISO">ISO</a> (<a href="#LIST">LIST</a> <a href="#CHAR">CHAR</a>) <a href="#STRING">STRING</a></code>
- <code><a href="#INTO">INTO</a> (<a href="#VECTOR">VECTOR</a> :C) (<a href="#LIST">LIST</a> :C)</code>
- <code><a href="#INTO">INTO</a> (<a href="#LIST">LIST</a> :C) (<a href="#VECTOR">VECTOR</a> :C)</code>
- <code><a href="#INTO">INTO</a> (<a href="#LIST">LIST</a> <a href="#CHAR">CHAR</a>) <a href="#STRING">STRING</a></code>
- <code><a href="#INTO">INTO</a> <a href="#STRING">STRING</a> (<a href="#LIST">LIST</a> <a href="#CHAR">CHAR</a>)</code>
- <code><a href="#MONAD">MONAD</a> <a href="#LIST">LIST</a></code>
- <code><a href="#MONOID">MONOID</a> (<a href="#LIST">LIST</a> :C)</code>
- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#LIST">LIST</a></code>
- <code><a href="#SEMIGROUP">SEMIGROUP</a> (<a href="#LIST">LIST</a> :C)</code>
- <code><a href="#ALTERNATIVE">ALTERNATIVE</a> <a href="#LIST">LIST</a></code>
- <code><a href="#APPLICATIVE">APPLICATIVE</a> <a href="#LIST">LIST</a></code>

</details>


***

#### <code>UNIT</code> <sup><sub>[TYPE]</sub></sup><a name="UNIT"></a>
- <code>UNIT</code>

Constructors:
- <code>UNIT :: <a href="#UNIT">UNIT</a></code>


***

#### <code>BOOLEAN</code> <sup><sub>[TYPE]</sub></sup><a name="BOOLEAN"></a>
- <code>FALSE</code>
- <code>TRUE</code>

Constructors:
- <code>FALSE :: <a href="#BOOLEAN">BOOLEAN</a></code>
- <code>TRUE :: <a href="#BOOLEAN">BOOLEAN</a></code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> <a href="#BOOLEAN">BOOLEAN</a></code>
- <code><a href="#ORD">ORD</a> <a href="#BOOLEAN">BOOLEAN</a></code>

</details>


***

### Functions

#### <code>NIL</code> <sup><sub>[FUNCTION]</sub></sup><a name="NIL"></a>
<code>∀ :A. (<a href="#LIST">LIST</a> :A)</code>

***

#### <code>CONS</code> <sup><sub>[FUNCTION]</sub></sup><a name="CONS"></a>
<code>∀ :A. (:A → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

***

#### <code>TRUE</code> <sup><sub>[FUNCTION]</sub></sup><a name="TRUE"></a>
<code><a href="#BOOLEAN">BOOLEAN</a></code>

***

#### <code>FALSE</code> <sup><sub>[FUNCTION]</sub></sup><a name="FALSE"></a>
<code><a href="#BOOLEAN">BOOLEAN</a></code>

***

# Reference for COALTON-LIBRARY

## File: [types.lisp](../src/library/types.lisp)

### Types

#### <code>TUPLE :A :B</code> <sup><sub>[TYPE]</sub></sup><a name="TUPLE"></a>
- <code>(TUPLE :A :B)</code>

A heterogeneous collection of items.

Constructors:
- <code>TUPLE :: (:A → :B → (<a href="#TUPLE">TUPLE</a> :A :B))</code>

<details>
<summary>Instances</summary>

- <code>(<a href="#EQ">EQ</a> :C) (<a href="#EQ">EQ</a> :D) => <a href="#EQ">EQ</a> (<a href="#TUPLE">TUPLE</a> :C :D)</code>
- <code><a href="#ISO">ISO</a> (<a href="#TUPLE">TUPLE</a> :C :D) (<a href="#TUPLE">TUPLE</a> :D :C)</code>
- <code>(<a href="#ORD">ORD</a> :C) (<a href="#ORD">ORD</a> :D) => <a href="#ORD">ORD</a> (<a href="#TUPLE">TUPLE</a> :C :D)</code>
- <code><a href="#INTO">INTO</a> (<a href="#TUPLE">TUPLE</a> :C :D) (<a href="#TUPLE">TUPLE</a> :D :C)</code>

</details>


***

#### <code>RESULT :A :B</code> <sup><sub>[TYPE]</sub></sup><a name="RESULT"></a>
- <code>(ERR :A)</code>
- <code>(OK :B)</code>

Represents something that may have failed.

Constructors:
- <code>ERR :: (:A → (<a href="#RESULT">RESULT</a> :A :B))</code>
- <code>OK :: (:B → (<a href="#RESULT">RESULT</a> :A :B))</code>

<details>
<summary>Instances</summary>

- <code>(<a href="#EQ">EQ</a> :C) (<a href="#EQ">EQ</a> :D) => <a href="#EQ">EQ</a> (<a href="#RESULT">RESULT</a> :C :D)</code>
- <code><a href="#ISO">ISO</a> (<a href="#RESULT">RESULT</a> <a href="#UNIT">UNIT</a> :C) (<a href="#OPTIONAL">OPTIONAL</a> :C)</code>
- <code>(<a href="#ORD">ORD</a> :C) (<a href="#ORD">ORD</a> :D) => <a href="#ORD">ORD</a> (<a href="#RESULT">RESULT</a> :C :D)</code>
- <code><a href="#INTO">INTO</a> (<a href="#OPTIONAL">OPTIONAL</a> :C) (<a href="#RESULT">RESULT</a> <a href="#UNIT">UNIT</a> :C)</code>
- <code><a href="#INTO">INTO</a> (<a href="#RESULT">RESULT</a> :C :D) (<a href="#OPTIONAL">OPTIONAL</a> :D)</code>
- <code><a href="#MONAD">MONAD</a> (<a href="#RESULT">RESULT</a> :C)</code>
- <code><a href="#MONOID">MONOID</a> :C => <a href="#MONOID">MONOID</a> (<a href="#RESULT">RESULT</a> :D :C)</code>
- <code><a href="#FUNCTOR">FUNCTOR</a> (<a href="#RESULT">RESULT</a> :C)</code>
- <code><a href="#SEMIGROUP">SEMIGROUP</a> :C => <a href="#SEMIGROUP">SEMIGROUP</a> (<a href="#RESULT">RESULT</a> :D :C)</code>
- <code><a href="#APPLICATIVE">APPLICATIVE</a> (<a href="#RESULT">RESULT</a> :C)</code>
- <code><a href="#UNWRAPPABLE">UNWRAPPABLE</a> (<a href="#RESULT">RESULT</a> :C)</code>

</details>


***

#### <code>TUPLE3 :A :B :C</code> <sup><sub>[TYPE]</sub></sup><a name="TUPLE3"></a>
- <code>(TUPLE3 :A :B :C)</code>

Constructors:
- <code>TUPLE3 :: (:A → :B → :C → (<a href="#TUPLE3">TUPLE3</a> :A :B :C))</code>

<details>
<summary>Instances</summary>

- <code>(<a href="#EQ">EQ</a> :D) (<a href="#EQ">EQ</a> :E) (<a href="#EQ">EQ</a> :F) => <a href="#EQ">EQ</a> (<a href="#TUPLE3">TUPLE3</a> :D :E :F)</code>

</details>


***

#### <code>TUPLE4 :A :B :C :D</code> <sup><sub>[TYPE]</sub></sup><a name="TUPLE4"></a>
- <code>(TUPLE4 :A :B :C :D)</code>

Constructors:
- <code>TUPLE4 :: (:A → :B → :C → :D → (<a href="#TUPLE4">TUPLE4</a> :A :B :C :D))</code>

<details>
<summary>Instances</summary>

- <code>(<a href="#EQ">EQ</a> :E) (<a href="#EQ">EQ</a> :F) (<a href="#EQ">EQ</a> :G) (<a href="#EQ">EQ</a> :H) => <a href="#EQ">EQ</a> (<a href="#TUPLE4">TUPLE4</a> :E :F :G :H)</code>

</details>


***

#### <code>TUPLE5 :A :B :C :D :E</code> <sup><sub>[TYPE]</sub></sup><a name="TUPLE5"></a>
- <code>(TUPLE5 :A :B :C :D :E)</code>

Constructors:
- <code>TUPLE5 :: (:A → :B → :C → :D → :E → (<a href="#TUPLE5">TUPLE5</a> :A :B :C :D :E))</code>

<details>
<summary>Instances</summary>

- <code>(<a href="#EQ">EQ</a> :F) (<a href="#EQ">EQ</a> :G) (<a href="#EQ">EQ</a> :H) (<a href="#EQ">EQ</a> :I) (<a href="#EQ">EQ</a> :J) => <a href="#EQ">EQ</a> (<a href="#TUPLE5">TUPLE5</a> :F :G :H :I :J)</code>

</details>


***

#### <code>FRACTION</code> <sup><sub>[TYPE]</sub></sup><a name="FRACTION"></a>
- <code>(%FRACTION <a href="#INTEGER">INTEGER</a> <a href="#INTEGER">INTEGER</a>)</code>

A ratio of integers always in reduced form.

Constructors:
- <code>%FRACTION :: (<a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a> → <a href="#FRACTION">FRACTION</a>)</code>

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
- <code>SOME :: (:A → (<a href="#OPTIONAL">OPTIONAL</a> :A))</code>
- <code>NONE :: (<a href="#OPTIONAL">OPTIONAL</a> :A)</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> :B => <a href="#EQ">EQ</a> (<a href="#OPTIONAL">OPTIONAL</a> :B)</code>
- <code><a href="#ISO">ISO</a> (<a href="#RESULT">RESULT</a> <a href="#UNIT">UNIT</a> :B) (<a href="#OPTIONAL">OPTIONAL</a> :B)</code>
- <code><a href="#ORD">ORD</a> :B => <a href="#ORD">ORD</a> (<a href="#OPTIONAL">OPTIONAL</a> :B)</code>
- <code><a href="#INTO">INTO</a> (<a href="#OPTIONAL">OPTIONAL</a> :B) (<a href="#RESULT">RESULT</a> <a href="#UNIT">UNIT</a> :B)</code>
- <code><a href="#INTO">INTO</a> (<a href="#RESULT">RESULT</a> :B :C) (<a href="#OPTIONAL">OPTIONAL</a> :C)</code>
- <code><a href="#MONAD">MONAD</a> <a href="#OPTIONAL">OPTIONAL</a></code>
- <code><a href="#MONOID">MONOID</a> :B => <a href="#MONOID">MONOID</a> (<a href="#OPTIONAL">OPTIONAL</a> :B)</code>
- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#OPTIONAL">OPTIONAL</a></code>
- <code><a href="#MONADFAIL">MONADFAIL</a> <a href="#OPTIONAL">OPTIONAL</a></code>
- <code><a href="#SEMIGROUP">SEMIGROUP</a> :B => <a href="#SEMIGROUP">SEMIGROUP</a> (<a href="#OPTIONAL">OPTIONAL</a> :B)</code>
- <code><a href="#ALTERNATIVE">ALTERNATIVE</a> <a href="#OPTIONAL">OPTIONAL</a></code>
- <code><a href="#APPLICATIVE">APPLICATIVE</a> <a href="#OPTIONAL">OPTIONAL</a></code>
- <code><a href="#UNWRAPPABLE">UNWRAPPABLE</a> <a href="#OPTIONAL">OPTIONAL</a></code>

</details>


***

### Functions

#### <code>NOT</code> <sup><sub>[FUNCTION]</sub></sup><a name="NOT"></a>
<code>(<a href="#BOOLEAN">BOOLEAN</a> → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Synonym for BOOLEAN-NOT.


***

#### <code>XOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="XOR"></a>
<code>(<a href="#BOOLEAN">BOOLEAN</a> → <a href="#BOOLEAN">BOOLEAN</a> → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Synonym for BOOLEAN-XOR.


***

#### <code>BOOLEAN-OR</code> <sup><sub>[FUNCTION]</sub></sup><a name="BOOLEAN-OR"></a>
<code>(<a href="#BOOLEAN">BOOLEAN</a> → <a href="#BOOLEAN">BOOLEAN</a> → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Is X or Y True? Note that this is a *function* which means both X and Y will be evaluated. Use the OR macro for short-circuiting behavior.


***

#### <code>BOOLEAN-AND</code> <sup><sub>[FUNCTION]</sub></sup><a name="BOOLEAN-AND"></a>
<code>(<a href="#BOOLEAN">BOOLEAN</a> → <a href="#BOOLEAN">BOOLEAN</a> → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Are X and Y True? Note that this is a *function* which means both X and Y will be evaluated. Use the AND macro for short-circuiting behavior.


***

#### <code>BOOLEAN-NOT</code> <sup><sub>[FUNCTION]</sub></sup><a name="BOOLEAN-NOT"></a>
<code>(<a href="#BOOLEAN">BOOLEAN</a> → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Is X False?


***

#### <code>BOOLEAN-XOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="BOOLEAN-XOR"></a>
<code>(<a href="#BOOLEAN">BOOLEAN</a> → <a href="#BOOLEAN">BOOLEAN</a> → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Are X or Y True, but not both?


***

## File: [classes.lisp](../src/library/classes.lisp)

### Types

#### <code>ORD</code> <sup><sub>[TYPE]</sub></sup><a name="ORD"></a>
- <code>LT</code>
- <code>GT</code>
- <code>EQ</code>

Constructors:
- <code>LT :: <a href="#ORD">ORD</a></code>
- <code>GT :: <a href="#ORD">ORD</a></code>
- <code>EQ :: <a href="#ORD">ORD</a></code>


***

#### <code>QUANTIZATION :A</code> <sup><sub>[TYPE]</sub></sup><a name="QUANTIZATION"></a>
- <code>(QUANTIZATION :A <a href="#INTEGER">INTEGER</a> :A <a href="#INTEGER">INTEGER</a> :A)</code>

Represents an integer quantization of `:t`. See the `Quantizable` typeclass.

The fields are defined as follows:

1. A value of type `:t`.

2. The greatest integer less than or equal to a particular value.

3. The remainder of this as a value of type `:t`.

4. The least integer greater than or equal to a particular value.

5. The remainder of this as a value of type `:t`.


Constructors:
- <code>QUANTIZATION :: (:A → <a href="#INTEGER">INTEGER</a> → :A → <a href="#INTEGER">INTEGER</a> → :A → (<a href="#QUANTIZATION">QUANTIZATION</a> :A))</code>


***

### Classes

#### <code>EQ</code> <sup><sub>[CLASS]</sub></sup><a name="EQ"></a>
<code><a href="#EQ">EQ</a> :A</code>

Types which have equality defined.

Methods:
- <code>== :: (:A → :A → <a href="#BOOLEAN">BOOLEAN</a>)</code>

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
- <code><a href="#EQ">EQ</a> :A => <a href="#EQ">EQ</a> (<a href="#OPTIONAL">OPTIONAL</a> :A)</code>
- <code><a href="#EQ">EQ</a> :A => <a href="#EQ">EQ</a> (<a href="#LIST">LIST</a> :A)</code>
- <code>(<a href="#EQ">EQ</a> :A) (<a href="#EQ">EQ</a> :B) => <a href="#EQ">EQ</a> (<a href="#TUPLE">TUPLE</a> :A :B)</code>
- <code>(<a href="#EQ">EQ</a> :A) (<a href="#EQ">EQ</a> :B) (<a href="#EQ">EQ</a> :C) => <a href="#EQ">EQ</a> (<a href="#TUPLE3">TUPLE3</a> :A :B :C)</code>
- <code>(<a href="#EQ">EQ</a> :A) (<a href="#EQ">EQ</a> :B) (<a href="#EQ">EQ</a> :C) (<a href="#EQ">EQ</a> :D) => <a href="#EQ">EQ</a> (<a href="#TUPLE4">TUPLE4</a> :A :B :C :D)</code>
- <code>(<a href="#EQ">EQ</a> :A) (<a href="#EQ">EQ</a> :B) (<a href="#EQ">EQ</a> :C) (<a href="#EQ">EQ</a> :D) (<a href="#EQ">EQ</a> :E) => <a href="#EQ">EQ</a> (<a href="#TUPLE5">TUPLE5</a> :A :B :C :D :E)</code>
- <code>(<a href="#EQ">EQ</a> :A) (<a href="#EQ">EQ</a> :B) => <a href="#EQ">EQ</a> (<a href="#RESULT">RESULT</a> :A :B)</code>
- <code><a href="#EQ">EQ</a> :A => <a href="#EQ">EQ</a> (<a href="#CELL">CELL</a> :A)</code>
- <code><a href="#EQ">EQ</a> :A => <a href="#EQ">EQ</a> (<a href="#VECTOR">VECTOR</a> :A)</code>
- <code><a href="#EQ">EQ</a> :A => <a href="#EQ">EQ</a> (<a href="#SLICE">SLICE</a> :A)</code>
- <code><a href="#EQ">EQ</a> <a href="#EDGEINDEX">EDGEINDEX</a></code>
- <code><a href="#EQ">EQ</a> <a href="#NODEINDEX">NODEINDEX</a></code>

</details>


***

#### <code>NUM</code> <sup><sub>[CLASS]</sub></sup><a name="NUM"></a>
<code><a href="#EQ">EQ</a> :A => <a href="#NUM">NUM</a> :A</code>

Types which have numeric operations defined.

Methods:
- <code>+ :: (:A → :A → :A)</code>
- <code>- :: (:A → :A → :A)</code>
- <code>* :: (:A → :A → :A)</code>
- <code>FROMINT :: (<a href="#INTEGER">INTEGER</a> → :A)</code>

<details>
<summary>Instances</summary>

- <code><a href="#NUM">NUM</a> <a href="#I32">I32</a></code>
- <code><a href="#NUM">NUM</a> <a href="#I64">I64</a></code>
- <code><a href="#NUM">NUM</a> <a href="#U8">U8</a></code>
- <code><a href="#NUM">NUM</a> <a href="#U32">U32</a></code>
- <code><a href="#NUM">NUM</a> <a href="#U64">U64</a></code>
- <code><a href="#NUM">NUM</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#NUM">NUM</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#NUM">NUM</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#NUM">NUM</a> <a href="#FRACTION">FRACTION</a></code>
- <code><a href="#NUM">NUM</a> :A => <a href="#NUM">NUM</a> (<a href="#CELL">CELL</a> :A)</code>

</details>


***

#### <code>ORD</code> <sup><sub>[CLASS]</sub></sup><a name="ORD"></a>
<code><a href="#EQ">EQ</a> :A => <a href="#ORD">ORD</a> :A</code>

Types whose values can be ordered.

Methods:
- <code><=> :: (:A → :A → <a href="#ORD">ORD</a>)</code>

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
- <code><a href="#ORD">ORD</a> :A => <a href="#ORD">ORD</a> (<a href="#OPTIONAL">OPTIONAL</a> :A)</code>
- <code>(<a href="#ORD">ORD</a> :A) (<a href="#ORD">ORD</a> :B) => <a href="#ORD">ORD</a> (<a href="#TUPLE">TUPLE</a> :A :B)</code>
- <code>(<a href="#ORD">ORD</a> :A) (<a href="#ORD">ORD</a> :B) => <a href="#ORD">ORD</a> (<a href="#RESULT">RESULT</a> :A :B)</code>

</details>


***

#### <code>INTO</code> <sup><sub>[CLASS]</sub></sup><a name="INTO"></a>
<code><a href="#INTO">INTO</a> :A :B</code>

INTO imples *every* element of :FROM can be represented by an element of :TO. This conversion might not be injective (i.e., there may be elements in :TO that don't correspond to any in :FROM).

Methods:
- <code>INTO :: (:A → :B)</code>

<details>
<summary>Instances</summary>

- <code><a href="#INTO">INTO</a> :A :A</code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#I32">I32</a></code>
- <code><a href="#INTO">INTO</a> <a href="#I32">I32</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#I64">I64</a></code>
- <code><a href="#INTO">INTO</a> <a href="#I64">I64</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#U8">U8</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U8">U8</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U8">U8</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U8">U8</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#U32">U32</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U32">U32</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U32">U32</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U32">U32</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#U64">U64</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U64">U64</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U64">U64</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#U64">U64</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#INTO">INTO</a> <a href="#INTEGER">INTEGER</a> <a href="#STRING">STRING</a></code>
- <code><a href="#INTO">INTO</a> <a href="#STRING">STRING</a> (<a href="#LIST">LIST</a> <a href="#CHAR">CHAR</a>)</code>
- <code><a href="#INTO">INTO</a> (<a href="#LIST">LIST</a> <a href="#CHAR">CHAR</a>) <a href="#STRING">STRING</a></code>
- <code><a href="#INTO">INTO</a> (<a href="#TUPLE">TUPLE</a> :A :B) (<a href="#TUPLE">TUPLE</a> :B :A)</code>
- <code><a href="#INTO">INTO</a> (<a href="#RESULT">RESULT</a> :A :B) (<a href="#OPTIONAL">OPTIONAL</a> :B)</code>
- <code><a href="#INTO">INTO</a> (<a href="#OPTIONAL">OPTIONAL</a> :A) (<a href="#RESULT">RESULT</a> <a href="#UNIT">UNIT</a> :A)</code>
- <code><a href="#INTO">INTO</a> :A (<a href="#CELL">CELL</a> :A)</code>
- <code><a href="#INTO">INTO</a> (<a href="#CELL">CELL</a> :A) :A</code>
- <code><a href="#INTO">INTO</a> (<a href="#LIST">LIST</a> :A) (<a href="#VECTOR">VECTOR</a> :A)</code>
- <code><a href="#INTO">INTO</a> (<a href="#VECTOR">VECTOR</a> :A) (<a href="#LIST">LIST</a> :A)</code>
- <code><a href="#INTO">INTO</a> (<a href="#SLICE">SLICE</a> :A) (<a href="#VECTOR">VECTOR</a> :A)</code>
- <code><a href="#INTO">INTO</a> (<a href="#VECTOR">VECTOR</a> :A) (<a href="#SLICE">SLICE</a> :A)</code>
- <code><a href="#INTO">INTO</a> <a href="#EDGEINDEX">EDGEINDEX</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#INTO">INTO</a> <a href="#NODEINDEX">NODEINDEX</a> <a href="#INTEGER">INTEGER</a></code>

</details>


***

#### <code>MONAD</code> <sup><sub>[CLASS]</sub></sup><a name="MONAD"></a>
<code><a href="#APPLICATIVE">APPLICATIVE</a> :A => <a href="#MONAD">MONAD</a> :A</code>

Types which are monads as defined in Haskell. See https://wiki.haskell.org/Monad for more information.

Methods:
- <code>>>= :: ∀ :B :C. ((:A :B) → (:B → (:A :C)) → (:A :C))</code>
- <code>>> :: ∀ :B :C. ((:A :B) → (:A :C) → (:A :C))</code>

<details>
<summary>Instances</summary>

- <code><a href="#MONAD">MONAD</a> <a href="#OPTIONAL">OPTIONAL</a></code>
- <code><a href="#MONAD">MONAD</a> <a href="#LIST">LIST</a></code>
- <code><a href="#MONAD">MONAD</a> (<a href="#RESULT">RESULT</a> :A)</code>

</details>


***

#### <code>MONOID</code> <sup><sub>[CLASS]</sub></sup><a name="MONOID"></a>
<code><a href="#SEMIGROUP">SEMIGROUP</a> :A => <a href="#MONOID">MONOID</a> :A</code>

Types with an associative binary operation and identity defined.

Methods:
- <code>MEMPTY :: :A</code>

<details>
<summary>Instances</summary>

- <code><a href="#MONOID">MONOID</a> <a href="#STRING">STRING</a></code>
- <code><a href="#MONOID">MONOID</a> :A => <a href="#MONOID">MONOID</a> (<a href="#OPTIONAL">OPTIONAL</a> :A)</code>
- <code><a href="#MONOID">MONOID</a> (<a href="#LIST">LIST</a> :A)</code>
- <code><a href="#MONOID">MONOID</a> :A => <a href="#MONOID">MONOID</a> (<a href="#RESULT">RESULT</a> :B :A)</code>

</details>


***

#### <code>FUNCTOR</code> <sup><sub>[CLASS]</sub></sup><a name="FUNCTOR"></a>
<code><a href="#FUNCTOR">FUNCTOR</a> :A</code>

Types which can map an inner type where the mapping adheres to the identity and composition laws.

Methods:
- <code>MAP :: ∀ :B :C. ((:B → :C) → (:A :B) → (:A :C))</code>

<details>
<summary>Instances</summary>

- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#OPTIONAL">OPTIONAL</a></code>
- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#LIST">LIST</a></code>
- <code><a href="#FUNCTOR">FUNCTOR</a> (<a href="#RESULT">RESULT</a> :A)</code>
- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#CELL">CELL</a></code>
- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#VECTOR">VECTOR</a></code>

</details>


***

#### <code>TRYINTO</code> <sup><sub>[CLASS]</sub></sup><a name="TRYINTO"></a>
<code><a href="#TRYINTO">TRYINTO</a> :A :B</code>

TRY-INTO implies *most* elements of :FROM can be represented exactly by an element of :TO, but sometimes not. If not, an error string is returned.

Methods:
- <code>TRYINTO :: (:A → (<a href="#RESULT">RESULT</a> <a href="#STRING">STRING</a> :B))</code>

<details>
<summary>Instances</summary>

- <code><a href="#TRYINTO">TRYINTO</a> <a href="#STRING">STRING</a> <a href="#INTEGER">INTEGER</a></code>

</details>


***

#### <code>DIVIDABLE</code> <sup><sub>[CLASS]</sub></sup><a name="DIVIDABLE"></a>
<code>(<a href="#NUM">NUM</a> :A) (<a href="#NUM">NUM</a> :B) => <a href="#DIVIDABLE">DIVIDABLE</a> :A :B</code>

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

- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#INTEGER">INTEGER</a> <a href="#FRACTION">FRACTION</a></code>
- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#INTEGER">INTEGER</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#DIVIDABLE">DIVIDABLE</a> <a href="#INTEGER">INTEGER</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>

</details>


***

#### <code>MONADFAIL</code> <sup><sub>[CLASS]</sub></sup><a name="MONADFAIL"></a>
<code><a href="#MONAD">MONAD</a> :A => <a href="#MONADFAIL">MONADFAIL</a> :A</code>

Methods:
- <code>FAIL :: ∀ :B. (<a href="#STRING">STRING</a> → (:A :B))</code>

<details>
<summary>Instances</summary>

- <code><a href="#MONADFAIL">MONADFAIL</a> <a href="#OPTIONAL">OPTIONAL</a></code>

</details>


***

#### <code>SEMIGROUP</code> <sup><sub>[CLASS]</sub></sup><a name="SEMIGROUP"></a>
<code><a href="#SEMIGROUP">SEMIGROUP</a> :A</code>

Types with an associative binary operation defined.

Methods:
- <code><> :: (:A → :A → :A)</code>

<details>
<summary>Instances</summary>

- <code><a href="#SEMIGROUP">SEMIGROUP</a> <a href="#STRING">STRING</a></code>
- <code><a href="#SEMIGROUP">SEMIGROUP</a> :A => <a href="#SEMIGROUP">SEMIGROUP</a> (<a href="#OPTIONAL">OPTIONAL</a> :A)</code>
- <code><a href="#SEMIGROUP">SEMIGROUP</a> (<a href="#LIST">LIST</a> :A)</code>
- <code><a href="#SEMIGROUP">SEMIGROUP</a> :A => <a href="#SEMIGROUP">SEMIGROUP</a> (<a href="#RESULT">RESULT</a> :B :A)</code>
- <code><a href="#SEMIGROUP">SEMIGROUP</a> :A => <a href="#SEMIGROUP">SEMIGROUP</a> (<a href="#CELL">CELL</a> :A)</code>
- <code><a href="#SEMIGROUP">SEMIGROUP</a> (<a href="#VECTOR">VECTOR</a> :A)</code>

</details>


***

#### <code>APPLICATIVE</code> <sup><sub>[CLASS]</sub></sup><a name="APPLICATIVE"></a>
<code><a href="#FUNCTOR">FUNCTOR</a> :A => <a href="#APPLICATIVE">APPLICATIVE</a> :A</code>

Types which are a functor which can embed pure expressions and sequence operations.

Methods:
- <code>PURE :: ∀ :B. (:B → (:A :B))</code>
- <code>LIFTA2 :: ∀ :B :C :D. ((:B → :C → :D) → (:A :B) → (:A :C) → (:A :D))</code>

<details>
<summary>Instances</summary>

- <code><a href="#APPLICATIVE">APPLICATIVE</a> <a href="#OPTIONAL">OPTIONAL</a></code>
- <code><a href="#APPLICATIVE">APPLICATIVE</a> <a href="#LIST">LIST</a></code>
- <code><a href="#APPLICATIVE">APPLICATIVE</a> (<a href="#RESULT">RESULT</a> :A)</code>
- <code><a href="#APPLICATIVE">APPLICATIVE</a> <a href="#CELL">CELL</a></code>

</details>


***

#### <code>QUANTIZABLE</code> <sup><sub>[CLASS]</sub></sup><a name="QUANTIZABLE"></a>
<code>(<a href="#ORD">ORD</a> :A) (<a href="#NUM">NUM</a> :A) => <a href="#QUANTIZABLE">QUANTIZABLE</a> :A</code>

The representation of a type that allows "quantizing", "snapping to integers", or "rounding." (All of these concepts are roughly equivalent.)


Methods:
- <code>QUANTIZE :: (:A → (<a href="#QUANTIZATION">QUANTIZATION</a> :A))</code>

<details>
<summary>Instances</summary>

- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#INTEGER">INTEGER</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#I32">I32</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#I64">I64</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#U8">U8</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#U32">U32</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#U64">U64</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a></code>
- <code><a href="#QUANTIZABLE">QUANTIZABLE</a> <a href="#FRACTION">FRACTION</a></code>

</details>


***

### Functions

#### <code><</code> <sup><sub>[FUNCTION]</sub></sup><a name="<"></a>
<code>∀ :A. <a href="#ORD">ORD</a> :A => (:A → :A → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Is X less than Y?


***

#### <code>></code> <sup><sub>[FUNCTION]</sub></sup><a name=">"></a>
<code>∀ :A. <a href="#ORD">ORD</a> :A => (:A → :A → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Is X greater than Y?


***

#### <code>/=</code> <sup><sub>[FUNCTION]</sub></sup><a name="/="></a>
<code>∀ :A. <a href="#EQ">EQ</a> :A => (:A → :A → <a href="#BOOLEAN">BOOLEAN</a>)</code>

***

#### <code><=</code> <sup><sub>[FUNCTION]</sub></sup><a name="<="></a>
<code>∀ :A. <a href="#ORD">ORD</a> :A => (:A → :A → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Is X less than or equal to Y?


***

#### <code>>=</code> <sup><sub>[FUNCTION]</sub></sup><a name=">="></a>
<code>∀ :A. <a href="#ORD">ORD</a> :A => (:A → :A → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Is X greater than or equal to Y?


***

#### <code>MAX</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAX"></a>
<code>∀ :A. <a href="#ORD">ORD</a> :A => (:A → :A → :A)</code>

Returns the greater element of X and Y.


***

#### <code>MIN</code> <sup><sub>[FUNCTION]</sub></sup><a name="MIN"></a>
<code>∀ :A. <a href="#ORD">ORD</a> :A => (:A → :A → :A)</code>

Returns the lesser element of X and Y.


***

## File: [builtin.lisp](../src/library/builtin.lisp)

### Functions

#### <code>ERROR</code> <sup><sub>[FUNCTION]</sub></sup><a name="ERROR"></a>
<code>∀ :A. (<a href="#STRING">STRING</a> → :A)</code>

Signal an error by calling CL:ERROR


***

#### <code>UNDEFINED</code> <sup><sub>[FUNCTION]</sub></sup><a name="UNDEFINED"></a>
<code>∀ :A :B. (:A → :B)</code>

A function which can be used in place of any value, throwing an error at runtime.


***

## File: [fraction.lisp](../src/library/fraction.lisp)

### Functions

#### <code>NUMERATOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="NUMERATOR"></a>
<code>(<a href="#FRACTION">FRACTION</a> → <a href="#INTEGER">INTEGER</a>)</code>

The numerator of a fraction Q.


***

#### <code>DENOMINATOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="DENOMINATOR"></a>
<code>(<a href="#FRACTION">FRACTION</a> → <a href="#INTEGER">INTEGER</a>)</code>

The denominator of a fraction Q.


***

## File: [arith.lisp](../src/library/arith.lisp)

### Functions

#### <code>ABS</code> <sup><sub>[FUNCTION]</sub></sup><a name="ABS"></a>
<code>∀ :A. (<a href="#NUM">NUM</a> :A) (<a href="#ORD">ORD</a> :A) => (:A → :A)</code>

Absolute value of X.


***

#### <code>ASH</code> <sup><sub>[FUNCTION]</sub></sup><a name="ASH"></a>
<code>(<a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a>)</code>

Compute the "arithmetic shift" of X by N. 


***

#### <code>GCD</code> <sup><sub>[FUNCTION]</sub></sup><a name="GCD"></a>
<code>(<a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a>)</code>

Compute the greatest common divisor of A and B.


***

#### <code>LCM</code> <sup><sub>[FUNCTION]</sub></sup><a name="LCM"></a>
<code>(<a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a>)</code>

Compute the least common multiple of A and B.


***

#### <code>MOD</code> <sup><sub>[FUNCTION]</sub></sup><a name="MOD"></a>
<code>(<a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a>)</code>

Compute NUM modulo BASE.


***

#### <code>ODD</code> <sup><sub>[FUNCTION]</sub></sup><a name="ODD"></a>
<code>(<a href="#INTEGER">INTEGER</a> → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Is N odd?


***

#### <code>EVEN</code> <sup><sub>[FUNCTION]</sub></sup><a name="EVEN"></a>
<code>(<a href="#INTEGER">INTEGER</a> → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Is N even?


***

#### <code>EXPT</code> <sup><sub>[FUNCTION]</sub></sup><a name="EXPT"></a>
<code>(<a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a>)</code>

Exponentiate BASE to a non-negative POWER.


***

#### <code>SIGN</code> <sup><sub>[FUNCTION]</sub></sup><a name="SIGN"></a>
<code>∀ :A. (<a href="#NUM">NUM</a> :A) (<a href="#ORD">ORD</a> :A) => (:A → <a href="#INTEGER">INTEGER</a>)</code>

The sign of X.


***

#### <code>NEGATE</code> <sup><sub>[FUNCTION]</sub></sup><a name="NEGATE"></a>
<code>∀ :A. <a href="#NUM">NUM</a> :A => (:A → :A)</code>

***

#### <code>INTEGER->STRING</code> <sup><sub>[FUNCTION]</sub></sup><a name="INTEGER->STRING"></a>
<code>(<a href="#INTEGER">INTEGER</a> → <a href="#STRING">STRING</a>)</code>

***

#### <code>DOUBLE-FLOAT->INTEGER</code> <sup><sub>[FUNCTION]</sub></sup><a name="DOUBLE-FLOAT->INTEGER"></a>
<code>(<a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a> → (<a href="#OPTIONAL">OPTIONAL</a> <a href="#INTEGER">INTEGER</a>))</code>

Round a Double-Float to the nearest Integer.


***

#### <code>INTEGER->DOUBLE-FLOAT</code> <sup><sub>[FUNCTION]</sub></sup><a name="INTEGER->DOUBLE-FLOAT"></a>
<code>(<a href="#INTEGER">INTEGER</a> → <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a>)</code>

***

#### <code>INTEGER->SINGLE-FLOAT</code> <sup><sub>[FUNCTION]</sub></sup><a name="INTEGER->SINGLE-FLOAT"></a>
<code>(<a href="#INTEGER">INTEGER</a> → <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a>)</code>

***

#### <code>SINGLE-FLOAT->INTEGER</code> <sup><sub>[FUNCTION]</sub></sup><a name="SINGLE-FLOAT->INTEGER"></a>
<code>(<a href="#SINGLE-FLOAT">SINGLE-FLOAT</a> → (<a href="#OPTIONAL">OPTIONAL</a> <a href="#INTEGER">INTEGER</a>))</code>

Round a Single-Float to the nearest Integer.


***

## File: [string.lisp](../src/library/string.lisp)

### Functions

#### <code>PARSE-INT</code> <sup><sub>[FUNCTION]</sub></sup><a name="PARSE-INT"></a>
<code>(<a href="#STRING">STRING</a> → (<a href="#OPTIONAL">OPTIONAL</a> <a href="#INTEGER">INTEGER</a>))</code>

Parse the integer in string STR.


***

#### <code>PACK-STRING</code> <sup><sub>[FUNCTION]</sub></sup><a name="PACK-STRING"></a>
<code>((<a href="#LIST">LIST</a> <a href="#CHAR">CHAR</a>) → <a href="#STRING">STRING</a>)</code>

Pack a list of charactes into a string.


***

#### <code>CONCAT-STRING</code> <sup><sub>[FUNCTION]</sub></sup><a name="CONCAT-STRING"></a>
<code>(<a href="#STRING">STRING</a> → <a href="#STRING">STRING</a> → <a href="#STRING">STRING</a>)</code>

Concatenate STR1 and STR2 together, returning a new string.


***

#### <code>UNPACK-STRING</code> <sup><sub>[FUNCTION]</sub></sup><a name="UNPACK-STRING"></a>
<code>(<a href="#STRING">STRING</a> → (<a href="#LIST">LIST</a> <a href="#CHAR">CHAR</a>))</code>

Unpack a string into a list of characters.


***

## File: [optional.lisp](../src/library/optional.lisp)

### Functions

#### <code>ISNONE</code> <sup><sub>[FUNCTION]</sub></sup><a name="ISNONE"></a>
<code>∀ :A. ((<a href="#OPTIONAL">OPTIONAL</a> :A) → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Is X None?


***

#### <code>ISSOME</code> <sup><sub>[FUNCTION]</sub></sup><a name="ISSOME"></a>
<code>∀ :A. ((<a href="#OPTIONAL">OPTIONAL</a> :A) → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Is X Some?


***

#### <code>FROMSOME</code> <sup><sub>[FUNCTION]</sub></sup><a name="FROMSOME"></a>
<code>∀ :A. (<a href="#STRING">STRING</a> → (<a href="#OPTIONAL">OPTIONAL</a> :A) → :A)</code>

Get the value of OPT, erroring with the provided string if it is None.


***

## File: [list.lisp](../src/library/list.lisp)

### Functions

#### <code>ALL</code> <sup><sub>[FUNCTION]</sub></sup><a name="ALL"></a>
<code>∀ :A. ((:A → <a href="#BOOLEAN">BOOLEAN</a>) → (<a href="#LIST">LIST</a> :A) → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Returns TRUE if every element in XS matches F.


***

#### <code>ANY</code> <sup><sub>[FUNCTION]</sub></sup><a name="ANY"></a>
<code>∀ :A. ((:A → <a href="#BOOLEAN">BOOLEAN</a>) → (<a href="#LIST">LIST</a> :A) → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Returns TRUE if at least one element in XS matches F.


***

#### <code>SUM</code> <sup><sub>[FUNCTION]</sub></sup><a name="SUM"></a>
<code>∀ :A. <a href="#NUM">NUM</a> :A => ((<a href="#LIST">LIST</a> :A) → :A)</code>

Returns the sum of XS


***

#### <code>ZIP</code> <sup><sub>[FUNCTION]</sub></sup><a name="ZIP"></a>
<code>∀ :A :B. ((<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :B) → (<a href="#LIST">LIST</a> (<a href="#TUPLE">TUPLE</a> :A :B)))</code>

Builds a list of tuples with the elements of XS and YS.


***

#### <code>DROP</code> <sup><sub>[FUNCTION]</sub></sup><a name="DROP"></a>
<code>∀ :A. (<a href="#INTEGER">INTEGER</a> → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

Returns a list with the first N elements of XS removed


***

#### <code>FIND</code> <sup><sub>[FUNCTION]</sub></sup><a name="FIND"></a>
<code>∀ :A. ((:A → <a href="#BOOLEAN">BOOLEAN</a>) → (<a href="#LIST">LIST</a> :A) → (<a href="#OPTIONAL">OPTIONAL</a> :A))</code>

Returns the first element in a list matching the predicate function F.


***

#### <code>FOLD</code> <sup><sub>[FUNCTION]</sub></sup><a name="FOLD"></a>
<code>∀ :A :B. ((:A → :B → :B) → :B → (<a href="#LIST">LIST</a> :A) → :B)</code>

Tail recursive left fold on lists.


***

#### <code>HEAD</code> <sup><sub>[FUNCTION]</sub></sup><a name="HEAD"></a>
<code>∀ :A. ((<a href="#LIST">LIST</a> :A) → (<a href="#OPTIONAL">OPTIONAL</a> :A))</code>

Returns the first element of a list.


***

#### <code>INIT</code> <sup><sub>[FUNCTION]</sub></sup><a name="INIT"></a>
<code>∀ :A. ((<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

Returns every element except the last in a list.


***

#### <code>LAST</code> <sup><sub>[FUNCTION]</sub></sup><a name="LAST"></a>
<code>∀ :A. ((<a href="#LIST">LIST</a> :A) → (<a href="#OPTIONAL">OPTIONAL</a> :A))</code>

Returns the last element of a list.


***

#### <code>NULL</code> <sup><sub>[FUNCTION]</sub></sup><a name="NULL"></a>
<code>∀ :A. ((<a href="#LIST">LIST</a> :A) → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Returns TRUE if XS is an empty list.


***

#### <code>SORT</code> <sup><sub>[FUNCTION]</sub></sup><a name="SORT"></a>
<code>∀ :A. <a href="#ORD">ORD</a> :A => ((<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

Performs a stable sort of XS.


***

#### <code>TAIL</code> <sup><sub>[FUNCTION]</sub></sup><a name="TAIL"></a>
<code>∀ :A. ((<a href="#LIST">LIST</a> :A) → (<a href="#OPTIONAL">OPTIONAL</a> (<a href="#LIST">LIST</a> :A)))</code>

Returns every element except the first in a list.


***

#### <code>TAKE</code> <sup><sub>[FUNCTION]</sub></sup><a name="TAKE"></a>
<code>∀ :A. (<a href="#INTEGER">INTEGER</a> → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

Returns the first N elements of XS


***

#### <code>FOLDR</code> <sup><sub>[FUNCTION]</sub></sup><a name="FOLDR"></a>
<code>∀ :A :B. ((:A → :B → :B) → :B → (<a href="#LIST">LIST</a> :A) → :B)</code>

Right fold on lists. Is not tail recursive.


***

#### <code>INDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="INDEX"></a>
<code>∀ :A. ((<a href="#LIST">LIST</a> :A) → <a href="#INTEGER">INTEGER</a> → (<a href="#OPTIONAL">OPTIONAL</a> :A))</code>

Returns the Ith element of XS.


***

#### <code>RANGE</code> <sup><sub>[FUNCTION]</sub></sup><a name="RANGE"></a>
<code>(<a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a> → (<a href="#LIST">LIST</a> <a href="#INTEGER">INTEGER</a>))</code>

Returns a list containing the numbers from START to END inclusive.


***

#### <code>SPLIT</code> <sup><sub>[FUNCTION]</sub></sup><a name="SPLIT"></a>
<code>(<a href="#CHAR">CHAR</a> → <a href="#STRING">STRING</a> → (<a href="#LIST">LIST</a> <a href="#STRING">STRING</a>))</code>

***

#### <code>UNION</code> <sup><sub>[FUNCTION]</sub></sup><a name="UNION"></a>
<code>∀ :A. <a href="#EQ">EQ</a> :A => ((<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

Returns a new list with the elements from both XS and YS and without duplicates.


***

#### <code>APPEND</code> <sup><sub>[FUNCTION]</sub></sup><a name="APPEND"></a>
<code>∀ :A. ((<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

Appends two lists together and returns a new list.


***

#### <code>CONCAT</code> <sup><sub>[FUNCTION]</sub></sup><a name="CONCAT"></a>
<code>∀ :A. ((<a href="#LIST">LIST</a> (<a href="#LIST">LIST</a> :A)) → (<a href="#LIST">LIST</a> :A))</code>

Appends a list of lists together into a single new list.


***

#### <code>DELETE</code> <sup><sub>[FUNCTION]</sub></sup><a name="DELETE"></a>
<code>∀ :A. <a href="#EQ">EQ</a> :A => (:A → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

Return a new list with the first element equal to X removed.


***

#### <code>FILTER</code> <sup><sub>[FUNCTION]</sub></sup><a name="FILTER"></a>
<code>∀ :A. ((:A → <a href="#BOOLEAN">BOOLEAN</a>) → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

Returns a new list containing every element of XS that matches the predicate function F in the same order.


***

#### <code>INSERT</code> <sup><sub>[FUNCTION]</sub></sup><a name="INSERT"></a>
<code>∀ :A. <a href="#ORD">ORD</a> :A => (:A → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

Inserts an element into a list at the first place it is less than or equal to the next element.


***

#### <code>LENGTH</code> <sup><sub>[FUNCTION]</sub></sup><a name="LENGTH"></a>
<code>∀ :A. ((<a href="#LIST">LIST</a> :A) → <a href="#INTEGER">INTEGER</a>)</code>

Returns the length of a list.


***

#### <code>LOOKUP</code> <sup><sub>[FUNCTION]</sub></sup><a name="LOOKUP"></a>
<code>∀ :A :B. <a href="#EQ">EQ</a> :A => (:A → (<a href="#LIST">LIST</a> (<a href="#TUPLE">TUPLE</a> :A :B)) → (<a href="#OPTIONAL">OPTIONAL</a> :B))</code>

Returns the value of the first (key, value) tuple in XS where the key matches E.


***

#### <code>MEMBER</code> <sup><sub>[FUNCTION]</sub></sup><a name="MEMBER"></a>
<code>∀ :A. <a href="#EQ">EQ</a> :A => (:A → (<a href="#LIST">LIST</a> :A) → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Returns true if any element of XS is equal to E.


***

#### <code>REPEAT</code> <sup><sub>[FUNCTION]</sub></sup><a name="REPEAT"></a>
<code>∀ :A. (<a href="#INTEGER">INTEGER</a> → :A → (<a href="#LIST">LIST</a> :A))</code>

Returns a list with X repeated N times.


***

#### <code>SORTBY</code> <sup><sub>[FUNCTION]</sub></sup><a name="SORTBY"></a>
<code>∀ :A. ((:A → :A → <a href="#ORD">ORD</a>) → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

Generic version of sort


***

#### <code>MAXIMUM</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAXIMUM"></a>
<code>∀ :A. <a href="#ORD">ORD</a> :A => ((<a href="#LIST">LIST</a> :A) → (<a href="#OPTIONAL">OPTIONAL</a> :A))</code>

Returns the greatest element in XS.


***

#### <code>MINIMUM</code> <sup><sub>[FUNCTION]</sub></sup><a name="MINIMUM"></a>
<code>∀ :A. <a href="#ORD">ORD</a> :A => ((<a href="#LIST">LIST</a> :A) → (<a href="#OPTIONAL">OPTIONAL</a> :A))</code>

Returns the least element in XS.


***

#### <code>PRODUCT</code> <sup><sub>[FUNCTION]</sub></sup><a name="PRODUCT"></a>
<code>∀ :A. <a href="#NUM">NUM</a> :A => ((<a href="#LIST">LIST</a> :A) → :A)</code>

Returns the product of XS


***

#### <code>REVERSE</code> <sup><sub>[FUNCTION]</sub></sup><a name="REVERSE"></a>
<code>∀ :A. ((<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

Returns a new list containing the same elements in reverse order.


***

#### <code>ZIPWITH</code> <sup><sub>[FUNCTION]</sub></sup><a name="ZIPWITH"></a>
<code>∀ :A :B :C. ((:A → :B → :C) → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :B) → (<a href="#LIST">LIST</a> :C))</code>

Builds a new list by calling F with elements of XS and YS.


***

#### <code>INSERTBY</code> <sup><sub>[FUNCTION]</sub></sup><a name="INSERTBY"></a>
<code>∀ :A. ((:A → :A → <a href="#ORD">ORD</a>) → :A → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

Generic version of insert


***

#### <code>ZIPWITH3</code> <sup><sub>[FUNCTION]</sub></sup><a name="ZIPWITH3"></a>
<code>∀ :A :B :C :D. ((:A → :B → :C → :D) → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :B) → (<a href="#LIST">LIST</a> :C) → (<a href="#LIST">LIST</a> :D))</code>

Build a new list by calling F with elements of XS, YS and ZS


***

#### <code>ZIPWITH4</code> <sup><sub>[FUNCTION]</sub></sup><a name="ZIPWITH4"></a>
<code>∀ :A :B :C :D :E. ((:A → :B → :C → :D → :E) → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :B) → (<a href="#LIST">LIST</a> :C) → (<a href="#LIST">LIST</a> :D) → (<a href="#LIST">LIST</a> :E))</code>

Build a new list by calling F with elements of AS, BS, CS and DS


***

#### <code>ZIPWITH5</code> <sup><sub>[FUNCTION]</sub></sup><a name="ZIPWITH5"></a>
<code>∀ :A :B :C :D :E :F. ((:A → :B → :C → :D → :E → :F) → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :B) → (<a href="#LIST">LIST</a> :C) → (<a href="#LIST">LIST</a> :D) → (<a href="#LIST">LIST</a> :E) → (<a href="#LIST">LIST</a> :F))</code>

Build a new list by calling F with elements of AS, BS, CS, DS and ES


***

#### <code>CONCATMAP</code> <sup><sub>[FUNCTION]</sub></sup><a name="CONCATMAP"></a>
<code>∀ :A :B. ((:A → (<a href="#LIST">LIST</a> :B)) → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :B))</code>

Apply F to each element in XS and concatenate the results.


***

#### <code>ELEMINDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="ELEMINDEX"></a>
<code>∀ :A. <a href="#EQ">EQ</a> :A => (:A → (<a href="#LIST">LIST</a> :A) → (<a href="#OPTIONAL">OPTIONAL</a> <a href="#INTEGER">INTEGER</a>))</code>

***

#### <code>FINDINDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="FINDINDEX"></a>
<code>∀ :A. ((:A → <a href="#BOOLEAN">BOOLEAN</a>) → (<a href="#LIST">LIST</a> :A) → (<a href="#OPTIONAL">OPTIONAL</a> <a href="#INTEGER">INTEGER</a>))</code>

***

#### <code>PARTITION</code> <sup><sub>[FUNCTION]</sub></sup><a name="PARTITION"></a>
<code>∀ :A. ((:A → <a href="#BOOLEAN">BOOLEAN</a>) → (<a href="#LIST">LIST</a> :A) → (<a href="#TUPLE">TUPLE</a> (<a href="#LIST">LIST</a> :A) (<a href="#LIST">LIST</a> :A)))</code>

Splits a list into two new lists. The first list contains elements matching predicate F.


***

#### <code>SINGLETON</code> <sup><sub>[FUNCTION]</sub></sup><a name="SINGLETON"></a>
<code>∀ :A. (:A → (<a href="#LIST">LIST</a> :A))</code>

Returns a single element list containg only X.


***

#### <code>TRANSPOSE</code> <sup><sub>[FUNCTION]</sub></sup><a name="TRANSPOSE"></a>
<code>∀ :A. ((<a href="#LIST">LIST</a> (<a href="#LIST">LIST</a> :A)) → (<a href="#LIST">LIST</a> (<a href="#LIST">LIST</a> :A)))</code>

Transposes a matrix represented by a list of lists.


***

#### <code>INTERCALATE</code> <sup><sub>[FUNCTION]</sub></sup><a name="INTERCALATE"></a>
<code>∀ :A. ((<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> (<a href="#LIST">LIST</a> :A)) → (<a href="#LIST">LIST</a> :A))</code>

Intersperses XS into XSS and then concatenates the result.


***

#### <code>INTERSPERSE</code> <sup><sub>[FUNCTION]</sub></sup><a name="INTERSPERSE"></a>
<code>∀ :A. (:A → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

Returns a new list where every other element is E.


***

#### <code>INTERSECTION</code> <sup><sub>[FUNCTION]</sub></sup><a name="INTERSECTION"></a>
<code>∀ :A. <a href="#EQ">EQ</a> :A => ((<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

Returns elements which occur in both lists. Does not return duplicates and does not guarantee order.


***

#### <code>LIST-DIFFERENCE</code> <sup><sub>[FUNCTION]</sub></sup><a name="LIST-DIFFERENCE"></a>
<code>∀ :A. <a href="#EQ">EQ</a> :A => ((<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

Returns a new list with the first occurence of each element in YS deleted from XS.


***

#### <code>REMOVE-DUPLICATES</code> <sup><sub>[FUNCTION]</sub></sup><a name="REMOVE-DUPLICATES"></a>
<code>∀ :A. <a href="#EQ">EQ</a> :A => ((<a href="#LIST">LIST</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

Returns a new list without duplicate elements.


***

## File: [tuple.lisp](../src/library/tuple.lisp)

### Functions

#### <code>FST</code> <sup><sub>[FUNCTION]</sub></sup><a name="FST"></a>
<code>∀ :A :B. ((<a href="#TUPLE">TUPLE</a> :A :B) → :A)</code>

Get the first element of a tuple.


***

#### <code>SND</code> <sup><sub>[FUNCTION]</sub></sup><a name="SND"></a>
<code>∀ :A :B. ((<a href="#TUPLE">TUPLE</a> :A :B) → :B)</code>

Get the second element of a tuple.


***

## File: [result.lisp](../src/library/result.lisp)

### Functions

#### <code>ISOK</code> <sup><sub>[FUNCTION]</sub></sup><a name="ISOK"></a>
<code>∀ :A :B. ((<a href="#RESULT">RESULT</a> :A :B) → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Returns TRUE if X is ERR


***

#### <code>ISERR</code> <sup><sub>[FUNCTION]</sub></sup><a name="ISERR"></a>
<code>∀ :A :B. ((<a href="#RESULT">RESULT</a> :A :B) → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Returns TRUE if X is ERR


***

#### <code>MAPERR</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAPERR"></a>
<code>∀ :A :B :C. ((:A → :B) → (<a href="#RESULT">RESULT</a> :A :C) → (<a href="#RESULT">RESULT</a> :B :C))</code>

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
<code>∀ :A :B. <a href="#ALTERNATIVE">ALTERNATIVE</a> :A => ((<a href="#LIST">LIST</a> (:A :B)) → (:A :B))</code>

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
<code>(<a href="#STRING">STRING</a> → <a href="#UNIT">UNIT</a>)</code>

Print a line to *STANDARD-OUTPUT*


***

#### <code>COMPOSE</code> <sup><sub>[FUNCTION]</sub></sup><a name="COMPOSE"></a>
<code>∀ :A :B :C. ((:A → :B) → (:C → :A) → :C → :B)</code>

***

#### <code>SEQUENCE</code> <sup><sub>[FUNCTION]</sub></sup><a name="SEQUENCE"></a>
<code>∀ :A :B. <a href="#APPLICATIVE">APPLICATIVE</a> :A => ((<a href="#LIST">LIST</a> (:A :B)) → (:A (<a href="#LIST">LIST</a> :B)))</code>

***

#### <code>TRAVERSE</code> <sup><sub>[FUNCTION]</sub></sup><a name="TRAVERSE"></a>
<code>∀ :A :B :C. <a href="#APPLICATIVE">APPLICATIVE</a> :B => ((:A → (:B :C)) → (<a href="#LIST">LIST</a> :A) → (:B (<a href="#LIST">LIST</a> :C)))</code>

Map the elements of XS with F then collect the results.


***

#### <code>TRACEOBJECT</code> <sup><sub>[FUNCTION]</sub></sup><a name="TRACEOBJECT"></a>
<code>∀ :A. (<a href="#STRING">STRING</a> → :A → <a href="#UNIT">UNIT</a>)</code>

Print a line to *STANDARD-OUTPUT* in the form "{STR}: {ITEM}"


***

## File: [quantize.lisp](../src/library/quantize.lisp)

### Functions

#### <code>FLOOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="FLOOR"></a>
<code>∀ :A. <a href="#QUANTIZABLE">QUANTIZABLE</a> :A => (:A → <a href="#INTEGER">INTEGER</a>)</code>

Return the greatest integer less than or equal to X.


***

#### <code>ROUND</code> <sup><sub>[FUNCTION]</sub></sup><a name="ROUND"></a>
<code>∀ :A. <a href="#QUANTIZABLE">QUANTIZABLE</a> :A => (:A → <a href="#INTEGER">INTEGER</a>)</code>

Return the nearest integer to X, with ties breaking toward positive infinity.


***

#### <code>SAFE/</code> <sup><sub>[FUNCTION]</sub></sup><a name="SAFE/"></a>
<code>∀ :A :B. <a href="#DIVIDABLE">DIVIDABLE</a> :A :B => (:A → :A → (<a href="#OPTIONAL">OPTIONAL</a> :B))</code>

Safely divide X by Y, returning None if Y is zero.


***

#### <code>EXACT/</code> <sup><sub>[FUNCTION]</sub></sup><a name="EXACT/"></a>
<code>(<a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a> → <a href="#FRACTION">FRACTION</a>)</code>

Exactly divide two integers and produce a fraction.


***

#### <code>FLOOR/</code> <sup><sub>[FUNCTION]</sub></sup><a name="FLOOR/"></a>
<code>(<a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a>)</code>

Divide two integers and compute the floor of the quotient.


***

#### <code>ROUND/</code> <sup><sub>[FUNCTION]</sub></sup><a name="ROUND/"></a>
<code>(<a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a>)</code>

Divide two integers and round the quotient.


***

#### <code>CEILING</code> <sup><sub>[FUNCTION]</sub></sup><a name="CEILING"></a>
<code>∀ :A. <a href="#QUANTIZABLE">QUANTIZABLE</a> :A => (:A → <a href="#INTEGER">INTEGER</a>)</code>

Return the least integer greater than or equal to X.


***

#### <code>DOUBLE/</code> <sup><sub>[FUNCTION]</sub></sup><a name="DOUBLE/"></a>
<code>(<a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a> → <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a> → <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a>)</code>

Compute the quotient of single-precision floats A and B as a single-precision float.


***

#### <code>SINGLE/</code> <sup><sub>[FUNCTION]</sub></sup><a name="SINGLE/"></a>
<code>(<a href="#SINGLE-FLOAT">SINGLE-FLOAT</a> → <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a> → <a href="#SINGLE-FLOAT">SINGLE-FLOAT</a>)</code>

Compute the quotient of single-precision floats A and B as a single-precision float.


***

#### <code>CEILING/</code> <sup><sub>[FUNCTION]</sub></sup><a name="CEILING/"></a>
<code>(<a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a>)</code>

Divide two integers and compute the ceiling of the quotient.


***

#### <code>INEXACT/</code> <sup><sub>[FUNCTION]</sub></sup><a name="INEXACT/"></a>
<code>(<a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a> → <a href="#DOUBLE-FLOAT">DOUBLE-FLOAT</a>)</code>

Compute the quotient of integers A and B as a double-precision float.

Note: This does *not* divide double-float arguments.


***

## File: [cell.lisp](../src/library/cell.lisp)

### Types

#### <code>CELL :A</code> <sup><sub>[TYPE]</sub></sup><a name="CELL"></a>
- <code>(CELL <a href="#LISP-OBJECT">LISP-OBJECT</a>)</code>

Internally mutable cell

Constructors:
- <code>CELL :: (<a href="#LISP-OBJECT">LISP-OBJECT</a> → (<a href="#CELL">CELL</a> :A))</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> :B => <a href="#EQ">EQ</a> (<a href="#CELL">CELL</a> :B)</code>
- <code><a href="#NUM">NUM</a> :B => <a href="#NUM">NUM</a> (<a href="#CELL">CELL</a> :B)</code>
- <code><a href="#INTO">INTO</a> (<a href="#CELL">CELL</a> :B) :B</code>
- <code><a href="#INTO">INTO</a> :B (<a href="#CELL">CELL</a> :B)</code>
- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#CELL">CELL</a></code>
- <code><a href="#SEMIGROUP">SEMIGROUP</a> :B => <a href="#SEMIGROUP">SEMIGROUP</a> (<a href="#CELL">CELL</a> :B)</code>
- <code><a href="#APPLICATIVE">APPLICATIVE</a> <a href="#CELL">CELL</a></code>

</details>


***

### Functions

#### <code>CELL-READ</code> <sup><sub>[FUNCTION]</sub></sup><a name="CELL-READ"></a>
<code>∀ :A. ((<a href="#CELL">CELL</a> :A) → :A)</code>

Read the value of a mutable cell


***

#### <code>CELL-SWAP</code> <sup><sub>[FUNCTION]</sub></sup><a name="CELL-SWAP"></a>
<code>∀ :A. (:A → (<a href="#CELL">CELL</a> :A) → :A)</code>

Replace the value of a mutable cell with a new value, then return the old value


***

#### <code>MAKE-CELL</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-CELL"></a>
<code>∀ :A. (:A → (<a href="#CELL">CELL</a> :A))</code>

Create a new mutable cell


***

#### <code>CELL-WRITE</code> <sup><sub>[FUNCTION]</sub></sup><a name="CELL-WRITE"></a>
<code>∀ :A. (:A → (<a href="#CELL">CELL</a> :A) → <a href="#UNIT">UNIT</a>)</code>

Set the value of a mutable cell


***

#### <code>CELL-UPDATE</code> <sup><sub>[FUNCTION]</sub></sup><a name="CELL-UPDATE"></a>
<code>∀ :A. ((:A → :A) → (<a href="#CELL">CELL</a> :A) → <a href="#UNIT">UNIT</a>)</code>

***

## File: [vector.lisp](../src/library/vector.lisp)

### Types

#### <code>VECTOR :A</code> <sup><sub>[TYPE]</sub></sup><a name="VECTOR"></a>
- <code>(VECTOR <a href="#LISP-OBJECT">LISP-OBJECT</a>)</code>

Constructors:
- <code>VECTOR :: (<a href="#LISP-OBJECT">LISP-OBJECT</a> → (<a href="#VECTOR">VECTOR</a> :A))</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> :B => <a href="#EQ">EQ</a> (<a href="#VECTOR">VECTOR</a> :B)</code>
- <code><a href="#ISO">ISO</a> (<a href="#SLICE">SLICE</a> :B) (<a href="#VECTOR">VECTOR</a> :B)</code>
- <code><a href="#ISO">ISO</a> (<a href="#VECTOR">VECTOR</a> :B) (<a href="#LIST">LIST</a> :B)</code>
- <code><a href="#INTO">INTO</a> (<a href="#VECTOR">VECTOR</a> :B) (<a href="#SLICE">SLICE</a> :B)</code>
- <code><a href="#INTO">INTO</a> (<a href="#SLICE">SLICE</a> :B) (<a href="#VECTOR">VECTOR</a> :B)</code>
- <code><a href="#INTO">INTO</a> (<a href="#VECTOR">VECTOR</a> :B) (<a href="#LIST">LIST</a> :B)</code>
- <code><a href="#INTO">INTO</a> (<a href="#LIST">LIST</a> :B) (<a href="#VECTOR">VECTOR</a> :B)</code>
- <code><a href="#FUNCTOR">FUNCTOR</a> <a href="#VECTOR">VECTOR</a></code>
- <code><a href="#SEMIGROUP">SEMIGROUP</a> (<a href="#VECTOR">VECTOR</a> :B)</code>

</details>


***

### Functions

#### <code>VECTOR-POP</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-POP"></a>
<code>∀ :A. ((<a href="#VECTOR">VECTOR</a> :A) → (<a href="#OPTIONAL">OPTIONAL</a> :A))</code>

Remove and return the first item of V


***

#### <code>VECTOR-SET</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-SET"></a>
<code>∀ :A. (<a href="#INTEGER">INTEGER</a> → :A → (<a href="#VECTOR">VECTOR</a> :A) → <a href="#UNIT">UNIT</a>)</code>

Set the INDEXth element of V to ITEM. This function left intentionally unsafe because it does not have a return value to check.


***

#### <code>MAKE-VECTOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-VECTOR"></a>
<code>∀ :A. (<a href="#UNIT">UNIT</a> → (<a href="#VECTOR">VECTOR</a> :A))</code>

Create a new empty vector


***

#### <code>VECTOR-COPY</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-COPY"></a>
<code>∀ :A. ((<a href="#VECTOR">VECTOR</a> :A) → (<a href="#VECTOR">VECTOR</a> :A))</code>

Return a new vector containing the same elements as V


***

#### <code>VECTOR-HEAD</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-HEAD"></a>
<code>∀ :A. ((<a href="#VECTOR">VECTOR</a> :A) → (<a href="#OPTIONAL">OPTIONAL</a> :A))</code>

Return the first item of V


***

#### <code>VECTOR-LAST</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-LAST"></a>
<code>∀ :A. ((<a href="#VECTOR">VECTOR</a> :A) → (<a href="#OPTIONAL">OPTIONAL</a> :A))</code>

Return the last element of V


***

#### <code>VECTOR-PUSH</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-PUSH"></a>
<code>∀ :A. (:A → (<a href="#VECTOR">VECTOR</a> :A) → <a href="#INTEGER">INTEGER</a>)</code>

Append ITEM to V and resize V if necessary


***

#### <code>VECTOR-SORT</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-SORT"></a>
<code>∀ :A. <a href="#ORD">ORD</a> :A => ((<a href="#VECTOR">VECTOR</a> :A) → <a href="#UNIT">UNIT</a>)</code>

Sort a vector inplace


***

#### <code>VECTOR-EMPTY</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-EMPTY"></a>
<code>∀ :A. ((<a href="#VECTOR">VECTOR</a> :A) → <a href="#BOOLEAN">BOOLEAN</a>)</code>

Returns TRUE if V is empty


***

#### <code>VECTOR-INDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-INDEX"></a>
<code>∀ :A. (<a href="#INTEGER">INTEGER</a> → (<a href="#VECTOR">VECTOR</a> :A) → (<a href="#OPTIONAL">OPTIONAL</a> :A))</code>

Return the INDEXth element of V


***

#### <code>VECTOR-APPEND</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-APPEND"></a>
<code>∀ :A. ((<a href="#VECTOR">VECTOR</a> :A) → (<a href="#VECTOR">VECTOR</a> :A) → (<a href="#VECTOR">VECTOR</a> :A))</code>

Create a new VECTOR containing the elements of v1 followed by the elements of v2


***

#### <code>VECTOR-LENGTH</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-LENGTH"></a>
<code>∀ :A. ((<a href="#VECTOR">VECTOR</a> :A) → <a href="#INTEGER">INTEGER</a>)</code>

Returns the length of V


***

#### <code>VECTOR-FOREACH</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-FOREACH"></a>
<code>∀ :A :B. ((:A → :B) → (<a href="#VECTOR">VECTOR</a> :A) → <a href="#UNIT">UNIT</a>)</code>

Call the function F once for each item in V


***

#### <code>VECTOR-SORT-BY</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-SORT-BY"></a>
<code>∀ :A. ((:A → :A → <a href="#BOOLEAN">BOOLEAN</a>) → (<a href="#VECTOR">VECTOR</a> :A) → <a href="#UNIT">UNIT</a>)</code>

Sort a vector with predicate function F


***

#### <code>VECTOR-TO-LIST</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-TO-LIST"></a>
<code>∀ :A. ((<a href="#VECTOR">VECTOR</a> :A) → (<a href="#LIST">LIST</a> :A))</code>

***

#### <code>VECTOR-CAPACITY</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-CAPACITY"></a>
<code>∀ :A. ((<a href="#VECTOR">VECTOR</a> :A) → <a href="#INTEGER">INTEGER</a>)</code>

Returns the number of elements that V can store without resizing


***

#### <code>VECTOR-FOREACH2</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-FOREACH2"></a>
<code>∀ :A :B :C. ((:A → :B → :C) → (<a href="#VECTOR">VECTOR</a> :A) → (<a href="#VECTOR">VECTOR</a> :B) → <a href="#UNIT">UNIT</a>)</code>

Like vector-foreach but twice as good


***

#### <code>VECTOR-POP-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-POP-UNSAFE"></a>
<code>∀ :A. ((<a href="#VECTOR">VECTOR</a> :A) → :A)</code>

Remove and return the first item of V without checking if the vector is empty


***

#### <code>VECTOR-HEAD-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-HEAD-UNSAFE"></a>
<code>∀ :A. ((<a href="#VECTOR">VECTOR</a> :A) → :A)</code>

Return the first item of V without first checking if V is empty


***

#### <code>VECTOR-LAST-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-LAST-UNSAFE"></a>
<code>∀ :A. ((<a href="#VECTOR">VECTOR</a> :A) → :A)</code>

Return the last element of V without first checking if V is empty


***

#### <code>VECTOR-SWAP-REMOVE</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-SWAP-REMOVE"></a>
<code>∀ :A. (<a href="#INTEGER">INTEGER</a> → (<a href="#VECTOR">VECTOR</a> :A) → (<a href="#OPTIONAL">OPTIONAL</a> :A))</code>

Remove the element IDX from VEC and replace it with the last element in VEC. Then return the removed element.


***

#### <code>VECTOR-INDEX-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-INDEX-UNSAFE"></a>
<code>∀ :A. (<a href="#INTEGER">INTEGER</a> → (<a href="#VECTOR">VECTOR</a> :A) → :A)</code>

Return the INDEXth element of V without checking if the element exists


***

#### <code>MAKE-VECTOR-CAPACITY</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-VECTOR-CAPACITY"></a>
<code>∀ :A. (<a href="#INTEGER">INTEGER</a> → (<a href="#VECTOR">VECTOR</a> :A))</code>

Create a new vector with N elements preallocated


***

#### <code>VECTOR-FOREACH-INDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-FOREACH-INDEX"></a>
<code>∀ :A :B. ((<a href="#INTEGER">INTEGER</a> → :A → :B) → (<a href="#VECTOR">VECTOR</a> :A) → <a href="#UNIT">UNIT</a>)</code>

Call the function F once for each item in V with its index


***

#### <code>VECTOR-SWAP-REMOVE-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-SWAP-REMOVE-UNSAFE"></a>
<code>∀ :A. (<a href="#INTEGER">INTEGER</a> → (<a href="#VECTOR">VECTOR</a> :A) → :A)</code>

Remove the element IDX from VEC and replace it with the last element in VEC without bounds checking. Then return the removed element.


***

## File: [slice.lisp](../src/library/slice.lisp)

### Types

#### <code>SLICE :A</code> <sup><sub>[TYPE]</sub></sup><a name="SLICE"></a>
- <code>(SLICE <a href="#LISP-OBJECT">LISP-OBJECT</a>)</code>

Constructors:
- <code>SLICE :: (<a href="#LISP-OBJECT">LISP-OBJECT</a> → (<a href="#SLICE">SLICE</a> :A))</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> :B => <a href="#EQ">EQ</a> (<a href="#SLICE">SLICE</a> :B)</code>
- <code><a href="#ISO">ISO</a> (<a href="#SLICE">SLICE</a> :B) (<a href="#VECTOR">VECTOR</a> :B)</code>
- <code><a href="#INTO">INTO</a> (<a href="#VECTOR">VECTOR</a> :B) (<a href="#SLICE">SLICE</a> :B)</code>
- <code><a href="#INTO">INTO</a> (<a href="#SLICE">SLICE</a> :B) (<a href="#VECTOR">VECTOR</a> :B)</code>

</details>


***

### Functions

#### <code>SLICE-SET</code> <sup><sub>[FUNCTION]</sub></sup><a name="SLICE-SET"></a>
<code>∀ :A. (<a href="#INTEGER">INTEGER</a> → :A → (<a href="#SLICE">SLICE</a> :A) → <a href="#UNIT">UNIT</a>)</code>

Set the element at INDEX in S to ITEM


***

#### <code>MAKE-SLICE</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-SLICE"></a>
<code>∀ :A. (<a href="#INTEGER">INTEGER</a> → <a href="#INTEGER">INTEGER</a> → (<a href="#VECTOR">VECTOR</a> :A) → (<a href="#SLICE">SLICE</a> :A))</code>

***

#### <code>SLICE-COPY</code> <sup><sub>[FUNCTION]</sub></sup><a name="SLICE-COPY"></a>
<code>∀ :A. ((<a href="#SLICE">SLICE</a> :A) → (<a href="#SLICE">SLICE</a> :A))</code>

Returns a new slice containg the same elements as S


***

#### <code>SLICE-INDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="SLICE-INDEX"></a>
<code>∀ :A. (<a href="#INTEGER">INTEGER</a> → (<a href="#SLICE">SLICE</a> :A) → (<a href="#OPTIONAL">OPTIONAL</a> :A))</code>

Lookup the element at INDEX in S


***

#### <code>SLICE-LENGTH</code> <sup><sub>[FUNCTION]</sub></sup><a name="SLICE-LENGTH"></a>
<code>∀ :A. ((<a href="#SLICE">SLICE</a> :A) → <a href="#INTEGER">INTEGER</a>)</code>

Returns the length of S


***

#### <code>SLICE-FOREACH</code> <sup><sub>[FUNCTION]</sub></sup><a name="SLICE-FOREACH"></a>
<code>∀ :A :B. ((:A → :B) → (<a href="#SLICE">SLICE</a> :A) → <a href="#UNIT">UNIT</a>)</code>

Call the function F once for each item in S


***

#### <code>SLICE-FOREACH2</code> <sup><sub>[FUNCTION]</sub></sup><a name="SLICE-FOREACH2"></a>
<code>∀ :A :B :C. ((:A → :B → :C) → (<a href="#SLICE">SLICE</a> :A) → (<a href="#SLICE">SLICE</a> :B) → <a href="#UNIT">UNIT</a>)</code>

Iterate over S1 and S2 calling F once on each iteration


***

#### <code>VECTOR-CHUNKED</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-CHUNKED"></a>
<code>∀ :A :B. (((<a href="#SLICE">SLICE</a> :A) → :B) → <a href="#INTEGER">INTEGER</a> → (<a href="#VECTOR">VECTOR</a> :A) → <a href="#UNIT">UNIT</a>)</code>

Chunked iteration over a vector. Ignores elements at the end if the vector does not evenly divide by the chunk size.


***

#### <code>VECTOR-SLIDING</code> <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-SLIDING"></a>
<code>∀ :A :B. (((<a href="#SLICE">SLICE</a> :A) → :B) → <a href="#INTEGER">INTEGER</a> → (<a href="#VECTOR">VECTOR</a> :A) → <a href="#UNIT">UNIT</a>)</code>

Sliding iteration over a vector


***

#### <code>SLICE-INDEX-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="SLICE-INDEX-UNSAFE"></a>
<code>∀ :A. (<a href="#INTEGER">INTEGER</a> → (<a href="#SLICE">SLICE</a> :A) → :A)</code>

Lookup the element at INDEX in S without bounds checking


***

#### <code>SLICE-FOREACH-INDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="SLICE-FOREACH-INDEX"></a>
<code>∀ :A :B. ((<a href="#INTEGER">INTEGER</a> → :A → :B) → (<a href="#SLICE">SLICE</a> :A) → <a href="#UNIT">UNIT</a>)</code>

Call the function F once for each item in S with its index


***

## File: [hashtable.lisp](../src/library/hashtable.lisp)

### Types

#### <code>HASHTABLE :A :B</code> <sup><sub>[TYPE]</sub></sup><a name="HASHTABLE"></a>
- <code>(HASHTABLE <a href="#LISP-OBJECT">LISP-OBJECT</a>)</code>

Constructors:
- <code>HASHTABLE :: (<a href="#LISP-OBJECT">LISP-OBJECT</a> → (<a href="#HASHTABLE">HASHTABLE</a> :A :B))</code>


***

### Functions

#### <code>HASHTABLE-GET</code> <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-GET"></a>
<code>∀ :A :B. (:A → (<a href="#HASHTABLE">HASHTABLE</a> :A :B) → (<a href="#OPTIONAL">OPTIONAL</a> :B))</code>

Lookup KEY in TABLE


***

#### <code>HASHTABLE-SET</code> <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-SET"></a>
<code>∀ :A :B. (:A → :B → (<a href="#HASHTABLE">HASHTABLE</a> :A :B) → <a href="#UNIT">UNIT</a>)</code>

Set KEY to VALUE in TABLE


***

#### <code>HASHTABLE-KEYS</code> <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-KEYS"></a>
<code>∀ :A :B. ((<a href="#HASHTABLE">HASHTABLE</a> :A :B) → (<a href="#VECTOR">VECTOR</a> :A))</code>

Returns the keys in TABLE as a vector


***

#### <code>MAKE-HASHTABLE</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-HASHTABLE"></a>
<code>∀ :A :B. (<a href="#UNIT">UNIT</a> → (<a href="#HASHTABLE">HASHTABLE</a> :A :B))</code>

Create a new empty hashtable


***

#### <code>HASHTABLE-COUNT</code> <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-COUNT"></a>
<code>∀ :A :B. ((<a href="#HASHTABLE">HASHTABLE</a> :A :B) → <a href="#INTEGER">INTEGER</a>)</code>

Returns the number of entries in TABLE


***

#### <code>HASHTABLE-REMOVE</code> <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-REMOVE"></a>
<code>∀ :A :B. (:A → (<a href="#HASHTABLE">HASHTABLE</a> :A :B) → <a href="#UNIT">UNIT</a>)</code>

Remove the entry at KEY from TABLE


***

#### <code>HASHTABLE-VALUES</code> <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-VALUES"></a>
<code>∀ :A :B. ((<a href="#HASHTABLE">HASHTABLE</a> :A :B) → (<a href="#VECTOR">VECTOR</a> :B))</code>

Returns the values in TABLE as a vector


***

#### <code>HASHTABLE-ENTRIES</code> <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-ENTRIES"></a>
<code>∀ :A :B. ((<a href="#HASHTABLE">HASHTABLE</a> :A :B) → (<a href="#VECTOR">VECTOR</a> (<a href="#TUPLE">TUPLE</a> :A :B)))</code>

Returns the keys and values in TABLE as a vector


***

#### <code>HASHTABLE-FOREACH</code> <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-FOREACH"></a>
<code>∀ :A :B :C. ((:A → :B → :C) → (<a href="#HASHTABLE">HASHTABLE</a> :A :B) → <a href="#UNIT">UNIT</a>)</code>

Call F once for each key value pair in TABLE


***

#### <code>MAKE-HASHTABLE-CAPACITY</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-HASHTABLE-CAPACITY"></a>
<code>∀ :A :B. (<a href="#INTEGER">INTEGER</a> → (<a href="#HASHTABLE">HASHTABLE</a> :A :B))</code>

Crate a new empty hashtable with a given capacity


***

## File: [graph.lisp](../src/library/graph.lisp)

### Types

#### <code>GRAPH :A :B</code> <sup><sub>[TYPE]</sub></sup><a name="GRAPH"></a>
- <code>(GRAPH <a href="#GRAPHTYPE">GRAPHTYPE</a> (<a href="#VECTOR">VECTOR</a> (<a href="#NODE">NODE</a> :A)) (<a href="#VECTOR">VECTOR</a> (<a href="#EDGE">EDGE</a> :B)))</code>

A graph using adjacency list representation

Constructors:
- <code>GRAPH :: (<a href="#GRAPHTYPE">GRAPHTYPE</a> → (<a href="#VECTOR">VECTOR</a> (<a href="#NODE">NODE</a> :A)) → (<a href="#VECTOR">VECTOR</a> (<a href="#EDGE">EDGE</a> :B)) → (<a href="#GRAPH">GRAPH</a> :A :B))</code>


***

#### <code>EDGEINDEX</code> <sup><sub>[TYPE]</sub></sup><a name="EDGEINDEX"></a>
- <code>(EDGEINDEX <a href="#INTEGER">INTEGER</a>)</code>

Constructors:
- <code>EDGEINDEX :: (<a href="#INTEGER">INTEGER</a> → <a href="#EDGEINDEX">EDGEINDEX</a>)</code>

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
- <code>UNDIRECTED :: <a href="#GRAPHTYPE">GRAPHTYPE</a></code>
- <code>DIRECTED :: <a href="#GRAPHTYPE">GRAPHTYPE</a></code>


***

#### <code>NODEINDEX</code> <sup><sub>[TYPE]</sub></sup><a name="NODEINDEX"></a>
- <code>(NODEINDEX <a href="#INTEGER">INTEGER</a>)</code>

Constructors:
- <code>NODEINDEX :: (<a href="#INTEGER">INTEGER</a> → <a href="#NODEINDEX">NODEINDEX</a>)</code>

<details>
<summary>Instances</summary>

- <code><a href="#EQ">EQ</a> <a href="#NODEINDEX">NODEINDEX</a></code>
- <code><a href="#INTO">INTO</a> <a href="#NODEINDEX">NODEINDEX</a> <a href="#INTEGER">INTEGER</a></code>

</details>


***

### Functions

#### <code>MAKE-GRAPH</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-GRAPH"></a>
<code>∀ :A :B. (<a href="#UNIT">UNIT</a> → (<a href="#GRAPH">GRAPH</a> :A :B))</code>

Create a new empty undirected graph


***

#### <code>GRAPH-EDGES</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-EDGES"></a>
<code>∀ :A :B. ((<a href="#GRAPH">GRAPH</a> :A :B) → (<a href="#VECTOR">VECTOR</a> (<a href="#EDGE">EDGE</a> :B)))</code>

Returns the edges in a graph


***

#### <code>GRAPH-NODES</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-NODES"></a>
<code>∀ :A :B. ((<a href="#GRAPH">GRAPH</a> :A :B) → (<a href="#VECTOR">VECTOR</a> (<a href="#NODE">NODE</a> :A)))</code>

Returns the nodes in a graph


***

#### <code>MAKE-DIGRAPH</code> <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-DIGRAPH"></a>
<code>∀ :A :B. (<a href="#UNIT">UNIT</a> → (<a href="#GRAPH">GRAPH</a> :A :B))</code>

Create a new directed graph


***

#### <code>GRAPH-ADD-EDGE</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-ADD-EDGE"></a>
<code>∀ :A :B. (:A → <a href="#NODEINDEX">NODEINDEX</a> → <a href="#NODEINDEX">NODEINDEX</a> → (<a href="#GRAPH">GRAPH</a> :B :A) → <a href="#EDGEINDEX">EDGEINDEX</a>)</code>

Add an edge with associated data from node FROM to node TO in the graph.


***

#### <code>GRAPH-ADD-NODE</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-ADD-NODE"></a>
<code>∀ :A :B. (:A → (<a href="#GRAPH">GRAPH</a> :A :B) → <a href="#NODEINDEX">NODEINDEX</a>)</code>

Add a node with associated data to the graph, returning the index of the new node.


***

#### <code>GRAPH-EDGE-COUNT</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-EDGE-COUNT"></a>
<code>∀ :A :B. ((<a href="#GRAPH">GRAPH</a> :A :B) → <a href="#INTEGER">INTEGER</a>)</code>

Returns the number of edges in a graph


***

#### <code>GRAPH-LOOKUP-EDGE</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-LOOKUP-EDGE"></a>
<code>∀ :A :B. (<a href="#EDGEINDEX">EDGEINDEX</a> → (<a href="#GRAPH">GRAPH</a> :A :B) → (<a href="#OPTIONAL">OPTIONAL</a> (<a href="#EDGE">EDGE</a> :B)))</code>

Lookup a node with index IDX in graph G


***

#### <code>GRAPH-LOOKUP-NODE</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-LOOKUP-NODE"></a>
<code>∀ :A :B. (<a href="#NODEINDEX">NODEINDEX</a> → (<a href="#GRAPH">GRAPH</a> :A :B) → (<a href="#OPTIONAL">OPTIONAL</a> (<a href="#NODE">NODE</a> :A)))</code>

Lookup a node with index IDX in graph G


***

#### <code>GRAPH-REMOVE-EDGE</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-REMOVE-EDGE"></a>
<code>∀ :A :B. (<a href="#EDGEINDEX">EDGEINDEX</a> → (<a href="#GRAPH">GRAPH</a> :A :B) → (<a href="#OPTIONAL">OPTIONAL</a> :B))</code>

Remove an edge from GRAPH


***

#### <code>GRAPH-REMOVE-NODE</code> <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-REMOVE-NODE"></a>
<code>∀ :A :B. (<a href="#NODEINDEX">NODEINDEX</a> → (<a href="#GRAPH">GRAPH</a> :A :B) → (<a href="#OPTIONAL">OPTIONAL</a> :A))</code>

Remove a node and all edges connecting to it from GRAPH


***

