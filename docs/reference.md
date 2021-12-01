# Reference for COALTON-LIBRARY

## File: [types.lisp](../src/library/types.lisp)

### Types

#### `LIST :A` <sup><sub>[TYPE]</sub></sup><a name="LIST"></a>
- `(CONS :A (LIST :A))`
- `NIL`

A list in singly-linked representation.

Constructors:
- `CONS :: (:A → (LIST :A) → (LIST :A))`
- `NIL :: (LIST :A)`

<details>
<summary>Instances</summary>

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

</details>

***

#### `TUPLE :A :B` <sup><sub>[TYPE]</sub></sup><a name="TUPLE"></a>
- `(TUPLE :A :B)`

A heterogeneous collection of items.

Constructors:
- `TUPLE :: (:A → :B → (TUPLE :A :B))`

<details>
<summary>Instances</summary>

- [`EQ :A`](#EQ) [`EQ :B`](#EQ) `=>` [`EQ`](#EQ) [`(TUPLE :A :B)`](#TUPLE)
- [`ISO`](#ISO) [`(TUPLE :A :B)`](#TUPLE) [`(TUPLE :B :A)`](#TUPLE)
- [`ORD :A`](#ORD) [`ORD :B`](#ORD) `=>` [`ORD`](#ORD) [`(TUPLE :A :B)`](#TUPLE)
- [`INTO`](#INTO) [`(TUPLE :A :B)`](#TUPLE) [`(TUPLE :B :A)`](#TUPLE)

</details>

***

#### `RESULT :A :B` <sup><sub>[TYPE]</sub></sup><a name="RESULT"></a>
- `(ERR :A)`
- `(OK :B)`

Represents something that may have failed.

Constructors:
- `ERR :: (:A → (RESULT :A :B))`
- `OK :: (:B → (RESULT :A :B))`

<details>
<summary>Instances</summary>

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

</details>

***

#### `TUPLE3 :A :B :C` <sup><sub>[TYPE]</sub></sup><a name="TUPLE3"></a>
- `(TUPLE3 :A :B :C)`

Constructors:
- `TUPLE3 :: (:A → :B → :C → (TUPLE3 :A :B :C))`

<details>
<summary>Instances</summary>

- [`EQ :A`](#EQ) [`EQ :B`](#EQ) [`EQ :C`](#EQ) `=>` [`EQ`](#EQ) [`(TUPLE3 :A :B :C)`](#TUPLE3)

</details>

***

#### `TUPLE4 :A :B :C :D` <sup><sub>[TYPE]</sub></sup><a name="TUPLE4"></a>
- `(TUPLE4 :A :B :C :D)`

Constructors:
- `TUPLE4 :: (:A → :B → :C → :D → (TUPLE4 :A :B :C :D))`

<details>
<summary>Instances</summary>

- [`EQ :A`](#EQ) [`EQ :B`](#EQ) [`EQ :C`](#EQ) [`EQ :D`](#EQ) `=>` [`EQ`](#EQ) [`(TUPLE4 :A :B :C :D)`](#TUPLE4)

</details>

***

#### `TUPLE5 :A :B :C :D :E` <sup><sub>[TYPE]</sub></sup><a name="TUPLE5"></a>
- `(TUPLE5 :A :B :C :D :E)`

Constructors:
- `TUPLE5 :: (:A → :B → :C → :D → :E → (TUPLE5 :A :B :C :D :E))`

<details>
<summary>Instances</summary>

- [`EQ :A`](#EQ) [`EQ :B`](#EQ) [`EQ :C`](#EQ) [`EQ :D`](#EQ) [`EQ :E`](#EQ) `=>` [`EQ`](#EQ) [`(TUPLE5 :A :B :C :D :E)`](#TUPLE5)

</details>

***

#### `FRACTION` <sup><sub>[TYPE]</sub></sup><a name="FRACTION"></a>
- `(%FRACTION INTEGER INTEGER)`

A ratio of integers always in reduced form.

Constructors:
- `%FRACTION :: (INTEGER → INTEGER → FRACTION)`

<details>
<summary>Instances</summary>

- [`EQ`](#EQ) [`FRACTION`](#FRACTION)
- [`NUM`](#NUM) [`FRACTION`](#FRACTION)
- [`ORD`](#ORD) [`FRACTION`](#FRACTION)
- [`SHOW`](#SHOW) [`FRACTION`](#FRACTION)
- [`DIVIDABLE`](#DIVIDABLE) [`INTEGER`](#INTEGER) [`FRACTION`](#FRACTION)

</details>

***

#### `OPTIONAL :A` <sup><sub>[TYPE]</sub></sup><a name="OPTIONAL"></a>
- `(SOME :A)`
- `NONE`

Represents something that may not have a value.

Constructors:
- `SOME :: (:A → (OPTIONAL :A))`
- `NONE :: (OPTIONAL :A)`

<details>
<summary>Instances</summary>

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

</details>

***

## File: [classes.lisp](../src/library/classes.lisp)

### Types

#### `ORD` <sup><sub>[TYPE]</sub></sup><a name="ORD"></a>
- `LT`
- `GT`
- `EQ`

Constructors:
- `LT :: ORD`
- `GT :: ORD`
- `EQ :: ORD`

***

### Classes

#### `EQ` <sup><sub>[CLASS]</sub></sup><a name="EQ"></a>
[`EQ`](#EQ) [`:A`](#:A)

Types which have equality defined.

Methods:
- `== :: (:A → :A → BOOLEAN)`
- `/= :: (:A → :A → BOOLEAN)`

<details>
<summary>Instances</summary>

- [`EQ`](#EQ) [`NODEINDEX`](#NODEINDEX)
- [`EQ`](#EQ) [`EDGEINDEX`](#EDGEINDEX)
- [`EQ :A`](#EQ) `=>` [`EQ`](#EQ) [`(VECTOR :A)`](#VECTOR)
- [`EQ :A`](#EQ) `=>` [`EQ`](#EQ) [`(CELL :A)`](#CELL)
- [`EQ :A`](#EQ) [`EQ :B`](#EQ) `=>` [`EQ`](#EQ) [`(RESULT :A :B)`](#RESULT)
- [`EQ :A`](#EQ) [`EQ :B`](#EQ) [`EQ :C`](#EQ) [`EQ :D`](#EQ) [`EQ :E`](#EQ) `=>` [`EQ`](#EQ) [`(TUPLE5 :A :B :C :D :E)`](#TUPLE5)
- [`EQ :A`](#EQ) [`EQ :B`](#EQ) [`EQ :C`](#EQ) [`EQ :D`](#EQ) `=>` [`EQ`](#EQ) [`(TUPLE4 :A :B :C :D)`](#TUPLE4)
- [`EQ :A`](#EQ) [`EQ :B`](#EQ) [`EQ :C`](#EQ) `=>` [`EQ`](#EQ) [`(TUPLE3 :A :B :C)`](#TUPLE3)
- [`EQ :A`](#EQ) [`EQ :B`](#EQ) `=>` [`EQ`](#EQ) [`(TUPLE :A :B)`](#TUPLE)
- [`EQ :A`](#EQ) `=>` [`EQ`](#EQ) [`(LIST :A)`](#LIST)
- [`EQ :A`](#EQ) `=>` [`EQ`](#EQ) [`(OPTIONAL :A)`](#OPTIONAL)
- [`EQ`](#EQ) [`STRING`](#STRING)
- [`EQ`](#EQ) [`CHAR`](#CHAR)
- [`EQ`](#EQ) [`FRACTION`](#FRACTION)
- [`EQ`](#EQ) [`DOUBLE-FLOAT`](#DOUBLE-FLOAT)
- [`EQ`](#EQ) [`SINGLE-FLOAT`](#SINGLE-FLOAT)
- [`EQ`](#EQ) [`INTEGER`](#INTEGER)
- [`EQ`](#EQ) [`U64`](#U64)
- [`EQ`](#EQ) [`U32`](#U32)
- [`EQ`](#EQ) [`U8`](#U8)
- [`EQ`](#EQ) [`I64`](#I64)
- [`EQ`](#EQ) [`I32`](#I32)
- [`EQ`](#EQ) [`BOOLEAN`](#BOOLEAN)

</details>


***

#### `NUM` <sup><sub>[CLASS]</sub></sup><a name="NUM"></a>
[`EQ :A`](#EQ) [`SHOW :A`](#SHOW) `=>` [`NUM`](#NUM) [`:A`](#:A)

Types which have numeric operations defined.

Methods:
- `+ :: (:A → :A → :A)`
- `- :: (:A → :A → :A)`
- `* :: (:A → :A → :A)`
- `FROMINT :: (INTEGER → :A)`

<details>
<summary>Instances</summary>

- [`NUM :A`](#NUM) `=>` [`NUM`](#NUM) [`(CELL :A)`](#CELL)
- [`NUM`](#NUM) [`FRACTION`](#FRACTION)
- [`NUM`](#NUM) [`DOUBLE-FLOAT`](#DOUBLE-FLOAT)
- [`NUM`](#NUM) [`SINGLE-FLOAT`](#SINGLE-FLOAT)
- [`NUM`](#NUM) [`INTEGER`](#INTEGER)
- [`NUM`](#NUM) [`U64`](#U64)
- [`NUM`](#NUM) [`U32`](#U32)
- [`NUM`](#NUM) [`U8`](#U8)
- [`NUM`](#NUM) [`I64`](#I64)
- [`NUM`](#NUM) [`I32`](#I32)

</details>


***

#### `ORD` <sup><sub>[CLASS]</sub></sup><a name="ORD"></a>
[`EQ :A`](#EQ) `=>` [`ORD`](#ORD) [`:A`](#:A)

Types whose values can be ordered.

Methods:
- `<=> :: (:A → :A → ORD)`

<details>
<summary>Instances</summary>

- [`ORD :A`](#ORD) [`ORD :B`](#ORD) `=>` [`ORD`](#ORD) [`(RESULT :A :B)`](#RESULT)
- [`ORD :A`](#ORD) [`ORD :B`](#ORD) `=>` [`ORD`](#ORD) [`(TUPLE :A :B)`](#TUPLE)
- [`ORD :A`](#ORD) `=>` [`ORD`](#ORD) [`(OPTIONAL :A)`](#OPTIONAL)
- [`ORD`](#ORD) [`STRING`](#STRING)
- [`ORD`](#ORD) [`CHAR`](#CHAR)
- [`ORD`](#ORD) [`FRACTION`](#FRACTION)
- [`ORD`](#ORD) [`DOUBLE-FLOAT`](#DOUBLE-FLOAT)
- [`ORD`](#ORD) [`SINGLE-FLOAT`](#SINGLE-FLOAT)
- [`ORD`](#ORD) [`INTEGER`](#INTEGER)
- [`ORD`](#ORD) [`U64`](#U64)
- [`ORD`](#ORD) [`U32`](#U32)
- [`ORD`](#ORD) [`U8`](#U8)
- [`ORD`](#ORD) [`I64`](#I64)
- [`ORD`](#ORD) [`I32`](#I32)
- [`ORD`](#ORD) [`BOOLEAN`](#BOOLEAN)

</details>


***

#### `INTO` <sup><sub>[CLASS]</sub></sup><a name="INTO"></a>
[`INTO`](#INTO) [`:A`](#:A) [`:B`](#:B)

INTO imples *every* element of :FROM can be represented by an element of :TO. This conversion might not be injective (i.e., there may be elements in :TO that don't correspond to any in :FROM).

Methods:
- `INTO :: (:A → :B)`

<details>
<summary>Instances</summary>

- [`INTO`](#INTO) [`NODEINDEX`](#NODEINDEX) [`INTEGER`](#INTEGER)
- [`INTO`](#INTO) [`EDGEINDEX`](#EDGEINDEX) [`INTEGER`](#INTEGER)
- [`INTO`](#INTO) [`(VECTOR :A)`](#VECTOR) [`(LIST :A)`](#LIST)
- [`INTO`](#INTO) [`(LIST :A)`](#LIST) [`(VECTOR :A)`](#VECTOR)
- [`INTO`](#INTO) [`(CELL :A)`](#CELL) [`:A`](#:A)
- [`INTO`](#INTO) [`:A`](#:A) [`(CELL :A)`](#CELL)
- [`INTO`](#INTO) [`(OPTIONAL :A)`](#OPTIONAL) [`(RESULT UNIT :A)`](#RESULT)
- [`INTO`](#INTO) [`(RESULT :A :B)`](#RESULT) [`(OPTIONAL :B)`](#OPTIONAL)
- [`INTO`](#INTO) [`(TUPLE :A :B)`](#TUPLE) [`(TUPLE :B :A)`](#TUPLE)
- [`INTO`](#INTO) [`(LIST CHAR)`](#LIST) [`STRING`](#STRING)
- [`INTO`](#INTO) [`STRING`](#STRING) [`(LIST CHAR)`](#LIST)
- [`INTO`](#INTO) [`INTEGER`](#INTEGER) [`STRING`](#STRING)
- [`INTO`](#INTO) [`U64`](#U64) [`DOUBLE-FLOAT`](#DOUBLE-FLOAT)
- [`INTO`](#INTO) [`U64`](#U64) [`SINGLE-FLOAT`](#SINGLE-FLOAT)
- [`INTO`](#INTO) [`U64`](#U64) [`INTEGER`](#INTEGER)
- [`INTO`](#INTO) [`INTEGER`](#INTEGER) [`U64`](#U64)
- [`INTO`](#INTO) [`U32`](#U32) [`DOUBLE-FLOAT`](#DOUBLE-FLOAT)
- [`INTO`](#INTO) [`U32`](#U32) [`SINGLE-FLOAT`](#SINGLE-FLOAT)
- [`INTO`](#INTO) [`U32`](#U32) [`INTEGER`](#INTEGER)
- [`INTO`](#INTO) [`INTEGER`](#INTEGER) [`U32`](#U32)
- [`INTO`](#INTO) [`U8`](#U8) [`DOUBLE-FLOAT`](#DOUBLE-FLOAT)
- [`INTO`](#INTO) [`U8`](#U8) [`SINGLE-FLOAT`](#SINGLE-FLOAT)
- [`INTO`](#INTO) [`U8`](#U8) [`INTEGER`](#INTEGER)
- [`INTO`](#INTO) [`INTEGER`](#INTEGER) [`U8`](#U8)
- [`INTO`](#INTO) [`I64`](#I64) [`INTEGER`](#INTEGER)
- [`INTO`](#INTO) [`INTEGER`](#INTEGER) [`I64`](#I64)
- [`INTO`](#INTO) [`I32`](#I32) [`INTEGER`](#INTEGER)
- [`INTO`](#INTO) [`INTEGER`](#INTEGER) [`I32`](#I32)
- [`INTO`](#INTO) [`:A`](#:A) [`:A`](#:A)

</details>


***

#### `SHOW` <sup><sub>[CLASS]</sub></sup><a name="SHOW"></a>
[`SHOW`](#SHOW) [`:A`](#:A)

Types which can be shown in string representation.

Methods:
- `SHOW :: (:A → STRING)`

<details>
<summary>Instances</summary>

- [`SHOW`](#SHOW) [`NODEINDEX`](#NODEINDEX)
- [`SHOW`](#SHOW) [`EDGEINDEX`](#EDGEINDEX)
- [`SHOW :A`](#SHOW) `=>` [`SHOW`](#SHOW) [`(CELL :A)`](#CELL)
- [`SHOW :A`](#SHOW) `=>` [`SHOW`](#SHOW) [`(OPTIONAL :A)`](#OPTIONAL)
- [`SHOW`](#SHOW) [`STRING`](#STRING)
- [`SHOW`](#SHOW) [`CHAR`](#CHAR)
- [`SHOW`](#SHOW) [`DOUBLE-FLOAT`](#DOUBLE-FLOAT)
- [`SHOW`](#SHOW) [`SINGLE-FLOAT`](#SINGLE-FLOAT)
- [`SHOW`](#SHOW) [`INTEGER`](#INTEGER)
- [`SHOW`](#SHOW) [`U64`](#U64)
- [`SHOW`](#SHOW) [`U32`](#U32)
- [`SHOW`](#SHOW) [`U8`](#U8)
- [`SHOW`](#SHOW) [`I64`](#I64)
- [`SHOW`](#SHOW) [`I32`](#I32)
- [`SHOW`](#SHOW) [`FRACTION`](#FRACTION)
- [`SHOW`](#SHOW) [`BOOLEAN`](#BOOLEAN)

</details>


***

#### `MONAD` <sup><sub>[CLASS]</sub></sup><a name="MONAD"></a>
[`APPLICATIVE :A`](#APPLICATIVE) `=>` [`MONAD`](#MONAD) [`:A`](#:A)

Types which are monads as defined in Haskell. See https://wiki.haskell.org/Monad for more information.

Methods:
- `>>= :: ∀ :B :C. ((:A :B) → (:B → (:A :C)) → (:A :C))`
- `>> :: ∀ :B :C. ((:A :B) → (:A :C) → (:A :C))`

<details>
<summary>Instances</summary>

- [`MONAD`](#MONAD) [`(RESULT :A)`](#RESULT)
- [`MONAD`](#MONAD) [`LIST`](#LIST)
- [`MONAD`](#MONAD) [`OPTIONAL`](#OPTIONAL)

</details>


***

#### `MONOID` <sup><sub>[CLASS]</sub></sup><a name="MONOID"></a>
[`SEMIGROUP :A`](#SEMIGROUP) `=>` [`MONOID`](#MONOID) [`:A`](#:A)

Types with an associative binary operation and identity defined.

Methods:
- `MEMPTY :: :A`

<details>
<summary>Instances</summary>

- [`MONOID :A`](#MONOID) `=>` [`MONOID`](#MONOID) [`(RESULT :B :A)`](#RESULT)
- [`MONOID`](#MONOID) [`(LIST :A)`](#LIST)
- [`MONOID :A`](#MONOID) `=>` [`MONOID`](#MONOID) [`(OPTIONAL :A)`](#OPTIONAL)
- [`MONOID`](#MONOID) [`STRING`](#STRING)

</details>


***

#### `FUNCTOR` <sup><sub>[CLASS]</sub></sup><a name="FUNCTOR"></a>
[`FUNCTOR`](#FUNCTOR) [`:A`](#:A)

Types which can map an inner type where the mapping adheres to the identity and composition laws.

Methods:
- `MAP :: ∀ :B :C. ((:B → :C) → (:A :B) → (:A :C))`

<details>
<summary>Instances</summary>

- [`FUNCTOR`](#FUNCTOR) [`VECTOR`](#VECTOR)
- [`FUNCTOR`](#FUNCTOR) [`CELL`](#CELL)
- [`FUNCTOR`](#FUNCTOR) [`(RESULT :A)`](#RESULT)
- [`FUNCTOR`](#FUNCTOR) [`LIST`](#LIST)
- [`FUNCTOR`](#FUNCTOR) [`OPTIONAL`](#OPTIONAL)

</details>


***

#### `TRYINTO` <sup><sub>[CLASS]</sub></sup><a name="TRYINTO"></a>
[`TRYINTO`](#TRYINTO) [`:A`](#:A) [`:B`](#:B)

TRY-INTO implies *most* elements of :FROM can be represented exactly by an element of :TO, but sometimes not. If not, an error string is returned.

Methods:
- `TRYINTO :: (:A → (RESULT STRING :B))`

<details>
<summary>Instances</summary>

- [`TRYINTO`](#TRYINTO) [`STRING`](#STRING) [`INTEGER`](#INTEGER)

</details>


***

#### `DIVIDABLE` <sup><sub>[CLASS]</sub></sup><a name="DIVIDABLE"></a>
[`NUM :A`](#NUM) [`NUM :B`](#NUM) `=>` [`DIVIDABLE`](#DIVIDABLE) [`:A`](#:A) [`:B`](#:B)

The representation of a type such that division within that type possibly results in another type. For instance,


    (Dividable Integer Fraction)


establishes that division of two `Integer`s can result in a `Fraction`, whereas


    (Dividable Single-Float Single-Float)


establishes that division of two `Single-Float`s can result in a `Single-Float`.

Note that `Dividable` does *not* establish a default result type; you must constrain the result type yourself.

See also: `/`


Methods:
- `UNSAFE-/ :: (:A → :A → :B)`

<details>
<summary>Instances</summary>

- [`DIVIDABLE`](#DIVIDABLE) [`INTEGER`](#INTEGER) [`FRACTION`](#FRACTION)
- [`DIVIDABLE`](#DIVIDABLE) [`DOUBLE-FLOAT`](#DOUBLE-FLOAT) [`DOUBLE-FLOAT`](#DOUBLE-FLOAT)
- [`DIVIDABLE`](#DIVIDABLE) [`SINGLE-FLOAT`](#SINGLE-FLOAT) [`SINGLE-FLOAT`](#SINGLE-FLOAT)

</details>


***

#### `MONADFAIL` <sup><sub>[CLASS]</sub></sup><a name="MONADFAIL"></a>
[`MONAD :A`](#MONAD) `=>` [`MONADFAIL`](#MONADFAIL) [`:A`](#:A)

Methods:
- `FAIL :: ∀ :B. (STRING → (:A :B))`

<details>
<summary>Instances</summary>

- [`MONADFAIL`](#MONADFAIL) [`OPTIONAL`](#OPTIONAL)

</details>


***

#### `SEMIGROUP` <sup><sub>[CLASS]</sub></sup><a name="SEMIGROUP"></a>
[`SEMIGROUP`](#SEMIGROUP) [`:A`](#:A)

Types with an associative binary operation defined.

Methods:
- `<> :: (:A → :A → :A)`

<details>
<summary>Instances</summary>

- [`SEMIGROUP`](#SEMIGROUP) [`(VECTOR :A)`](#VECTOR)
- [`SEMIGROUP :A`](#SEMIGROUP) `=>` [`SEMIGROUP`](#SEMIGROUP) [`(CELL :A)`](#CELL)
- [`SEMIGROUP :A`](#SEMIGROUP) `=>` [`SEMIGROUP`](#SEMIGROUP) [`(RESULT :B :A)`](#RESULT)
- [`SEMIGROUP`](#SEMIGROUP) [`(LIST :A)`](#LIST)
- [`SEMIGROUP :A`](#SEMIGROUP) `=>` [`SEMIGROUP`](#SEMIGROUP) [`(OPTIONAL :A)`](#OPTIONAL)
- [`SEMIGROUP`](#SEMIGROUP) [`STRING`](#STRING)

</details>


***

#### `APPLICATIVE` <sup><sub>[CLASS]</sub></sup><a name="APPLICATIVE"></a>
[`FUNCTOR :A`](#FUNCTOR) `=>` [`APPLICATIVE`](#APPLICATIVE) [`:A`](#:A)

Types which are a functor which can embed pure expressions and sequence operations.

Methods:
- `PURE :: ∀ :B. (:B → (:A :B))`
- `LIFTA2 :: ∀ :B :C :D. ((:B → :C → :D) → (:A :B) → (:A :C) → (:A :D))`

<details>
<summary>Instances</summary>

- [`APPLICATIVE`](#APPLICATIVE) [`CELL`](#CELL)
- [`APPLICATIVE`](#APPLICATIVE) [`(RESULT :A)`](#RESULT)
- [`APPLICATIVE`](#APPLICATIVE) [`LIST`](#LIST)
- [`APPLICATIVE`](#APPLICATIVE) [`OPTIONAL`](#OPTIONAL)

</details>


***


### Functions

#### `<` <sup><sub>[FUNCTION]</sub></sup><a name="<"></a>
`∀ :A. ORD :A ⇒ (:A → :A → BOOLEAN)`

Is X less than Y?


***

#### `>` <sup><sub>[FUNCTION]</sub></sup><a name=">"></a>
`∀ :A. ORD :A ⇒ (:A → :A → BOOLEAN)`

Is X greater than Y?


***

#### `<=` <sup><sub>[FUNCTION]</sub></sup><a name="<="></a>
`∀ :A. ORD :A ⇒ (:A → :A → BOOLEAN)`

Is X less than or equal to Y?


***

#### `>=` <sup><sub>[FUNCTION]</sub></sup><a name=">="></a>
`∀ :A. ORD :A ⇒ (:A → :A → BOOLEAN)`

Is X greater than or equal to Y?


***

#### `MAX` <sup><sub>[FUNCTION]</sub></sup><a name="MAX"></a>
`∀ :A. ORD :A ⇒ (:A → :A → :A)`

Returns the greater element of X and Y.


***

#### `MIN` <sup><sub>[FUNCTION]</sub></sup><a name="MIN"></a>
`∀ :A. ORD :A ⇒ (:A → :A → :A)`

Returns the lesser element of X and Y.


***


## File: [boolean.lisp](../src/library/boolean.lisp)

### Functions

#### `NOT` <sup><sub>[FUNCTION]</sub></sup><a name="NOT"></a>
`(BOOLEAN → BOOLEAN)`

Synonym for BOOLEAN-NOT.


***

#### `XOR` <sup><sub>[FUNCTION]</sub></sup><a name="XOR"></a>
`(BOOLEAN → BOOLEAN → BOOLEAN)`

Synonym for BOOLEAN-XOR.


***

#### `BOOLEAN-OR` <sup><sub>[FUNCTION]</sub></sup><a name="BOOLEAN-OR"></a>
`(BOOLEAN → BOOLEAN → BOOLEAN)`

Is X or Y True? Note that this is a *function* which means both X and Y will be evaluated. Use the OR macro for short-circuiting behavior.


***

#### `BOOLEAN-AND` <sup><sub>[FUNCTION]</sub></sup><a name="BOOLEAN-AND"></a>
`(BOOLEAN → BOOLEAN → BOOLEAN)`

Are X and Y True? Note that this is a *function* which means both X and Y will be evaluated. Use the AND macro for short-circuiting behavior.


***

#### `BOOLEAN-NOT` <sup><sub>[FUNCTION]</sub></sup><a name="BOOLEAN-NOT"></a>
`(BOOLEAN → BOOLEAN)`

Is X False?


***

#### `BOOLEAN-XOR` <sup><sub>[FUNCTION]</sub></sup><a name="BOOLEAN-XOR"></a>
`(BOOLEAN → BOOLEAN → BOOLEAN)`

Are X or Y True, but not both?


***


## File: [builtin.lisp](../src/library/builtin.lisp)

### Functions

#### `UNDEFINED` <sup><sub>[FUNCTION]</sub></sup><a name="UNDEFINED"></a>
`∀ :A :B. (:A → :B)`

A function which can be used in place of any value, throwing an error at runtime.


***


## File: [fraction.lisp](../src/library/fraction.lisp)

### Functions

#### `NUMERATOR` <sup><sub>[FUNCTION]</sub></sup><a name="NUMERATOR"></a>
`(FRACTION → INTEGER)`

The numerator of a fraction Q.


***

#### `DENOMINATOR` <sup><sub>[FUNCTION]</sub></sup><a name="DENOMINATOR"></a>
`(FRACTION → INTEGER)`

The denominator of a fraction Q.


***


## File: [arith.lisp](../src/library/arith.lisp)

### Functions

#### `/` <sup><sub>[FUNCTION]</sub></sup><a name="/"></a>
`∀ :A :B. DIVIDABLE :A :B ⇒ (:A → :A → (OPTIONAL :B))`

Divide X by Y, returning None if Y is zero.


***

#### `ABS` <sup><sub>[FUNCTION]</sub></sup><a name="ABS"></a>
`∀ :A. (NUM :A) (ORD :A) ⇒ (:A → :A)`

Absolute value of X.


***

#### `ASH` <sup><sub>[FUNCTION]</sub></sup><a name="ASH"></a>
`(INTEGER → INTEGER → INTEGER)`

Compute the "arithmetic shift" of X by N. 


***

#### `GCD` <sup><sub>[FUNCTION]</sub></sup><a name="GCD"></a>
`(INTEGER → INTEGER → INTEGER)`

Compute the greatest common divisor of A and B.


***

#### `LCM` <sup><sub>[FUNCTION]</sub></sup><a name="LCM"></a>
`(INTEGER → INTEGER → INTEGER)`

Compute the least common multiple of A and B.


***

#### `MOD` <sup><sub>[FUNCTION]</sub></sup><a name="MOD"></a>
`(INTEGER → INTEGER → INTEGER)`

Compute NUM modulo BASE.


***

#### `ODD` <sup><sub>[FUNCTION]</sub></sup><a name="ODD"></a>
`(INTEGER → BOOLEAN)`

Is N odd?


***

#### `EVEN` <sup><sub>[FUNCTION]</sub></sup><a name="EVEN"></a>
`(INTEGER → BOOLEAN)`

Is N even?


***

#### `EXPT` <sup><sub>[FUNCTION]</sub></sup><a name="EXPT"></a>
`(INTEGER → INTEGER → INTEGER)`

Exponentiate BASE to the POWER.


***

#### `SIGN` <sup><sub>[FUNCTION]</sub></sup><a name="SIGN"></a>
`∀ :A. (NUM :A) (ORD :A) ⇒ (:A → INTEGER)`

The sign of X.


***

#### `NEGATE` <sup><sub>[FUNCTION]</sub></sup><a name="NEGATE"></a>
`∀ :A. NUM :A ⇒ (:A → :A)`

***

#### `DOUBLE-FLOAT->INTEGER` <sup><sub>[FUNCTION]</sub></sup><a name="DOUBLE-FLOAT->INTEGER"></a>
`(DOUBLE-FLOAT → (OPTIONAL INTEGER))`

Round a Double-Float to the nearest Integer.


***

#### `INTEGER->DOUBLE-FLOAT` <sup><sub>[FUNCTION]</sub></sup><a name="INTEGER->DOUBLE-FLOAT"></a>
`(INTEGER → DOUBLE-FLOAT)`

***

#### `INTEGER->SINGLE-FLOAT` <sup><sub>[FUNCTION]</sub></sup><a name="INTEGER->SINGLE-FLOAT"></a>
`(INTEGER → SINGLE-FLOAT)`

***

#### `SINGLE-FLOAT->INTEGER` <sup><sub>[FUNCTION]</sub></sup><a name="SINGLE-FLOAT->INTEGER"></a>
`(SINGLE-FLOAT → (OPTIONAL INTEGER))`

Round a Single-Float to the nearest Integer.


***


## File: [string.lisp](../src/library/string.lisp)

### Functions

#### `PARSE-INT` <sup><sub>[FUNCTION]</sub></sup><a name="PARSE-INT"></a>
`(STRING → (OPTIONAL INTEGER))`

Parse the integer in string STR.


***

#### `PACK-STRING` <sup><sub>[FUNCTION]</sub></sup><a name="PACK-STRING"></a>
`((LIST CHAR) → STRING)`

Pack a list of charactes into a string.


***

#### `CONCAT-STRING` <sup><sub>[FUNCTION]</sub></sup><a name="CONCAT-STRING"></a>
`(STRING → STRING → STRING)`

Concatenate STR1 and STR2 together, returning a new string.


***

#### `UNPACK-STRING` <sup><sub>[FUNCTION]</sub></sup><a name="UNPACK-STRING"></a>
`(STRING → (LIST CHAR))`

Unpack a string into a list of characters.


***


## File: [optional.lisp](../src/library/optional.lisp)

### Functions

#### `ISNONE` <sup><sub>[FUNCTION]</sub></sup><a name="ISNONE"></a>
`∀ :A. ((OPTIONAL :A) → BOOLEAN)`

Is X None?


***

#### `ISSOME` <sup><sub>[FUNCTION]</sub></sup><a name="ISSOME"></a>
`∀ :A. ((OPTIONAL :A) → BOOLEAN)`

Is X Some?


***

#### `FROMSOME` <sup><sub>[FUNCTION]</sub></sup><a name="FROMSOME"></a>
`∀ :A. (STRING → (OPTIONAL :A) → :A)`

Get the value of OPT, erroring with the provided string if it is None.


***


## File: [list.lisp](../src/library/list.lisp)

### Functions

#### `ALL` <sup><sub>[FUNCTION]</sub></sup><a name="ALL"></a>
`∀ :A. ((:A → BOOLEAN) → (LIST :A) → BOOLEAN)`

Returns TRUE if every element in XS matches F.


***

#### `ANY` <sup><sub>[FUNCTION]</sub></sup><a name="ANY"></a>
`∀ :A. ((:A → BOOLEAN) → (LIST :A) → BOOLEAN)`

Returns TRUE if at least one element in XS matches F.


***

#### `SUM` <sup><sub>[FUNCTION]</sub></sup><a name="SUM"></a>
`∀ :A. NUM :A ⇒ ((LIST :A) → :A)`

Returns the sum of XS


***

#### `ZIP` <sup><sub>[FUNCTION]</sub></sup><a name="ZIP"></a>
`∀ :A :B. ((LIST :A) → (LIST :B) → (LIST (TUPLE :A :B)))`

Builds a list of tuples with the elements of XS and YS.


***

#### `DROP` <sup><sub>[FUNCTION]</sub></sup><a name="DROP"></a>
`∀ :A. (INTEGER → (LIST :A) → (LIST :A))`

Returns a list with the first N elements of XS removed


***

#### `FIND` <sup><sub>[FUNCTION]</sub></sup><a name="FIND"></a>
`∀ :A. ((:A → BOOLEAN) → (LIST :A) → (OPTIONAL :A))`

Returns the first element in a list matching the predicate function F.


***

#### `FOLD` <sup><sub>[FUNCTION]</sub></sup><a name="FOLD"></a>
`∀ :A :B. ((:A → :B → :B) → :B → (LIST :A) → :B)`

Tail recursive left fold on lists.


***

#### `HEAD` <sup><sub>[FUNCTION]</sub></sup><a name="HEAD"></a>
`∀ :A. ((LIST :A) → (OPTIONAL :A))`

Returns the first element of a list.


***

#### `NULL` <sup><sub>[FUNCTION]</sub></sup><a name="NULL"></a>
`∀ :A. ((LIST :A) → BOOLEAN)`

Returns TRUE if XS is an empty list.


***

#### `SORT` <sup><sub>[FUNCTION]</sub></sup><a name="SORT"></a>
`∀ :A. ORD :A ⇒ ((LIST :A) → (LIST :A))`

Performs a stable sort of XS.


***

#### `TAIL` <sup><sub>[FUNCTION]</sub></sup><a name="TAIL"></a>
`∀ :A. ((LIST :A) → (OPTIONAL (LIST :A)))`

Returns every element but the first in a list.


***

#### `TAKE` <sup><sub>[FUNCTION]</sub></sup><a name="TAKE"></a>
`∀ :A. (INTEGER → (LIST :A) → (LIST :A))`

Returns the first N elements of XS


***

#### `FOLDR` <sup><sub>[FUNCTION]</sub></sup><a name="FOLDR"></a>
`∀ :A :B. ((:A → :B → :B) → :B → (LIST :A) → :B)`

Right fold on lists. Is not tail recursive.


***

#### `INDEX` <sup><sub>[FUNCTION]</sub></sup><a name="INDEX"></a>
`∀ :A. ((LIST :A) → INTEGER → (OPTIONAL :A))`

Returns the Ith element of XS.


***

#### `RANGE` <sup><sub>[FUNCTION]</sub></sup><a name="RANGE"></a>
`(INTEGER → INTEGER → (LIST INTEGER))`

Returns a list containing the numbers from START to END inclusive.


***

#### `UNION` <sup><sub>[FUNCTION]</sub></sup><a name="UNION"></a>
`∀ :A. EQ :A ⇒ ((LIST :A) → (LIST :A) → (LIST :A))`

Returns a new list with the elements from both XS and YS and without duplicates.


***

#### `APPEND` <sup><sub>[FUNCTION]</sub></sup><a name="APPEND"></a>
`∀ :A. ((LIST :A) → (LIST :A) → (LIST :A))`

Appends two lists together and returns a new list.


***

#### `CONCAT` <sup><sub>[FUNCTION]</sub></sup><a name="CONCAT"></a>
`∀ :A. ((LIST (LIST :A)) → (LIST :A))`

Appends a list of lists together into a single new list.


***

#### `DELETE` <sup><sub>[FUNCTION]</sub></sup><a name="DELETE"></a>
`∀ :A. EQ :A ⇒ (:A → (LIST :A) → (LIST :A))`

Return a new list with the first element equal to X removed.


***

#### `FILTER` <sup><sub>[FUNCTION]</sub></sup><a name="FILTER"></a>
`∀ :A. ((:A → BOOLEAN) → (LIST :A) → (LIST :A))`

Returns a new list containing every element of XS that matches the predicate function F in the same order.


***

#### `INSERT` <sup><sub>[FUNCTION]</sub></sup><a name="INSERT"></a>
`∀ :A. ORD :A ⇒ (:A → (LIST :A) → (LIST :A))`

Inserts an element into a list at the first place it is less than or equal to the next element.


***

#### `LENGTH` <sup><sub>[FUNCTION]</sub></sup><a name="LENGTH"></a>
`∀ :A. ((LIST :A) → INTEGER)`

Returns the length of a list.


***

#### `LOOKUP` <sup><sub>[FUNCTION]</sub></sup><a name="LOOKUP"></a>
`∀ :A :B. EQ :A ⇒ (:A → (LIST (TUPLE :A :B)) → (OPTIONAL :B))`

Returns the value of the first (key, value) tuple in XS where the key matches E.


***

#### `MEMBER` <sup><sub>[FUNCTION]</sub></sup><a name="MEMBER"></a>
`∀ :A. EQ :A ⇒ (:A → (LIST :A) → BOOLEAN)`

Returns true if any element of XS is equal to E.


***

#### `REPEAT` <sup><sub>[FUNCTION]</sub></sup><a name="REPEAT"></a>
`∀ :A. (INTEGER → :A → (LIST :A))`

Returns a list with X repeated N times.


***

#### `SORTBY` <sup><sub>[FUNCTION]</sub></sup><a name="SORTBY"></a>
`∀ :A. ((:A → :A → ORD) → (LIST :A) → (LIST :A))`

Generic version of sort


***

#### `MAXIMUM` <sup><sub>[FUNCTION]</sub></sup><a name="MAXIMUM"></a>
`∀ :A. ORD :A ⇒ ((LIST :A) → (OPTIONAL :A))`

Returns the greatest element in XS.


***

#### `MINIMUM` <sup><sub>[FUNCTION]</sub></sup><a name="MINIMUM"></a>
`∀ :A. ORD :A ⇒ ((LIST :A) → (OPTIONAL :A))`

Returns the least element in XS.


***

#### `PRODUCT` <sup><sub>[FUNCTION]</sub></sup><a name="PRODUCT"></a>
`∀ :A. NUM :A ⇒ ((LIST :A) → :A)`

Returns the product of XS


***

#### `REVERSE` <sup><sub>[FUNCTION]</sub></sup><a name="REVERSE"></a>
`∀ :A. ((LIST :A) → (LIST :A))`

Returns a new list containing the same elements in reverse order.


***

#### `ZIPWITH` <sup><sub>[FUNCTION]</sub></sup><a name="ZIPWITH"></a>
`∀ :A :B :C. ((:A → :B → :C) → (LIST :A) → (LIST :B) → (LIST :C))`

Builds a new list by calling F with elements of XS and YS.


***

#### `INSERTBY` <sup><sub>[FUNCTION]</sub></sup><a name="INSERTBY"></a>
`∀ :A. ((:A → :A → ORD) → :A → (LIST :A) → (LIST :A))`

Generic version of insert


***

#### `ZIPWITH3` <sup><sub>[FUNCTION]</sub></sup><a name="ZIPWITH3"></a>
`∀ :A :B :C :D. ((:A → :B → :C → :D) → (LIST :A) → (LIST :B) → (LIST :C) → (LIST :D))`

Build a new list by calling F with elements of XS, YS and ZS


***

#### `ZIPWITH4` <sup><sub>[FUNCTION]</sub></sup><a name="ZIPWITH4"></a>
`∀ :A :B :C :D :E. ((:A → :B → :C → :D → :E) → (LIST :A) → (LIST :B) → (LIST :C) → (LIST :D) → (LIST :E))`

Build a new list by calling F with elements of AS, BS, CS and DS


***

#### `ZIPWITH5` <sup><sub>[FUNCTION]</sub></sup><a name="ZIPWITH5"></a>
`∀ :A :B :C :D :E :F. ((:A → :B → :C → :D → :E → :F) → (LIST :A) → (LIST :B) → (LIST :C) → (LIST :D) → (LIST :E) → (LIST :F))`

Build a new list by calling F with elements of AS, BS, CS, DS and ES


***

#### `CONCATMAP` <sup><sub>[FUNCTION]</sub></sup><a name="CONCATMAP"></a>
`∀ :A :B. ((:A → (LIST :B)) → (LIST :A) → (LIST :B))`

Apply F to each element in XS and concatenate the results.


***

#### `ELEMINDEX` <sup><sub>[FUNCTION]</sub></sup><a name="ELEMINDEX"></a>
`∀ :A. EQ :A ⇒ (:A → (LIST :A) → (OPTIONAL INTEGER))`

***

#### `FINDINDEX` <sup><sub>[FUNCTION]</sub></sup><a name="FINDINDEX"></a>
`∀ :A. ((:A → BOOLEAN) → (LIST :A) → (OPTIONAL INTEGER))`

***

#### `PARTITION` <sup><sub>[FUNCTION]</sub></sup><a name="PARTITION"></a>
`∀ :A. ((:A → BOOLEAN) → (LIST :A) → (TUPLE (LIST :A) (LIST :A)))`

Splits a list into two new lists. The first list contains elements matching predicate F.


***

#### `SINGLETON` <sup><sub>[FUNCTION]</sub></sup><a name="SINGLETON"></a>
`∀ :A. (:A → (LIST :A))`

Returns a single element list containg only X.


***

#### `TRANSPOSE` <sup><sub>[FUNCTION]</sub></sup><a name="TRANSPOSE"></a>
`∀ :A. ((LIST (LIST :A)) → (LIST (LIST :A)))`

Transposes a matrix represented by a list of lists.


***

#### `INTERCALATE` <sup><sub>[FUNCTION]</sub></sup><a name="INTERCALATE"></a>
`∀ :A. ((LIST :A) → (LIST (LIST :A)) → (LIST :A))`

Intersperses XS into XSS and then concatenates the result.


***

#### `INTERSPERSE` <sup><sub>[FUNCTION]</sub></sup><a name="INTERSPERSE"></a>
`∀ :A. (:A → (LIST :A) → (LIST :A))`

Returns a new list where every other element is E.


***

#### `INTERSECTION` <sup><sub>[FUNCTION]</sub></sup><a name="INTERSECTION"></a>
`∀ :A. EQ :A ⇒ ((LIST :A) → (LIST :A) → (LIST :A))`

Returns elements which occur in both lists. Does not return duplicates and does not guarantee order.


***

#### `LIST-DIFFERENCE` <sup><sub>[FUNCTION]</sub></sup><a name="LIST-DIFFERENCE"></a>
`∀ :A. EQ :A ⇒ ((LIST :A) → (LIST :A) → (LIST :A))`

Returns a new list with the first occurence of each element in YS deleted from XS.


***

#### `REMOVE-DUPLICATES` <sup><sub>[FUNCTION]</sub></sup><a name="REMOVE-DUPLICATES"></a>
`∀ :A. EQ :A ⇒ ((LIST :A) → (LIST :A))`

Returns a new list without duplicate elements.


***


## File: [tuple.lisp](../src/library/tuple.lisp)

### Functions

#### `FST` <sup><sub>[FUNCTION]</sub></sup><a name="FST"></a>
`∀ :A :B. ((TUPLE :A :B) → :A)`

Get the first element of a tuple.


***

#### `SND` <sup><sub>[FUNCTION]</sub></sup><a name="SND"></a>
`∀ :A :B. ((TUPLE :A :B) → :B)`

Get the second element of a tuple.


***


## File: [result.lisp](../src/library/result.lisp)

### Functions

#### `ISOK` <sup><sub>[FUNCTION]</sub></sup><a name="ISOK"></a>
`∀ :A :B. ((RESULT :A :B) → BOOLEAN)`

Returns TRUE if X is ERR


***

#### `ISERR` <sup><sub>[FUNCTION]</sub></sup><a name="ISERR"></a>
`∀ :A :B. ((RESULT :A :B) → BOOLEAN)`

Returns TRUE if X is ERR


***

#### `MAPERR` <sup><sub>[FUNCTION]</sub></sup><a name="MAPERR"></a>
`∀ :A :B :C. ((:A → :B) → (RESULT :A :C) → (RESULT :B :C))`

Map over the ERR case


***


## File: [functions.lisp](../src/library/functions.lisp)

### Functions

#### `ID` <sup><sub>[FUNCTION]</sub></sup><a name="ID"></a>
`∀ :A. (:A → :A)`

A function that always returns its argument


***

#### `FIX` <sup><sub>[FUNCTION]</sub></sup><a name="FIX"></a>
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


***

#### `ASUM` <sup><sub>[FUNCTION]</sub></sup><a name="ASUM"></a>
`∀ :A :B. ALTERNATIVE :A ⇒ ((LIST (:A :B)) → (:A :B))`

Fold over a list using alt


***

#### `FLIP` <sup><sub>[FUNCTION]</sub></sup><a name="FLIP"></a>
`∀ :A :B :C. ((:A → :B → :C) → :B → :A → :C)`

FLIP reverses the arguments to F


***

#### `CONST` <sup><sub>[FUNCTION]</sub></sup><a name="CONST"></a>
`∀ :A :B. (:A → :B → :A)`

A function that always returns its first argument


***

#### `ERROR` <sup><sub>[FUNCTION]</sub></sup><a name="ERROR"></a>
`∀ :A. (STRING → :A)`

Signal an error by calling CL:ERROR


***

#### `TRACE` <sup><sub>[FUNCTION]</sub></sup><a name="TRACE"></a>
`(STRING → UNIT)`

Print a line to *STANDARD-OUTPUT*


***

#### `COMPOSE` <sup><sub>[FUNCTION]</sub></sup><a name="COMPOSE"></a>
`∀ :A :B :C. ((:A → :B) → (:C → :A) → :C → :B)`

***

#### `SEQUENCE` <sup><sub>[FUNCTION]</sub></sup><a name="SEQUENCE"></a>
`∀ :A :B. APPLICATIVE :A ⇒ ((LIST (:A :B)) → (:A (LIST :B)))`

***

#### `TRAVERSE` <sup><sub>[FUNCTION]</sub></sup><a name="TRAVERSE"></a>
`∀ :A :B :C. APPLICATIVE :B ⇒ ((:A → (:B :C)) → (LIST :A) → (:B (LIST :C)))`

Map the elements of XS with F then collect the results.


***

#### `TRACEOBJECT` <sup><sub>[FUNCTION]</sub></sup><a name="TRACEOBJECT"></a>
`∀ :A. (STRING → :A → UNIT)`

Print a line to *STANDARD-OUTPUT* in the form "{STR}: {ITEM}"


***


## File: [cell.lisp](../src/library/cell.lisp)

### Types

#### `CELL :A` <sup><sub>[TYPE]</sub></sup><a name="CELL"></a>
- `(CELL LISP-OBJECT)`

Internally mutable cell

Constructors:
- `CELL :: (LISP-OBJECT → (CELL :A))`

<details>
<summary>Instances</summary>

- [`EQ :A`](#EQ) `=>` [`EQ`](#EQ) [`(CELL :A)`](#CELL)
- [`NUM :A`](#NUM) `=>` [`NUM`](#NUM) [`(CELL :A)`](#CELL)
- [`INTO`](#INTO) [`(CELL :A)`](#CELL) [`:A`](#:A)
- [`INTO`](#INTO) [`:A`](#:A) [`(CELL :A)`](#CELL)
- [`SHOW :A`](#SHOW) `=>` [`SHOW`](#SHOW) [`(CELL :A)`](#CELL)
- [`FUNCTOR`](#FUNCTOR) [`CELL`](#CELL)
- [`SEMIGROUP :A`](#SEMIGROUP) `=>` [`SEMIGROUP`](#SEMIGROUP) [`(CELL :A)`](#CELL)
- [`APPLICATIVE`](#APPLICATIVE) [`CELL`](#CELL)

</details>

***

### Functions

#### `CELL-READ` <sup><sub>[FUNCTION]</sub></sup><a name="CELL-READ"></a>
`∀ :A. ((CELL :A) → :A)`

Read the value of a mutable cell


***

#### `CELL-SWAP` <sup><sub>[FUNCTION]</sub></sup><a name="CELL-SWAP"></a>
`∀ :A. (:A → (CELL :A) → :A)`

Replace the value of a mutable cell with a new value, then return the old value


***

#### `MAKE-CELL` <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-CELL"></a>
`∀ :A. (:A → (CELL :A))`

Create a new mutable cell


***

#### `CELL-WRITE` <sup><sub>[FUNCTION]</sub></sup><a name="CELL-WRITE"></a>
`∀ :A. (:A → (CELL :A) → UNIT)`

Set the value of a mutable cell


***

#### `CELL-UPDATE` <sup><sub>[FUNCTION]</sub></sup><a name="CELL-UPDATE"></a>
`∀ :A. ((:A → :A) → (CELL :A) → UNIT)`

***


## File: [vector.lisp](../src/library/vector.lisp)

### Types

#### `VECTOR :A` <sup><sub>[TYPE]</sub></sup><a name="VECTOR"></a>
- `(VECTOR LISP-OBJECT)`

Constructors:
- `VECTOR :: (LISP-OBJECT → (VECTOR :A))`

<details>
<summary>Instances</summary>

- [`EQ :A`](#EQ) `=>` [`EQ`](#EQ) [`(VECTOR :A)`](#VECTOR)
- [`ISO`](#ISO) [`(VECTOR :A)`](#VECTOR) [`(LIST :A)`](#LIST)
- [`INTO`](#INTO) [`(VECTOR :A)`](#VECTOR) [`(LIST :A)`](#LIST)
- [`INTO`](#INTO) [`(LIST :A)`](#LIST) [`(VECTOR :A)`](#VECTOR)
- [`FUNCTOR`](#FUNCTOR) [`VECTOR`](#VECTOR)
- [`SEMIGROUP`](#SEMIGROUP) [`(VECTOR :A)`](#VECTOR)

</details>

***

### Functions

#### `VECTOR-POP` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-POP"></a>
`∀ :A. ((VECTOR :A) → (OPTIONAL :A))`

Remove and return the first item of V


***

#### `VECTOR-SET` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-SET"></a>
`∀ :A. (INTEGER → :A → (VECTOR :A) → UNIT)`

Set the INDEXth element of V to ITEM. This function left intentionally unsafe because it does not have a return value to check.


***

#### `MAKE-VECTOR` <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-VECTOR"></a>
`∀ :A. (UNIT → (VECTOR :A))`

Create a new empty vector


***

#### `VECTOR-COPY` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-COPY"></a>
`∀ :A. ((VECTOR :A) → (VECTOR :A))`

Return a new vector containing the same elements as V


***

#### `VECTOR-HEAD` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-HEAD"></a>
`∀ :A. ((VECTOR :A) → (OPTIONAL :A))`

Return the first item of V


***

#### `VECTOR-LAST` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-LAST"></a>
`∀ :A. ((VECTOR :A) → (OPTIONAL :A))`

Return the last element of V


***

#### `VECTOR-PUSH` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-PUSH"></a>
`∀ :A. (:A → (VECTOR :A) → INTEGER)`

Append ITEM to V and resize V if necessary


***

#### `VECTOR-SORT` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-SORT"></a>
`∀ :A. ORD :A ⇒ ((VECTOR :A) → UNIT)`

Sort a vector inplace


***

#### `VECTOR-EMPTY` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-EMPTY"></a>
`∀ :A. ((VECTOR :A) → BOOLEAN)`

Returns TRUE if V is empty


***

#### `VECTOR-INDEX` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-INDEX"></a>
`∀ :A. (INTEGER → (VECTOR :A) → (OPTIONAL :A))`

Return the INDEXth element of V


***

#### `VECTOR-APPEND` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-APPEND"></a>
`∀ :A. ((VECTOR :A) → (VECTOR :A) → (VECTOR :A))`

Create a new VECTOR containing the elements of v1 followed by the elements of v2


***

#### `VECTOR-LENGTH` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-LENGTH"></a>
`∀ :A. ((VECTOR :A) → INTEGER)`

Returns the length of V


***

#### `VECTOR-FOREACH` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-FOREACH"></a>
`∀ :A :B. ((:A → :B) → (VECTOR :A) → UNIT)`

Call the function F once for each item in V


***

#### `VECTOR-SORT-BY` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-SORT-BY"></a>
`∀ :A. ((:A → :A → BOOLEAN) → (VECTOR :A) → UNIT)`

Sort a vector with predicate function F


***

#### `VECTOR-CAPACITY` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-CAPACITY"></a>
`∀ :A. ((VECTOR :A) → INTEGER)`

Returns the number of elements that V can store without resizing


***

#### `VECTOR-FOREACH2` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-FOREACH2"></a>
`∀ :A :B. ((:A → :A → :B) → (VECTOR :A) → (VECTOR :A) → UNIT)`

Like vector-foreach but twice as good


***

#### `VECTOR-POP-UNSAFE` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-POP-UNSAFE"></a>
`∀ :A. ((VECTOR :A) → :A)`

Remove and return the first item of V without checking if the vector is empty


***

#### `VECTOR-HEAD-UNSAFE` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-HEAD-UNSAFE"></a>
`∀ :A. ((VECTOR :A) → :A)`

Return the first item of V without first checking if V is empty


***

#### `VECTOR-LAST-UNSAFE` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-LAST-UNSAFE"></a>
`∀ :A. ((VECTOR :A) → :A)`

Return the last element of V without first checking if V is empty


***

#### `VECTOR-SWAP-REMOVE` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-SWAP-REMOVE"></a>
`∀ :A. (INTEGER → (VECTOR :A) → (OPTIONAL :A))`

Remove the element IDX from VEC and replace it with the last element in VEC. Then return the removed element.


***

#### `VECTOR-INDEX-UNSAFE` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-INDEX-UNSAFE"></a>
`∀ :A. (INTEGER → (VECTOR :A) → :A)`

Return the INDEXth element of V without checking if the element exists


***

#### `MAKE-VECTOR-CAPACITY` <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-VECTOR-CAPACITY"></a>
`∀ :A. (INTEGER → (VECTOR :A))`

Create a new vector with N elements preallocated


***

#### `VECTOR-FOREACH-INDEX` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-FOREACH-INDEX"></a>
`∀ :A :B. ((INTEGER → :A → :B) → (VECTOR :A) → UNIT)`

Call the function F once for each item in V with its index


***

#### `VECTOR-SWAP-REMOVE-UNSAFE` <sup><sub>[FUNCTION]</sub></sup><a name="VECTOR-SWAP-REMOVE-UNSAFE"></a>
`∀ :A. (INTEGER → (VECTOR :A) → :A)`

Remove the element IDX from VEC and replace it with the last element in VEC without bounds checking. Then return the removed element.


***


## File: [hashtable.lisp](../src/library/hashtable.lisp)

### Types

#### `HASHTABLE :A :B` <sup><sub>[TYPE]</sub></sup><a name="HASHTABLE"></a>
- `(HASHTABLE LISP-OBJECT)`

Constructors:
- `HASHTABLE :: (LISP-OBJECT → (HASHTABLE :A :B))`

***

### Functions

#### `HASHTABLE-GET` <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-GET"></a>
`∀ :A :B. (:A → (HASHTABLE :A :B) → (OPTIONAL :B))`

Lookup KEY in TABLE


***

#### `HASHTABLE-SET` <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-SET"></a>
`∀ :A :B. (:A → :B → (HASHTABLE :A :B) → UNIT)`

Set KEY to VALUE in TABLE


***

#### `HASHTABLE-KEYS` <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-KEYS"></a>
`∀ :A :B. ((HASHTABLE :A :B) → (VECTOR :A))`

Returns the keys in TABLE as a vector


***

#### `MAKE-HASHTABLE` <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-HASHTABLE"></a>
`∀ :A :B. (UNIT → (HASHTABLE :A :B))`

Create a new empty hashtable


***

#### `HASHTABLE-COUNT` <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-COUNT"></a>
`∀ :A :B. ((HASHTABLE :A :B) → INTEGER)`

Returns the number of entries in TABLE


***

#### `HASHTABLE-REMOVE` <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-REMOVE"></a>
`∀ :A :B. (:A → (HASHTABLE :A :B) → UNIT)`

Remove the entry at KEY from TABLE


***

#### `HASHTABLE-VALUES` <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-VALUES"></a>
`∀ :A :B. ((HASHTABLE :A :B) → (VECTOR :B))`

Returns the values in TABLE as a vector


***

#### `HASHTABLE-ENTRIES` <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-ENTRIES"></a>
`∀ :A :B. ((HASHTABLE :A :B) → (VECTOR (TUPLE :A :B)))`

Returns the keys and values in TABLE as a vector


***

#### `HASHTABLE-FOREACH` <sup><sub>[FUNCTION]</sub></sup><a name="HASHTABLE-FOREACH"></a>
`∀ :A :B :C. ((:A → :B → :C) → (HASHTABLE :A :B) → UNIT)`

Call F once for each key value pair in TABLE


***

#### `MAKE-HASHTABLE-CAPACITY` <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-HASHTABLE-CAPACITY"></a>
`∀ :A :B. (INTEGER → (HASHTABLE :A :B))`

Crate a new empty hashtable with a given capacity


***


## File: [graph.lisp](../src/library/graph.lisp)

### Types

#### `GRAPH :A :B` <sup><sub>[TYPE]</sub></sup><a name="GRAPH"></a>
- `(GRAPH GRAPHTYPE (VECTOR (NODE :A)) (VECTOR (EDGE :B)))`

A graph using adjacency list representation

Constructors:
- `GRAPH :: (GRAPHTYPE → (VECTOR (NODE :A)) → (VECTOR (EDGE :B)) → (GRAPH :A :B))`

***

#### `EDGEINDEX` <sup><sub>[TYPE]</sub></sup><a name="EDGEINDEX"></a>
- `(EDGEINDEX INTEGER)`

Constructors:
- `EDGEINDEX :: (INTEGER → EDGEINDEX)`

<details>
<summary>Instances</summary>

- [`EQ`](#EQ) [`EDGEINDEX`](#EDGEINDEX)
- [`INTO`](#INTO) [`EDGEINDEX`](#EDGEINDEX) [`INTEGER`](#INTEGER)
- [`SHOW`](#SHOW) [`EDGEINDEX`](#EDGEINDEX)

</details>

***

#### `GRAPHTYPE` <sup><sub>[TYPE]</sub></sup><a name="GRAPHTYPE"></a>
- `UNDIRECTED`
- `DIRECTED`

Constructors:
- `UNDIRECTED :: GRAPHTYPE`
- `DIRECTED :: GRAPHTYPE`

***

#### `NODEINDEX` <sup><sub>[TYPE]</sub></sup><a name="NODEINDEX"></a>
- `(NODEINDEX INTEGER)`

Constructors:
- `NODEINDEX :: (INTEGER → NODEINDEX)`

<details>
<summary>Instances</summary>

- [`EQ`](#EQ) [`NODEINDEX`](#NODEINDEX)
- [`INTO`](#INTO) [`NODEINDEX`](#NODEINDEX) [`INTEGER`](#INTEGER)
- [`SHOW`](#SHOW) [`NODEINDEX`](#NODEINDEX)

</details>

***

### Functions

#### `GRAPH-VIZ` <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-VIZ"></a>
`∀ :A :B. SHOW :A ⇒ ((GRAPH :A :B) → STRING)`

***

#### `MAKE-GRAPH` <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-GRAPH"></a>
`∀ :A :B. (UNIT → (GRAPH :A :B))`

Create a new empty undirected graph


***

#### `GRAPH-EDGES` <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-EDGES"></a>
`∀ :A :B. ((GRAPH :A :B) → (VECTOR (EDGE :B)))`

Returns the edges in a graph


***

#### `GRAPH-NODES` <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-NODES"></a>
`∀ :A :B. ((GRAPH :A :B) → (VECTOR (NODE :A)))`

Returns the nodes in a graph


***

#### `MAKE-DIGRAPH` <sup><sub>[FUNCTION]</sub></sup><a name="MAKE-DIGRAPH"></a>
`∀ :A :B. (UNIT → (GRAPH :A :B))`

Create a new directed graph


***

#### `GRAPH-ADD-EDGE` <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-ADD-EDGE"></a>
`∀ :A :B. (:A → NODEINDEX → NODEINDEX → (GRAPH :B :A) → EDGEINDEX)`

Add an edge with associated data from node FROM to node TO in the graph.


***

#### `GRAPH-ADD-NODE` <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-ADD-NODE"></a>
`∀ :A :B. (:A → (GRAPH :A :B) → NODEINDEX)`

Add a node with associated data to the graph, returning the index of the new node.


***

#### `GRAPH-EDGE-COUNT` <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-EDGE-COUNT"></a>
`∀ :A :B. ((GRAPH :A :B) → INTEGER)`

Returns the number of edges in a graph


***

#### `GRAPH-LOOKUP-EDGE` <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-LOOKUP-EDGE"></a>
`∀ :A :B. (EDGEINDEX → (GRAPH :A :B) → (OPTIONAL (EDGE :B)))`

Lookup a node with index IDX in graph G


***

#### `GRAPH-LOOKUP-NODE` <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-LOOKUP-NODE"></a>
`∀ :A :B. (NODEINDEX → (GRAPH :A :B) → (OPTIONAL (NODE :A)))`

Lookup a node with index IDX in graph G


***

#### `GRAPH-REMOVE-EDGE` <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-REMOVE-EDGE"></a>
`∀ :A :B. (EDGEINDEX → (GRAPH :A :B) → (OPTIONAL :B))`

Remove an edge from GRAPH


***

#### `GRAPH-REMOVE-NODE` <sup><sub>[FUNCTION]</sub></sup><a name="GRAPH-REMOVE-NODE"></a>
`∀ :A :B. (NODEINDEX → (GRAPH :A :B) → (OPTIONAL :A))`

Remove a node and all edges connecting to it from GRAPH


***


