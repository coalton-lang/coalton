# Reference for COALTON-USER

## File: [types.lisp](../src/library/types.lisp)

### Types

#### `LIST :A` <sup><sub>[TYPE]</sub></sup><a name="LIST"></a>
- `(CONS :A (LIST :A))`
- `NIL`

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

#### `BOOLEAN`<sup><sub>[TYPE]</sub></sup><a name="BOOLEAN"></a>
- `FALSE`
- `TRUE`

Constructors:
- `FALSE :: BOOLEAN`
- `TRUE :: BOOLEAN`

<details>
<summary>Instances</summary>

- [`EQ`](#EQ) [`BOOLEAN`](#BOOLEAN)
- [`ORD`](#ORD) [`BOOLEAN`](#BOOLEAN)
- [`SHOW`](#SHOW) [`BOOLEAN`](#BOOLEAN)

</details>

***

#### `OPTIONAL :A` <sup><sub>[TYPE]</sub></sup><a name="OPTIONAL"></a>
- `(SOME :A)`
- `NONE`

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

### Classes

#### `APPLICATIVE` <sup><sub>[CLASS]</sub></sup><a name="APPLICATIVE"></a>
[`FUNCTOR :A`](#FUNCTOR) `=>` [`APPLICATIVE`](#APPLICATIVE) [`:A`](#:A)

Methods:
- `PURE :: ∀ :B. (:B → (:A :B))`
- `LIFTA2 :: ∀ :B :C :D. ((:B → :C → :D) → (:A :B) → (:A :C) → (:A :D))`

***

#### `SEMIGROUP` <sup><sub>[CLASS]</sub></sup><a name="SEMIGROUP"></a>
[`SEMIGROUP`](#SEMIGROUP) [`:A`](#:A)

Methods:
- `<> :: (:A → :A → :A)`

***

#### `MONADFAIL` <sup><sub>[CLASS]</sub></sup><a name="MONADFAIL"></a>
[`MONAD :A`](#MONAD) `=>` [`MONADFAIL`](#MONADFAIL) [`:A`](#:A)

Methods:
- `FAIL :: ∀ :B. (STRING → (:A :B))`

***

#### `TRYINTO` <sup><sub>[CLASS]</sub></sup><a name="TRYINTO"></a>
[`TRYINTO`](#TRYINTO) [`:A`](#:A) [`:B`](#:B) [`:C`](#:C)

Methods:
- `TRYINTO :: (:A → (RESULT :B :C))`

***

#### `FUNCTOR` <sup><sub>[CLASS]</sub></sup><a name="FUNCTOR"></a>
[`FUNCTOR`](#FUNCTOR) [`:A`](#:A)

Methods:
- `MAP :: ∀ :B :C. ((:B → :C) → (:A :B) → (:A :C))`

***

#### `MONOID` <sup><sub>[CLASS]</sub></sup><a name="MONOID"></a>
[`SEMIGROUP :A`](#SEMIGROUP) `=>` [`MONOID`](#MONOID) [`:A`](#:A)

Methods:
- `MEMPTY :: :A`

***

#### `MONAD` <sup><sub>[CLASS]</sub></sup><a name="MONAD"></a>
[`APPLICATIVE :A`](#APPLICATIVE) `=>` [`MONAD`](#MONAD) [`:A`](#:A)

Methods:
- `>>= :: ∀ :B :C. ((:A :B) → (:B → (:A :C)) → (:A :C))`
- `>> :: ∀ :B :C. ((:A :B) → (:A :C) → (:A :C))`

***

#### `SHOW` <sup><sub>[CLASS]</sub></sup><a name="SHOW"></a>
[`SHOW`](#SHOW) [`:A`](#:A)

Methods:
- `SHOW :: (:A → STRING)`

***

#### `INTO` <sup><sub>[CLASS]</sub></sup><a name="INTO"></a>
[`INTO`](#INTO) [`:A`](#:A) [`:B`](#:B)

Methods:
- `INTO :: (:A → :B)`

***

#### `ORD` <sup><sub>[CLASS]</sub></sup><a name="ORD"></a>
[`EQ :A`](#EQ) `=>` [`ORD`](#ORD) [`:A`](#:A)

Methods:
- `<=> :: (:A → :A → ORD)`

***

#### `NUM` <sup><sub>[CLASS]</sub></sup><a name="NUM"></a>
[`EQ :A`](#EQ) [`SHOW :A`](#SHOW) `=>` [`NUM`](#NUM) [`:A`](#:A)

Methods:
- `+ :: (:A → :A → :A)`
- `- :: (:A → :A → :A)`
- `* :: (:A → :A → :A)`
- `FROMINT :: (INT → :A)`

***

#### `EQ` <sup><sub>[CLASS]</sub></sup><a name="EQ"></a>
[`EQ`](#EQ) [`:A`](#:A)

Methods:
- `== :: (:A → :A → BOOLEAN)`
- `/= :: (:A → :A → BOOLEAN)`

***


### Functions

#### `OR` <sup><sub>[FUNCTION]</sub></sup><a name="OR"></a>
`(BOOLEAN → BOOLEAN → BOOLEAN)`

***

#### `AND` <sup><sub>[FUNCTION]</sub></sup><a name="AND"></a>
`(BOOLEAN → BOOLEAN → BOOLEAN)`

***

#### `NOT` <sup><sub>[FUNCTION]</sub></sup><a name="NOT"></a>
`(BOOLEAN → BOOLEAN)`

***

#### `XOR` <sup><sub>[FUNCTION]</sub></sup><a name="XOR"></a>
`(BOOLEAN → BOOLEAN → BOOLEAN)`

***

#### `UNDEFINED` <sup><sub>[FUNCTION]</sub></sup><a name="UNDEFINED"></a>
`∀ :A :B. (:A → :B)`

***


## File: [classes.lisp](../src/library/classes.lisp)

### Types

#### `ORD`<sup><sub>[TYPE]</sub></sup><a name="ORD"></a>
- `LT`
- `GT`
- `EQ`

Constructors:
- `LT :: ORD`
- `GT :: ORD`
- `EQ :: ORD`

***

### Classes

#### `APPLICATIVE` <sup><sub>[CLASS]</sub></sup><a name="APPLICATIVE"></a>
[`FUNCTOR :A`](#FUNCTOR) `=>` [`APPLICATIVE`](#APPLICATIVE) [`:A`](#:A)

Methods:
- `PURE :: ∀ :B. (:B → (:A :B))`
- `LIFTA2 :: ∀ :B :C :D. ((:B → :C → :D) → (:A :B) → (:A :C) → (:A :D))`

***

#### `SEMIGROUP` <sup><sub>[CLASS]</sub></sup><a name="SEMIGROUP"></a>
[`SEMIGROUP`](#SEMIGROUP) [`:A`](#:A)

Methods:
- `<> :: (:A → :A → :A)`

***

#### `MONADFAIL` <sup><sub>[CLASS]</sub></sup><a name="MONADFAIL"></a>
[`MONAD :A`](#MONAD) `=>` [`MONADFAIL`](#MONADFAIL) [`:A`](#:A)

Methods:
- `FAIL :: ∀ :B. (STRING → (:A :B))`

***

#### `TRYINTO` <sup><sub>[CLASS]</sub></sup><a name="TRYINTO"></a>
[`TRYINTO`](#TRYINTO) [`:A`](#:A) [`:B`](#:B) [`:C`](#:C)

Methods:
- `TRYINTO :: (:A → (RESULT :B :C))`

***

#### `FUNCTOR` <sup><sub>[CLASS]</sub></sup><a name="FUNCTOR"></a>
[`FUNCTOR`](#FUNCTOR) [`:A`](#:A)

Methods:
- `MAP :: ∀ :B :C. ((:B → :C) → (:A :B) → (:A :C))`

***

#### `MONOID` <sup><sub>[CLASS]</sub></sup><a name="MONOID"></a>
[`SEMIGROUP :A`](#SEMIGROUP) `=>` [`MONOID`](#MONOID) [`:A`](#:A)

Methods:
- `MEMPTY :: :A`

***

#### `MONAD` <sup><sub>[CLASS]</sub></sup><a name="MONAD"></a>
[`APPLICATIVE :A`](#APPLICATIVE) `=>` [`MONAD`](#MONAD) [`:A`](#:A)

Methods:
- `>>= :: ∀ :B :C. ((:A :B) → (:B → (:A :C)) → (:A :C))`
- `>> :: ∀ :B :C. ((:A :B) → (:A :C) → (:A :C))`

***

#### `SHOW` <sup><sub>[CLASS]</sub></sup><a name="SHOW"></a>
[`SHOW`](#SHOW) [`:A`](#:A)

Methods:
- `SHOW :: (:A → STRING)`

***

#### `INTO` <sup><sub>[CLASS]</sub></sup><a name="INTO"></a>
[`INTO`](#INTO) [`:A`](#:A) [`:B`](#:B)

Methods:
- `INTO :: (:A → :B)`

***

#### `ORD` <sup><sub>[CLASS]</sub></sup><a name="ORD"></a>
[`EQ :A`](#EQ) `=>` [`ORD`](#ORD) [`:A`](#:A)

Methods:
- `<=> :: (:A → :A → ORD)`

***

#### `NUM` <sup><sub>[CLASS]</sub></sup><a name="NUM"></a>
[`EQ :A`](#EQ) [`SHOW :A`](#SHOW) `=>` [`NUM`](#NUM) [`:A`](#:A)

Methods:
- `+ :: (:A → :A → :A)`
- `- :: (:A → :A → :A)`
- `* :: (:A → :A → :A)`
- `FROMINT :: (INT → :A)`

***

#### `EQ` <sup><sub>[CLASS]</sub></sup><a name="EQ"></a>
[`EQ`](#EQ) [`:A`](#:A)

Methods:
- `== :: (:A → :A → BOOLEAN)`
- `/= :: (:A → :A → BOOLEAN)`

***


### Functions

#### `<` <sup><sub>[FUNCTION]</sub></sup><a name="<"></a>
`∀ :A. ORD :A ⇒ (:A → :A → BOOLEAN)`

***

#### `>` <sup><sub>[FUNCTION]</sub></sup><a name=">"></a>
`∀ :A. ORD :A ⇒ (:A → :A → BOOLEAN)`

***

#### `<=` <sup><sub>[FUNCTION]</sub></sup><a name="<="></a>
`∀ :A. ORD :A ⇒ (:A → :A → BOOLEAN)`

***

#### `>=` <sup><sub>[FUNCTION]</sub></sup><a name=">="></a>
`∀ :A. ORD :A ⇒ (:A → :A → BOOLEAN)`

***

#### `MAX` <sup><sub>[FUNCTION]</sub></sup><a name="MAX"></a>
`∀ :A. ORD :A ⇒ (:A → :A → :A)`

***

#### `MIN` <sup><sub>[FUNCTION]</sub></sup><a name="MIN"></a>
`∀ :A. ORD :A ⇒ (:A → :A → :A)`

***


## File: [builtin.lisp](../src/library/builtin.lisp)

### Classes

#### `APPLICATIVE` <sup><sub>[CLASS]</sub></sup><a name="APPLICATIVE"></a>
[`FUNCTOR :A`](#FUNCTOR) `=>` [`APPLICATIVE`](#APPLICATIVE) [`:A`](#:A)

Methods:
- `PURE :: ∀ :B. (:B → (:A :B))`
- `LIFTA2 :: ∀ :B :C :D. ((:B → :C → :D) → (:A :B) → (:A :C) → (:A :D))`

***

#### `SEMIGROUP` <sup><sub>[CLASS]</sub></sup><a name="SEMIGROUP"></a>
[`SEMIGROUP`](#SEMIGROUP) [`:A`](#:A)

Methods:
- `<> :: (:A → :A → :A)`

***

#### `MONADFAIL` <sup><sub>[CLASS]</sub></sup><a name="MONADFAIL"></a>
[`MONAD :A`](#MONAD) `=>` [`MONADFAIL`](#MONADFAIL) [`:A`](#:A)

Methods:
- `FAIL :: ∀ :B. (STRING → (:A :B))`

***

#### `TRYINTO` <sup><sub>[CLASS]</sub></sup><a name="TRYINTO"></a>
[`TRYINTO`](#TRYINTO) [`:A`](#:A) [`:B`](#:B) [`:C`](#:C)

Methods:
- `TRYINTO :: (:A → (RESULT :B :C))`

***

#### `FUNCTOR` <sup><sub>[CLASS]</sub></sup><a name="FUNCTOR"></a>
[`FUNCTOR`](#FUNCTOR) [`:A`](#:A)

Methods:
- `MAP :: ∀ :B :C. ((:B → :C) → (:A :B) → (:A :C))`

***

#### `MONOID` <sup><sub>[CLASS]</sub></sup><a name="MONOID"></a>
[`SEMIGROUP :A`](#SEMIGROUP) `=>` [`MONOID`](#MONOID) [`:A`](#:A)

Methods:
- `MEMPTY :: :A`

***

#### `MONAD` <sup><sub>[CLASS]</sub></sup><a name="MONAD"></a>
[`APPLICATIVE :A`](#APPLICATIVE) `=>` [`MONAD`](#MONAD) [`:A`](#:A)

Methods:
- `>>= :: ∀ :B :C. ((:A :B) → (:B → (:A :C)) → (:A :C))`
- `>> :: ∀ :B :C. ((:A :B) → (:A :C) → (:A :C))`

***

#### `SHOW` <sup><sub>[CLASS]</sub></sup><a name="SHOW"></a>
[`SHOW`](#SHOW) [`:A`](#:A)

Methods:
- `SHOW :: (:A → STRING)`

***

#### `INTO` <sup><sub>[CLASS]</sub></sup><a name="INTO"></a>
[`INTO`](#INTO) [`:A`](#:A) [`:B`](#:B)

Methods:
- `INTO :: (:A → :B)`

***

#### `ORD` <sup><sub>[CLASS]</sub></sup><a name="ORD"></a>
[`EQ :A`](#EQ) `=>` [`ORD`](#ORD) [`:A`](#:A)

Methods:
- `<=> :: (:A → :A → ORD)`

***

#### `NUM` <sup><sub>[CLASS]</sub></sup><a name="NUM"></a>
[`EQ :A`](#EQ) [`SHOW :A`](#SHOW) `=>` [`NUM`](#NUM) [`:A`](#:A)

Methods:
- `+ :: (:A → :A → :A)`
- `- :: (:A → :A → :A)`
- `* :: (:A → :A → :A)`
- `FROMINT :: (INT → :A)`

***

#### `EQ` <sup><sub>[CLASS]</sub></sup><a name="EQ"></a>
[`EQ`](#EQ) [`:A`](#:A)

Methods:
- `== :: (:A → :A → BOOLEAN)`
- `/= :: (:A → :A → BOOLEAN)`

***


### Functions

#### `GCD` <sup><sub>[FUNCTION]</sub></sup><a name="GCD"></a>
`(INT → INT → INT)`

***

#### `LCM` <sup><sub>[FUNCTION]</sub></sup><a name="LCM"></a>
`(INT → INT → INT)`

***

#### `MOD` <sup><sub>[FUNCTION]</sub></sup><a name="MOD"></a>
`(INT → INT → INT)`

***

#### `ODD` <sup><sub>[FUNCTION]</sub></sup><a name="ODD"></a>
`(INT → BOOLEAN)`

***

#### `EVEN` <sup><sub>[FUNCTION]</sub></sup><a name="EVEN"></a>
`(INT → BOOLEAN)`

***

#### `EXPT` <sup><sub>[FUNCTION]</sub></sup><a name="EXPT"></a>
`(INT → INT → INT)`

***


## File: [string.lisp](../src/library/string.lisp)

### Classes

#### `APPLICATIVE` <sup><sub>[CLASS]</sub></sup><a name="APPLICATIVE"></a>
[`FUNCTOR :A`](#FUNCTOR) `=>` [`APPLICATIVE`](#APPLICATIVE) [`:A`](#:A)

Methods:
- `PURE :: ∀ :B. (:B → (:A :B))`
- `LIFTA2 :: ∀ :B :C :D. ((:B → :C → :D) → (:A :B) → (:A :C) → (:A :D))`

***

#### `SEMIGROUP` <sup><sub>[CLASS]</sub></sup><a name="SEMIGROUP"></a>
[`SEMIGROUP`](#SEMIGROUP) [`:A`](#:A)

Methods:
- `<> :: (:A → :A → :A)`

***

#### `MONADFAIL` <sup><sub>[CLASS]</sub></sup><a name="MONADFAIL"></a>
[`MONAD :A`](#MONAD) `=>` [`MONADFAIL`](#MONADFAIL) [`:A`](#:A)

Methods:
- `FAIL :: ∀ :B. (STRING → (:A :B))`

***

#### `TRYINTO` <sup><sub>[CLASS]</sub></sup><a name="TRYINTO"></a>
[`TRYINTO`](#TRYINTO) [`:A`](#:A) [`:B`](#:B) [`:C`](#:C)

Methods:
- `TRYINTO :: (:A → (RESULT :B :C))`

***

#### `FUNCTOR` <sup><sub>[CLASS]</sub></sup><a name="FUNCTOR"></a>
[`FUNCTOR`](#FUNCTOR) [`:A`](#:A)

Methods:
- `MAP :: ∀ :B :C. ((:B → :C) → (:A :B) → (:A :C))`

***

#### `MONOID` <sup><sub>[CLASS]</sub></sup><a name="MONOID"></a>
[`SEMIGROUP :A`](#SEMIGROUP) `=>` [`MONOID`](#MONOID) [`:A`](#:A)

Methods:
- `MEMPTY :: :A`

***

#### `MONAD` <sup><sub>[CLASS]</sub></sup><a name="MONAD"></a>
[`APPLICATIVE :A`](#APPLICATIVE) `=>` [`MONAD`](#MONAD) [`:A`](#:A)

Methods:
- `>>= :: ∀ :B :C. ((:A :B) → (:B → (:A :C)) → (:A :C))`
- `>> :: ∀ :B :C. ((:A :B) → (:A :C) → (:A :C))`

***

#### `SHOW` <sup><sub>[CLASS]</sub></sup><a name="SHOW"></a>
[`SHOW`](#SHOW) [`:A`](#:A)

Methods:
- `SHOW :: (:A → STRING)`

***

#### `INTO` <sup><sub>[CLASS]</sub></sup><a name="INTO"></a>
[`INTO`](#INTO) [`:A`](#:A) [`:B`](#:B)

Methods:
- `INTO :: (:A → :B)`

***

#### `ORD` <sup><sub>[CLASS]</sub></sup><a name="ORD"></a>
[`EQ :A`](#EQ) `=>` [`ORD`](#ORD) [`:A`](#:A)

Methods:
- `<=> :: (:A → :A → ORD)`

***

#### `NUM` <sup><sub>[CLASS]</sub></sup><a name="NUM"></a>
[`EQ :A`](#EQ) [`SHOW :A`](#SHOW) `=>` [`NUM`](#NUM) [`:A`](#:A)

Methods:
- `+ :: (:A → :A → :A)`
- `- :: (:A → :A → :A)`
- `* :: (:A → :A → :A)`
- `FROMINT :: (INT → :A)`

***

#### `EQ` <sup><sub>[CLASS]</sub></sup><a name="EQ"></a>
[`EQ`](#EQ) [`:A`](#:A)

Methods:
- `== :: (:A → :A → BOOLEAN)`
- `/= :: (:A → :A → BOOLEAN)`

***


### Functions

#### `PARSE-INT` <sup><sub>[FUNCTION]</sub></sup><a name="PARSE-INT"></a>
`(STRING → (OPTIONAL INT))`

***

#### `PACK-STRING` <sup><sub>[FUNCTION]</sub></sup><a name="PACK-STRING"></a>
`((LIST CHAR) → STRING)`

***

#### `CONCAT-STRING` <sup><sub>[FUNCTION]</sub></sup><a name="CONCAT-STRING"></a>
`(STRING → STRING → STRING)`

***

#### `UNPACK-STRING` <sup><sub>[FUNCTION]</sub></sup><a name="UNPACK-STRING"></a>
`(STRING → (LIST CHAR))`

***


## File: [optional.lisp](../src/library/optional.lisp)

### Classes

#### `APPLICATIVE` <sup><sub>[CLASS]</sub></sup><a name="APPLICATIVE"></a>
[`FUNCTOR :A`](#FUNCTOR) `=>` [`APPLICATIVE`](#APPLICATIVE) [`:A`](#:A)

Methods:
- `PURE :: ∀ :B. (:B → (:A :B))`
- `LIFTA2 :: ∀ :B :C :D. ((:B → :C → :D) → (:A :B) → (:A :C) → (:A :D))`

***

#### `SEMIGROUP` <sup><sub>[CLASS]</sub></sup><a name="SEMIGROUP"></a>
[`SEMIGROUP`](#SEMIGROUP) [`:A`](#:A)

Methods:
- `<> :: (:A → :A → :A)`

***

#### `MONADFAIL` <sup><sub>[CLASS]</sub></sup><a name="MONADFAIL"></a>
[`MONAD :A`](#MONAD) `=>` [`MONADFAIL`](#MONADFAIL) [`:A`](#:A)

Methods:
- `FAIL :: ∀ :B. (STRING → (:A :B))`

***

#### `TRYINTO` <sup><sub>[CLASS]</sub></sup><a name="TRYINTO"></a>
[`TRYINTO`](#TRYINTO) [`:A`](#:A) [`:B`](#:B) [`:C`](#:C)

Methods:
- `TRYINTO :: (:A → (RESULT :B :C))`

***

#### `FUNCTOR` <sup><sub>[CLASS]</sub></sup><a name="FUNCTOR"></a>
[`FUNCTOR`](#FUNCTOR) [`:A`](#:A)

Methods:
- `MAP :: ∀ :B :C. ((:B → :C) → (:A :B) → (:A :C))`

***

#### `MONOID` <sup><sub>[CLASS]</sub></sup><a name="MONOID"></a>
[`SEMIGROUP :A`](#SEMIGROUP) `=>` [`MONOID`](#MONOID) [`:A`](#:A)

Methods:
- `MEMPTY :: :A`

***

#### `MONAD` <sup><sub>[CLASS]</sub></sup><a name="MONAD"></a>
[`APPLICATIVE :A`](#APPLICATIVE) `=>` [`MONAD`](#MONAD) [`:A`](#:A)

Methods:
- `>>= :: ∀ :B :C. ((:A :B) → (:B → (:A :C)) → (:A :C))`
- `>> :: ∀ :B :C. ((:A :B) → (:A :C) → (:A :C))`

***

#### `SHOW` <sup><sub>[CLASS]</sub></sup><a name="SHOW"></a>
[`SHOW`](#SHOW) [`:A`](#:A)

Methods:
- `SHOW :: (:A → STRING)`

***

#### `INTO` <sup><sub>[CLASS]</sub></sup><a name="INTO"></a>
[`INTO`](#INTO) [`:A`](#:A) [`:B`](#:B)

Methods:
- `INTO :: (:A → :B)`

***

#### `ORD` <sup><sub>[CLASS]</sub></sup><a name="ORD"></a>
[`EQ :A`](#EQ) `=>` [`ORD`](#ORD) [`:A`](#:A)

Methods:
- `<=> :: (:A → :A → ORD)`

***

#### `NUM` <sup><sub>[CLASS]</sub></sup><a name="NUM"></a>
[`EQ :A`](#EQ) [`SHOW :A`](#SHOW) `=>` [`NUM`](#NUM) [`:A`](#:A)

Methods:
- `+ :: (:A → :A → :A)`
- `- :: (:A → :A → :A)`
- `* :: (:A → :A → :A)`
- `FROMINT :: (INT → :A)`

***

#### `EQ` <sup><sub>[CLASS]</sub></sup><a name="EQ"></a>
[`EQ`](#EQ) [`:A`](#:A)

Methods:
- `== :: (:A → :A → BOOLEAN)`
- `/= :: (:A → :A → BOOLEAN)`

***


### Functions

#### `ISNONE` <sup><sub>[FUNCTION]</sub></sup><a name="ISNONE"></a>
`∀ :A. ((OPTIONAL :A) → BOOLEAN)`

***

#### `ISSOME` <sup><sub>[FUNCTION]</sub></sup><a name="ISSOME"></a>
`∀ :A. ((OPTIONAL :A) → BOOLEAN)`

***

#### `FROMSOME` <sup><sub>[FUNCTION]</sub></sup><a name="FROMSOME"></a>
`∀ :A. (STRING → (OPTIONAL :A) → :A)`

***


## File: [list.lisp](../src/library/list.lisp)

### Classes

#### `APPLICATIVE` <sup><sub>[CLASS]</sub></sup><a name="APPLICATIVE"></a>
[`FUNCTOR :A`](#FUNCTOR) `=>` [`APPLICATIVE`](#APPLICATIVE) [`:A`](#:A)

Methods:
- `PURE :: ∀ :B. (:B → (:A :B))`
- `LIFTA2 :: ∀ :B :C :D. ((:B → :C → :D) → (:A :B) → (:A :C) → (:A :D))`

***

#### `SEMIGROUP` <sup><sub>[CLASS]</sub></sup><a name="SEMIGROUP"></a>
[`SEMIGROUP`](#SEMIGROUP) [`:A`](#:A)

Methods:
- `<> :: (:A → :A → :A)`

***

#### `MONADFAIL` <sup><sub>[CLASS]</sub></sup><a name="MONADFAIL"></a>
[`MONAD :A`](#MONAD) `=>` [`MONADFAIL`](#MONADFAIL) [`:A`](#:A)

Methods:
- `FAIL :: ∀ :B. (STRING → (:A :B))`

***

#### `TRYINTO` <sup><sub>[CLASS]</sub></sup><a name="TRYINTO"></a>
[`TRYINTO`](#TRYINTO) [`:A`](#:A) [`:B`](#:B) [`:C`](#:C)

Methods:
- `TRYINTO :: (:A → (RESULT :B :C))`

***

#### `FUNCTOR` <sup><sub>[CLASS]</sub></sup><a name="FUNCTOR"></a>
[`FUNCTOR`](#FUNCTOR) [`:A`](#:A)

Methods:
- `MAP :: ∀ :B :C. ((:B → :C) → (:A :B) → (:A :C))`

***

#### `MONOID` <sup><sub>[CLASS]</sub></sup><a name="MONOID"></a>
[`SEMIGROUP :A`](#SEMIGROUP) `=>` [`MONOID`](#MONOID) [`:A`](#:A)

Methods:
- `MEMPTY :: :A`

***

#### `MONAD` <sup><sub>[CLASS]</sub></sup><a name="MONAD"></a>
[`APPLICATIVE :A`](#APPLICATIVE) `=>` [`MONAD`](#MONAD) [`:A`](#:A)

Methods:
- `>>= :: ∀ :B :C. ((:A :B) → (:B → (:A :C)) → (:A :C))`
- `>> :: ∀ :B :C. ((:A :B) → (:A :C) → (:A :C))`

***

#### `SHOW` <sup><sub>[CLASS]</sub></sup><a name="SHOW"></a>
[`SHOW`](#SHOW) [`:A`](#:A)

Methods:
- `SHOW :: (:A → STRING)`

***

#### `INTO` <sup><sub>[CLASS]</sub></sup><a name="INTO"></a>
[`INTO`](#INTO) [`:A`](#:A) [`:B`](#:B)

Methods:
- `INTO :: (:A → :B)`

***

#### `ORD` <sup><sub>[CLASS]</sub></sup><a name="ORD"></a>
[`EQ :A`](#EQ) `=>` [`ORD`](#ORD) [`:A`](#:A)

Methods:
- `<=> :: (:A → :A → ORD)`

***

#### `NUM` <sup><sub>[CLASS]</sub></sup><a name="NUM"></a>
[`EQ :A`](#EQ) [`SHOW :A`](#SHOW) `=>` [`NUM`](#NUM) [`:A`](#:A)

Methods:
- `+ :: (:A → :A → :A)`
- `- :: (:A → :A → :A)`
- `* :: (:A → :A → :A)`
- `FROMINT :: (INT → :A)`

***

#### `EQ` <sup><sub>[CLASS]</sub></sup><a name="EQ"></a>
[`EQ`](#EQ) [`:A`](#:A)

Methods:
- `== :: (:A → :A → BOOLEAN)`
- `/= :: (:A → :A → BOOLEAN)`

***


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

#### `FOLDR` <sup><sub>[FUNCTION]</sub></sup><a name="FOLDR"></a>
`∀ :A :B. ((:A → :B → :B) → :B → (LIST :A) → :B)`

Right fold on lists. Is short circuiting but is not tail recursive.


***

#### `INDEX` <sup><sub>[FUNCTION]</sub></sup><a name="INDEX"></a>
`∀ :A. ((LIST :A) → INT → (OPTIONAL :A))`

Returns the Ith element of XS.


***

#### `RANGE` <sup><sub>[FUNCTION]</sub></sup><a name="RANGE"></a>
`(INT → INT → (LIST INT))`

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
`∀ :A. ((LIST :A) → INT)`

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
`∀ :A. (INT → :A → (LIST :A))`

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

#### `CONCATMAP` <sup><sub>[FUNCTION]</sub></sup><a name="CONCATMAP"></a>
`∀ :A :B. ((:A → (LIST :B)) → (LIST :A) → (LIST :B))`

Apply F to each element in XS and concatenate the results.


***

#### `ELEMINDEX` <sup><sub>[FUNCTION]</sub></sup><a name="ELEMINDEX"></a>
`∀ :A. EQ :A ⇒ (:A → (LIST :A) → (OPTIONAL INT))`

***

#### `FINDINDEX` <sup><sub>[FUNCTION]</sub></sup><a name="FINDINDEX"></a>
`∀ :A. ((:A → BOOLEAN) → (LIST :A) → (OPTIONAL INT))`

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

### Classes

#### `APPLICATIVE` <sup><sub>[CLASS]</sub></sup><a name="APPLICATIVE"></a>
[`FUNCTOR :A`](#FUNCTOR) `=>` [`APPLICATIVE`](#APPLICATIVE) [`:A`](#:A)

Methods:
- `PURE :: ∀ :B. (:B → (:A :B))`
- `LIFTA2 :: ∀ :B :C :D. ((:B → :C → :D) → (:A :B) → (:A :C) → (:A :D))`

***

#### `SEMIGROUP` <sup><sub>[CLASS]</sub></sup><a name="SEMIGROUP"></a>
[`SEMIGROUP`](#SEMIGROUP) [`:A`](#:A)

Methods:
- `<> :: (:A → :A → :A)`

***

#### `MONADFAIL` <sup><sub>[CLASS]</sub></sup><a name="MONADFAIL"></a>
[`MONAD :A`](#MONAD) `=>` [`MONADFAIL`](#MONADFAIL) [`:A`](#:A)

Methods:
- `FAIL :: ∀ :B. (STRING → (:A :B))`

***

#### `TRYINTO` <sup><sub>[CLASS]</sub></sup><a name="TRYINTO"></a>
[`TRYINTO`](#TRYINTO) [`:A`](#:A) [`:B`](#:B) [`:C`](#:C)

Methods:
- `TRYINTO :: (:A → (RESULT :B :C))`

***

#### `FUNCTOR` <sup><sub>[CLASS]</sub></sup><a name="FUNCTOR"></a>
[`FUNCTOR`](#FUNCTOR) [`:A`](#:A)

Methods:
- `MAP :: ∀ :B :C. ((:B → :C) → (:A :B) → (:A :C))`

***

#### `MONOID` <sup><sub>[CLASS]</sub></sup><a name="MONOID"></a>
[`SEMIGROUP :A`](#SEMIGROUP) `=>` [`MONOID`](#MONOID) [`:A`](#:A)

Methods:
- `MEMPTY :: :A`

***

#### `MONAD` <sup><sub>[CLASS]</sub></sup><a name="MONAD"></a>
[`APPLICATIVE :A`](#APPLICATIVE) `=>` [`MONAD`](#MONAD) [`:A`](#:A)

Methods:
- `>>= :: ∀ :B :C. ((:A :B) → (:B → (:A :C)) → (:A :C))`
- `>> :: ∀ :B :C. ((:A :B) → (:A :C) → (:A :C))`

***

#### `SHOW` <sup><sub>[CLASS]</sub></sup><a name="SHOW"></a>
[`SHOW`](#SHOW) [`:A`](#:A)

Methods:
- `SHOW :: (:A → STRING)`

***

#### `INTO` <sup><sub>[CLASS]</sub></sup><a name="INTO"></a>
[`INTO`](#INTO) [`:A`](#:A) [`:B`](#:B)

Methods:
- `INTO :: (:A → :B)`

***

#### `ORD` <sup><sub>[CLASS]</sub></sup><a name="ORD"></a>
[`EQ :A`](#EQ) `=>` [`ORD`](#ORD) [`:A`](#:A)

Methods:
- `<=> :: (:A → :A → ORD)`

***

#### `NUM` <sup><sub>[CLASS]</sub></sup><a name="NUM"></a>
[`EQ :A`](#EQ) [`SHOW :A`](#SHOW) `=>` [`NUM`](#NUM) [`:A`](#:A)

Methods:
- `+ :: (:A → :A → :A)`
- `- :: (:A → :A → :A)`
- `* :: (:A → :A → :A)`
- `FROMINT :: (INT → :A)`

***

#### `EQ` <sup><sub>[CLASS]</sub></sup><a name="EQ"></a>
[`EQ`](#EQ) [`:A`](#:A)

Methods:
- `== :: (:A → :A → BOOLEAN)`
- `/= :: (:A → :A → BOOLEAN)`

***


### Functions

#### `FST` <sup><sub>[FUNCTION]</sub></sup><a name="FST"></a>
`∀ :A :B. ((TUPLE :A :B) → :A)`

***

#### `SND` <sup><sub>[FUNCTION]</sub></sup><a name="SND"></a>
`∀ :A :B. ((TUPLE :A :B) → :B)`

***


## File: [result.lisp](../src/library/result.lisp)

### Classes

#### `APPLICATIVE` <sup><sub>[CLASS]</sub></sup><a name="APPLICATIVE"></a>
[`FUNCTOR :A`](#FUNCTOR) `=>` [`APPLICATIVE`](#APPLICATIVE) [`:A`](#:A)

Methods:
- `PURE :: ∀ :B. (:B → (:A :B))`
- `LIFTA2 :: ∀ :B :C :D. ((:B → :C → :D) → (:A :B) → (:A :C) → (:A :D))`

***

#### `SEMIGROUP` <sup><sub>[CLASS]</sub></sup><a name="SEMIGROUP"></a>
[`SEMIGROUP`](#SEMIGROUP) [`:A`](#:A)

Methods:
- `<> :: (:A → :A → :A)`

***

#### `MONADFAIL` <sup><sub>[CLASS]</sub></sup><a name="MONADFAIL"></a>
[`MONAD :A`](#MONAD) `=>` [`MONADFAIL`](#MONADFAIL) [`:A`](#:A)

Methods:
- `FAIL :: ∀ :B. (STRING → (:A :B))`

***

#### `TRYINTO` <sup><sub>[CLASS]</sub></sup><a name="TRYINTO"></a>
[`TRYINTO`](#TRYINTO) [`:A`](#:A) [`:B`](#:B) [`:C`](#:C)

Methods:
- `TRYINTO :: (:A → (RESULT :B :C))`

***

#### `FUNCTOR` <sup><sub>[CLASS]</sub></sup><a name="FUNCTOR"></a>
[`FUNCTOR`](#FUNCTOR) [`:A`](#:A)

Methods:
- `MAP :: ∀ :B :C. ((:B → :C) → (:A :B) → (:A :C))`

***

#### `MONOID` <sup><sub>[CLASS]</sub></sup><a name="MONOID"></a>
[`SEMIGROUP :A`](#SEMIGROUP) `=>` [`MONOID`](#MONOID) [`:A`](#:A)

Methods:
- `MEMPTY :: :A`

***

#### `MONAD` <sup><sub>[CLASS]</sub></sup><a name="MONAD"></a>
[`APPLICATIVE :A`](#APPLICATIVE) `=>` [`MONAD`](#MONAD) [`:A`](#:A)

Methods:
- `>>= :: ∀ :B :C. ((:A :B) → (:B → (:A :C)) → (:A :C))`
- `>> :: ∀ :B :C. ((:A :B) → (:A :C) → (:A :C))`

***

#### `SHOW` <sup><sub>[CLASS]</sub></sup><a name="SHOW"></a>
[`SHOW`](#SHOW) [`:A`](#:A)

Methods:
- `SHOW :: (:A → STRING)`

***

#### `INTO` <sup><sub>[CLASS]</sub></sup><a name="INTO"></a>
[`INTO`](#INTO) [`:A`](#:A) [`:B`](#:B)

Methods:
- `INTO :: (:A → :B)`

***

#### `ORD` <sup><sub>[CLASS]</sub></sup><a name="ORD"></a>
[`EQ :A`](#EQ) `=>` [`ORD`](#ORD) [`:A`](#:A)

Methods:
- `<=> :: (:A → :A → ORD)`

***

#### `NUM` <sup><sub>[CLASS]</sub></sup><a name="NUM"></a>
[`EQ :A`](#EQ) [`SHOW :A`](#SHOW) `=>` [`NUM`](#NUM) [`:A`](#:A)

Methods:
- `+ :: (:A → :A → :A)`
- `- :: (:A → :A → :A)`
- `* :: (:A → :A → :A)`
- `FROMINT :: (INT → :A)`

***

#### `EQ` <sup><sub>[CLASS]</sub></sup><a name="EQ"></a>
[`EQ`](#EQ) [`:A`](#:A)

Methods:
- `== :: (:A → :A → BOOLEAN)`
- `/= :: (:A → :A → BOOLEAN)`

***


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

### Classes

#### `APPLICATIVE` <sup><sub>[CLASS]</sub></sup><a name="APPLICATIVE"></a>
[`FUNCTOR :A`](#FUNCTOR) `=>` [`APPLICATIVE`](#APPLICATIVE) [`:A`](#:A)

Methods:
- `PURE :: ∀ :B. (:B → (:A :B))`
- `LIFTA2 :: ∀ :B :C :D. ((:B → :C → :D) → (:A :B) → (:A :C) → (:A :D))`

***

#### `SEMIGROUP` <sup><sub>[CLASS]</sub></sup><a name="SEMIGROUP"></a>
[`SEMIGROUP`](#SEMIGROUP) [`:A`](#:A)

Methods:
- `<> :: (:A → :A → :A)`

***

#### `MONADFAIL` <sup><sub>[CLASS]</sub></sup><a name="MONADFAIL"></a>
[`MONAD :A`](#MONAD) `=>` [`MONADFAIL`](#MONADFAIL) [`:A`](#:A)

Methods:
- `FAIL :: ∀ :B. (STRING → (:A :B))`

***

#### `TRYINTO` <sup><sub>[CLASS]</sub></sup><a name="TRYINTO"></a>
[`TRYINTO`](#TRYINTO) [`:A`](#:A) [`:B`](#:B) [`:C`](#:C)

Methods:
- `TRYINTO :: (:A → (RESULT :B :C))`

***

#### `FUNCTOR` <sup><sub>[CLASS]</sub></sup><a name="FUNCTOR"></a>
[`FUNCTOR`](#FUNCTOR) [`:A`](#:A)

Methods:
- `MAP :: ∀ :B :C. ((:B → :C) → (:A :B) → (:A :C))`

***

#### `MONOID` <sup><sub>[CLASS]</sub></sup><a name="MONOID"></a>
[`SEMIGROUP :A`](#SEMIGROUP) `=>` [`MONOID`](#MONOID) [`:A`](#:A)

Methods:
- `MEMPTY :: :A`

***

#### `MONAD` <sup><sub>[CLASS]</sub></sup><a name="MONAD"></a>
[`APPLICATIVE :A`](#APPLICATIVE) `=>` [`MONAD`](#MONAD) [`:A`](#:A)

Methods:
- `>>= :: ∀ :B :C. ((:A :B) → (:B → (:A :C)) → (:A :C))`
- `>> :: ∀ :B :C. ((:A :B) → (:A :C) → (:A :C))`

***

#### `SHOW` <sup><sub>[CLASS]</sub></sup><a name="SHOW"></a>
[`SHOW`](#SHOW) [`:A`](#:A)

Methods:
- `SHOW :: (:A → STRING)`

***

#### `INTO` <sup><sub>[CLASS]</sub></sup><a name="INTO"></a>
[`INTO`](#INTO) [`:A`](#:A) [`:B`](#:B)

Methods:
- `INTO :: (:A → :B)`

***

#### `ORD` <sup><sub>[CLASS]</sub></sup><a name="ORD"></a>
[`EQ :A`](#EQ) `=>` [`ORD`](#ORD) [`:A`](#:A)

Methods:
- `<=> :: (:A → :A → ORD)`

***

#### `NUM` <sup><sub>[CLASS]</sub></sup><a name="NUM"></a>
[`EQ :A`](#EQ) [`SHOW :A`](#SHOW) `=>` [`NUM`](#NUM) [`:A`](#:A)

Methods:
- `+ :: (:A → :A → :A)`
- `- :: (:A → :A → :A)`
- `* :: (:A → :A → :A)`
- `FROMINT :: (INT → :A)`

***

#### `EQ` <sup><sub>[CLASS]</sub></sup><a name="EQ"></a>
[`EQ`](#EQ) [`:A`](#:A)

Methods:
- `== :: (:A → :A → BOOLEAN)`
- `/= :: (:A → :A → BOOLEAN)`

***


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


## File: [graph.lisp](../src/library/graph.lisp)

### Types

#### `GRAPH :A :B` <sup><sub>[TYPE]</sub></sup><a name="GRAPH"></a>
- `(GRAPH GRAPHTYPE (VECTOR (NODE :A)) (VECTOR (EDGE :B)))`

A graph using adjacency list representation

Constructors:
- `GRAPH :: (GRAPHTYPE → (VECTOR (NODE :A)) → (VECTOR (EDGE :B)) → (GRAPH :A :B))`

***

#### `EDGEINDEX`<sup><sub>[TYPE]</sub></sup><a name="EDGEINDEX"></a>
- `(EDGEINDEX INT)`

Constructors:
- `EDGEINDEX :: (INT → EDGEINDEX)`

<details>
<summary>Instances</summary>

- [`EQ`](#EQ) [`EDGEINDEX`](#EDGEINDEX)
- [`INTO`](#INTO) [`EDGEINDEX`](#EDGEINDEX) [`INT`](#INT)
- [`SHOW`](#SHOW) [`EDGEINDEX`](#EDGEINDEX)

</details>

***

#### `GRAPHTYPE`<sup><sub>[TYPE]</sub></sup><a name="GRAPHTYPE"></a>
- `UNDIRECTED`
- `DIRECTED`

Constructors:
- `UNDIRECTED :: GRAPHTYPE`
- `DIRECTED :: GRAPHTYPE`

***

#### `NODEINDEX`<sup><sub>[TYPE]</sub></sup><a name="NODEINDEX"></a>
- `(NODEINDEX INT)`

Constructors:
- `NODEINDEX :: (INT → NODEINDEX)`

<details>
<summary>Instances</summary>

- [`EQ`](#EQ) [`NODEINDEX`](#NODEINDEX)
- [`INTO`](#INTO) [`NODEINDEX`](#NODEINDEX) [`INT`](#INT)
- [`SHOW`](#SHOW) [`NODEINDEX`](#NODEINDEX)

</details>

***

### Classes

#### `APPLICATIVE` <sup><sub>[CLASS]</sub></sup><a name="APPLICATIVE"></a>
[`FUNCTOR :A`](#FUNCTOR) `=>` [`APPLICATIVE`](#APPLICATIVE) [`:A`](#:A)

Methods:
- `PURE :: ∀ :B. (:B → (:A :B))`
- `LIFTA2 :: ∀ :B :C :D. ((:B → :C → :D) → (:A :B) → (:A :C) → (:A :D))`

***

#### `SEMIGROUP` <sup><sub>[CLASS]</sub></sup><a name="SEMIGROUP"></a>
[`SEMIGROUP`](#SEMIGROUP) [`:A`](#:A)

Methods:
- `<> :: (:A → :A → :A)`

***

#### `MONADFAIL` <sup><sub>[CLASS]</sub></sup><a name="MONADFAIL"></a>
[`MONAD :A`](#MONAD) `=>` [`MONADFAIL`](#MONADFAIL) [`:A`](#:A)

Methods:
- `FAIL :: ∀ :B. (STRING → (:A :B))`

***

#### `TRYINTO` <sup><sub>[CLASS]</sub></sup><a name="TRYINTO"></a>
[`TRYINTO`](#TRYINTO) [`:A`](#:A) [`:B`](#:B) [`:C`](#:C)

Methods:
- `TRYINTO :: (:A → (RESULT :B :C))`

***

#### `FUNCTOR` <sup><sub>[CLASS]</sub></sup><a name="FUNCTOR"></a>
[`FUNCTOR`](#FUNCTOR) [`:A`](#:A)

Methods:
- `MAP :: ∀ :B :C. ((:B → :C) → (:A :B) → (:A :C))`

***

#### `MONOID` <sup><sub>[CLASS]</sub></sup><a name="MONOID"></a>
[`SEMIGROUP :A`](#SEMIGROUP) `=>` [`MONOID`](#MONOID) [`:A`](#:A)

Methods:
- `MEMPTY :: :A`

***

#### `MONAD` <sup><sub>[CLASS]</sub></sup><a name="MONAD"></a>
[`APPLICATIVE :A`](#APPLICATIVE) `=>` [`MONAD`](#MONAD) [`:A`](#:A)

Methods:
- `>>= :: ∀ :B :C. ((:A :B) → (:B → (:A :C)) → (:A :C))`
- `>> :: ∀ :B :C. ((:A :B) → (:A :C) → (:A :C))`

***

#### `SHOW` <sup><sub>[CLASS]</sub></sup><a name="SHOW"></a>
[`SHOW`](#SHOW) [`:A`](#:A)

Methods:
- `SHOW :: (:A → STRING)`

***

#### `INTO` <sup><sub>[CLASS]</sub></sup><a name="INTO"></a>
[`INTO`](#INTO) [`:A`](#:A) [`:B`](#:B)

Methods:
- `INTO :: (:A → :B)`

***

#### `ORD` <sup><sub>[CLASS]</sub></sup><a name="ORD"></a>
[`EQ :A`](#EQ) `=>` [`ORD`](#ORD) [`:A`](#:A)

Methods:
- `<=> :: (:A → :A → ORD)`

***

#### `NUM` <sup><sub>[CLASS]</sub></sup><a name="NUM"></a>
[`EQ :A`](#EQ) [`SHOW :A`](#SHOW) `=>` [`NUM`](#NUM) [`:A`](#:A)

Methods:
- `+ :: (:A → :A → :A)`
- `- :: (:A → :A → :A)`
- `* :: (:A → :A → :A)`
- `FROMINT :: (INT → :A)`

***

#### `EQ` <sup><sub>[CLASS]</sub></sup><a name="EQ"></a>
[`EQ`](#EQ) [`:A`](#:A)

Methods:
- `== :: (:A → :A → BOOLEAN)`
- `/= :: (:A → :A → BOOLEAN)`

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
`∀ :A :B. ((GRAPH :A :B) → INT)`

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


