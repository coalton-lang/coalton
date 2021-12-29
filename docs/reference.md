# Reference for COALTON

### Types

#### <code>I8</code> <sup><sub>[TYPE]</sub></sup><a name="i8-type"></a>

Signed 8-bit integer capable of storing values in `[-128, 127]`. Uses `(signed-byte 8)`.


<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#i8-type">I8</a></code>
- <code><a href="#num-class">NUM</a> <a href="#i8-type">I8</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#i8-type">I8</a></code>
- <code><a href="#into-class">INTO</a> <a href="#i8-type">I8</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#i8-type">I8</a></code>

</details>


***

#### <code>U8</code> <sup><sub>[TYPE]</sub></sup><a name="u8-type"></a>

Unsigned 8-bit integer capable of storing values in `[0, 255]`. Uses `(unsigned-byte 8)`.


<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#u8-type">U8</a></code>
- <code><a href="#num-class">NUM</a> <a href="#u8-type">U8</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#u8-type">U8</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u8-type">U8</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u8-type">U8</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u8-type">U8</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#u8-type">U8</a></code>
- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#u8-type">U8</a></code>

</details>


***

#### <code>I16</code> <sup><sub>[TYPE]</sub></sup><a name="i16-type"></a>

Signed 16-bit integer capable of storing values in `[-32768, 32767]`. Uses `(signed-byte 16)`.


<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#i16-type">I16</a></code>
- <code><a href="#num-class">NUM</a> <a href="#i16-type">I16</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#i16-type">I16</a></code>
- <code><a href="#into-class">INTO</a> <a href="#i16-type">I16</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#i16-type">I16</a></code>

</details>


***

#### <code>I32</code> <sup><sub>[TYPE]</sub></sup><a name="i32-type"></a>

Signed 32-bit integer capable of storing values in `[-2147483648, 2147483647]`. Uses `(signed-byte 32)`.


<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#i32-type">I32</a></code>
- <code><a href="#num-class">NUM</a> <a href="#i32-type">I32</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#i32-type">I32</a></code>
- <code><a href="#into-class">INTO</a> <a href="#i32-type">I32</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#i32-type">I32</a></code>
- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#i32-type">I32</a></code>

</details>


***

#### <code>I64</code> <sup><sub>[TYPE]</sub></sup><a name="i64-type"></a>

Signed 64-bit integer capable of storing values in `[-9223372036854775808, 9223372036854775807]`. Uses `(signed-byte 64)`.


<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#i64-type">I64</a></code>
- <code><a href="#num-class">NUM</a> <a href="#i64-type">I64</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#i64-type">I64</a></code>
- <code><a href="#into-class">INTO</a> <a href="#i64-type">I64</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#i64-type">I64</a></code>
- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#i64-type">I64</a></code>

</details>


***

#### <code>U16</code> <sup><sub>[TYPE]</sub></sup><a name="u16-type"></a>

Unsigned 16-bit integer capable of storing values in `[0, 65535]`. Uses `(unsigned-byte 16)`.


<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#u16-type">U16</a></code>
- <code><a href="#num-class">NUM</a> <a href="#u16-type">U16</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#u16-type">U16</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u16-type">U16</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u16-type">U16</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u16-type">U16</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#u16-type">U16</a></code>

</details>


***

#### <code>U32</code> <sup><sub>[TYPE]</sub></sup><a name="u32-type"></a>

Unsigned 32-bit integer capable of storing values in `[0, 4294967295]`. Uses `(unsigned-byte 32)`.


<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#u32-type">U32</a></code>
- <code><a href="#num-class">NUM</a> <a href="#u32-type">U32</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#u32-type">U32</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u32-type">U32</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u32-type">U32</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u32-type">U32</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#u32-type">U32</a></code>
- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#u32-type">U32</a></code>

</details>


***

#### <code>U64</code> <sup><sub>[TYPE]</sub></sup><a name="u64-type"></a>

Unsigned 64-bit integer capable of storing values in `[0, 18446744073709551615]`. Uses `(unsigned-byte 64)`.


<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#u64-type">U64</a></code>
- <code><a href="#num-class">NUM</a> <a href="#u64-type">U64</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#u64-type">U64</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u64-type">U64</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u64-type">U64</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u64-type">U64</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#u64-type">U64</a></code>
- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#u64-type">U64</a></code>

</details>


***

#### <code>CHAR</code> <sup><sub>[TYPE]</sub></sup><a name="char-type"></a>

A single character represented as a `character` type.


<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#char-type">CHAR</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#char-type">CHAR</a></code>

</details>


***

#### <code>ARROW :A :B</code> <sup><sub>[TYPE]</sub></sup><a name="arrow-type"></a>

Type constructor for function types. `(Arrow :a :b)` is equivalent to `(:a -&gt; :b)`.



***

#### <code>STRING</code> <sup><sub>[TYPE]</sub></sup><a name="string-type"></a>

String of characters represented by Common Lisp `string`.


<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#string-type">STRING</a></code>
- <code><a href="#iso-class">ISO</a> (<a href="#list-type">LIST</a> <a href="#char-type">CHAR</a>) <a href="#string-type">STRING</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#string-type">STRING</a></code>
- <code><a href="#into-class">INTO</a> (<a href="#list-type">LIST</a> <a href="#char-type">CHAR</a>) <a href="#string-type">STRING</a></code>
- <code><a href="#into-class">INTO</a> <a href="#string-type">STRING</a> (<a href="#list-type">LIST</a> <a href="#char-type">CHAR</a>)</code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#string-type">STRING</a></code>
- <code><a href="#monoid-class">MONOID</a> <a href="#string-type">STRING</a></code>
- <code><a href="#tryinto-class">TRYINTO</a> <a href="#string-type">STRING</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#semigroup-class">SEMIGROUP</a> <a href="#string-type">STRING</a></code>

</details>


***

#### <code>INTEGER</code> <sup><sub>[TYPE]</sub></sup><a name="integer-type"></a>

Unbound integer. Uses `integer`.


<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#num-class">NUM</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#nodeindex-type">NODEINDEX</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#edgeindex-type">EDGEINDEX</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#string-type">STRING</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u64-type">U64</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#u64-type">U64</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u32-type">U32</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#u32-type">U32</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u16-type">U16</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#u16-type">U16</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u8-type">U8</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#u8-type">U8</a></code>
- <code><a href="#into-class">INTO</a> <a href="#i64-type">I64</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#i64-type">I64</a></code>
- <code><a href="#into-class">INTO</a> <a href="#i32-type">I32</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#i32-type">I32</a></code>
- <code><a href="#into-class">INTO</a> <a href="#i16-type">I16</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#i16-type">I16</a></code>
- <code><a href="#into-class">INTO</a> <a href="#i8-type">I8</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#i8-type">I8</a></code>
- <code><a href="#tryinto-class">TRYINTO</a> <a href="#string-type">STRING</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#dividable-class">DIVIDABLE</a> <a href="#integer-type">INTEGER</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#dividable-class">DIVIDABLE</a> <a href="#integer-type">INTEGER</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#dividable-class">DIVIDABLE</a> <a href="#integer-type">INTEGER</a> <a href="#fraction-type">FRACTION</a></code>
- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#integer-type">INTEGER</a></code>

</details>


***

#### <code>LISP-OBJECT</code> <sup><sub>[TYPE]</sub></sup><a name="lisp-object-type"></a>

Opaque container for arbitrary lisp objects. At runtime this is equivalent to the type `t`.



***

#### <code>DOUBLE-FLOAT</code> <sup><sub>[TYPE]</sub></sup><a name="double-float-type"></a>

Double precision floating point numer. Uses `double-float`.


<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#num-class">NUM</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u64-type">U64</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u32-type">U32</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u16-type">U16</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u8-type">U8</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#dividable-class">DIVIDABLE</a> <a href="#integer-type">INTEGER</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#dividable-class">DIVIDABLE</a> <a href="#double-float-type">DOUBLE-FLOAT</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>

</details>


***

#### <code>SINGLE-FLOAT</code> <sup><sub>[TYPE]</sub></sup><a name="single-float-type"></a>

Single precision floating point numer. Uses `single-float`.


<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#num-class">NUM</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u64-type">U64</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u32-type">U32</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u16-type">U16</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u8-type">U8</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#dividable-class">DIVIDABLE</a> <a href="#integer-type">INTEGER</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#dividable-class">DIVIDABLE</a> <a href="#single-float-type">SINGLE-FLOAT</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>

</details>


***

## File: [types.lisp](../src/library/types.lisp)

### Types

#### <code>LIST :A</code> <sup><sub>[TYPE]</sub></sup><a name="list-type"></a>
- <code>CONS</code>
- <code>NIL</code>

Homogeneous list of objects represented as a Common Lisp `list`.

Constructors:
- <code>CONS :: (<a href="#list-type">LIST</a> :B)</code>
- <code>NIL :: (<a href="#list-type">LIST</a> :B)</code>

<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> :A &rArr; <a href="#eq-class">EQ</a> (<a href="#list-type">LIST</a> :A)</code>
- <code><a href="#iso-class">ISO</a> (<a href="#vector-type">VECTOR</a> :A) (<a href="#list-type">LIST</a> :A)</code>
- <code><a href="#iso-class">ISO</a> (<a href="#list-type">LIST</a> <a href="#char-type">CHAR</a>) <a href="#string-type">STRING</a></code>
- <code><a href="#into-class">INTO</a> (<a href="#vector-type">VECTOR</a> :A) (<a href="#list-type">LIST</a> :A)</code>
- <code><a href="#into-class">INTO</a> (<a href="#list-type">LIST</a> :A) (<a href="#vector-type">VECTOR</a> :A)</code>
- <code><a href="#into-class">INTO</a> (<a href="#list-type">LIST</a> <a href="#char-type">CHAR</a>) <a href="#string-type">STRING</a></code>
- <code><a href="#into-class">INTO</a> <a href="#string-type">STRING</a> (<a href="#list-type">LIST</a> <a href="#char-type">CHAR</a>)</code>
- <code><a href="#monad-class">MONAD</a> <a href="#list-type">LIST</a></code>
- <code><a href="#monoid-class">MONOID</a> (<a href="#list-type">LIST</a> :A)</code>
- <code><a href="#functor-class">FUNCTOR</a> <a href="#list-type">LIST</a></code>
- <code><a href="#semigroup-class">SEMIGROUP</a> (<a href="#list-type">LIST</a> :A)</code>
- <code><a href="#alternative-class">ALTERNATIVE</a> <a href="#list-type">LIST</a></code>
- <code><a href="#applicative-class">APPLICATIVE</a> <a href="#list-type">LIST</a></code>

</details>


***

#### <code>UNIT</code> <sup><sub>[TYPE]</sub></sup><a name="unit-type"></a>
- <code>UNIT</code>

Constructors:
- <code>UNIT :: <a href="#unit-type">UNIT</a></code>


***

#### <code>BOOLEAN</code> <sup><sub>[TYPE]</sub></sup><a name="boolean-type"></a>
- <code>FALSE</code>
- <code>TRUE</code>

Either true or false represented by `t` and `nil` respectively.

Constructors:
- <code>FALSE :: <a href="#boolean-type">BOOLEAN</a></code>
- <code>TRUE :: <a href="#boolean-type">BOOLEAN</a></code>

<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#boolean-type">BOOLEAN</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#boolean-type">BOOLEAN</a></code>

</details>


***

### Values

#### <code>NIL</code> <sup><sub>[VALUE]</sub></sup><a name="nil-function"></a>
<code>&forall; :A. (<a href="#list-type">LIST</a> :A)</code>

***

#### <code>CONS</code> <sup><sub>[FUNCTION]</sub></sup><a name="cons-function"></a>
<code>&forall; :A. (:A &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

***

#### <code>TRUE</code> <sup><sub>[VALUE]</sub></sup><a name="true-function"></a>
<code><a href="#boolean-type">BOOLEAN</a></code>

***

#### <code>FALSE</code> <sup><sub>[VALUE]</sub></sup><a name="false-function"></a>
<code><a href="#boolean-type">BOOLEAN</a></code>

***

# Reference for COALTON-LIBRARY

## File: [types.lisp](../src/library/types.lisp)

### Types

#### <code>TUPLE :A :B</code> <sup><sub>[TYPE]</sub></sup><a name="tuple-type"></a>
- <code>(TUPLE :A :B)</code>

A heterogeneous collection of items.

Constructors:
- <code>TUPLE :: (:A &rarr; :B &rarr; (<a href="#tuple-type">TUPLE</a> :A :B))</code>

<details>
<summary>Instances</summary>

- <code>(<a href="#eq-class">EQ</a> :A) (<a href="#eq-class">EQ</a> :B) &rArr; <a href="#eq-class">EQ</a> (<a href="#tuple-type">TUPLE</a> :A :B)</code>
- <code><a href="#iso-class">ISO</a> (<a href="#tuple-type">TUPLE</a> :A :B) (<a href="#tuple-type">TUPLE</a> :B :A)</code>
- <code>(<a href="#ord-class">ORD</a> :A) (<a href="#ord-class">ORD</a> :B) &rArr; <a href="#ord-class">ORD</a> (<a href="#tuple-type">TUPLE</a> :A :B)</code>
- <code><a href="#into-class">INTO</a> (<a href="#tuple-type">TUPLE</a> :A :B) (<a href="#tuple-type">TUPLE</a> :B :A)</code>

</details>


***

#### <code>RESULT :A :B</code> <sup><sub>[TYPE]</sub></sup><a name="result-type"></a>
- <code>(ERR :A)</code>
- <code>(OK :B)</code>

Represents something that may have failed.

Constructors:
- <code>ERR :: (:A &rarr; (<a href="#result-type">RESULT</a> :A :B))</code>
- <code>OK :: (:B &rarr; (<a href="#result-type">RESULT</a> :A :B))</code>

<details>
<summary>Instances</summary>

- <code>(<a href="#eq-class">EQ</a> :A) (<a href="#eq-class">EQ</a> :B) &rArr; <a href="#eq-class">EQ</a> (<a href="#result-type">RESULT</a> :A :B)</code>
- <code><a href="#iso-class">ISO</a> (<a href="#result-type">RESULT</a> <a href="#unit-type">UNIT</a> :A) (<a href="#optional-type">OPTIONAL</a> :A)</code>
- <code>(<a href="#ord-class">ORD</a> :A) (<a href="#ord-class">ORD</a> :B) &rArr; <a href="#ord-class">ORD</a> (<a href="#result-type">RESULT</a> :A :B)</code>
- <code><a href="#into-class">INTO</a> (<a href="#optional-type">OPTIONAL</a> :A) (<a href="#result-type">RESULT</a> <a href="#unit-type">UNIT</a> :A)</code>
- <code><a href="#into-class">INTO</a> (<a href="#result-type">RESULT</a> :A :B) (<a href="#optional-type">OPTIONAL</a> :B)</code>
- <code><a href="#monad-class">MONAD</a> (<a href="#result-type">RESULT</a> :A)</code>
- <code><a href="#monoid-class">MONOID</a> :A &rArr; <a href="#monoid-class">MONOID</a> (<a href="#result-type">RESULT</a> :B :A)</code>
- <code><a href="#functor-class">FUNCTOR</a> (<a href="#result-type">RESULT</a> :A)</code>
- <code><a href="#semigroup-class">SEMIGROUP</a> :A &rArr; <a href="#semigroup-class">SEMIGROUP</a> (<a href="#result-type">RESULT</a> :B :A)</code>
- <code><a href="#applicative-class">APPLICATIVE</a> (<a href="#result-type">RESULT</a> :A)</code>
- <code><a href="#unwrappable-class">UNWRAPPABLE</a> (<a href="#result-type">RESULT</a> :A)</code>

</details>


***

#### <code>TUPLE3 :A :B :C</code> <sup><sub>[TYPE]</sub></sup><a name="tuple3-type"></a>
- <code>(TUPLE3 :A :B :C)</code>

Constructors:
- <code>TUPLE3 :: (:A &rarr; :B &rarr; :C &rarr; (<a href="#tuple3-type">TUPLE3</a> :A :B :C))</code>

<details>
<summary>Instances</summary>

- <code>(<a href="#eq-class">EQ</a> :A) (<a href="#eq-class">EQ</a> :B) (<a href="#eq-class">EQ</a> :C) &rArr; <a href="#eq-class">EQ</a> (<a href="#tuple3-type">TUPLE3</a> :A :B :C)</code>

</details>


***

#### <code>TUPLE4 :A :B :C :D</code> <sup><sub>[TYPE]</sub></sup><a name="tuple4-type"></a>
- <code>(TUPLE4 :A :B :C :D)</code>

Constructors:
- <code>TUPLE4 :: (:A &rarr; :B &rarr; :C &rarr; :D &rarr; (<a href="#tuple4-type">TUPLE4</a> :A :B :C :D))</code>

<details>
<summary>Instances</summary>

- <code>(<a href="#eq-class">EQ</a> :A) (<a href="#eq-class">EQ</a> :B) (<a href="#eq-class">EQ</a> :C) (<a href="#eq-class">EQ</a> :D) &rArr; <a href="#eq-class">EQ</a> (<a href="#tuple4-type">TUPLE4</a> :A :B :C :D)</code>

</details>


***

#### <code>TUPLE5 :A :B :C :D :E</code> <sup><sub>[TYPE]</sub></sup><a name="tuple5-type"></a>
- <code>(TUPLE5 :A :B :C :D :E)</code>

Constructors:
- <code>TUPLE5 :: (:A &rarr; :B &rarr; :C &rarr; :D &rarr; :E &rarr; (<a href="#tuple5-type">TUPLE5</a> :A :B :C :D :E))</code>

<details>
<summary>Instances</summary>

- <code>(<a href="#eq-class">EQ</a> :A) (<a href="#eq-class">EQ</a> :B) (<a href="#eq-class">EQ</a> :C) (<a href="#eq-class">EQ</a> :D) (<a href="#eq-class">EQ</a> :E) &rArr; <a href="#eq-class">EQ</a> (<a href="#tuple5-type">TUPLE5</a> :A :B :C :D :E)</code>

</details>


***

#### <code>FRACTION</code> <sup><sub>[TYPE]</sub></sup><a name="fraction-type"></a>
- <code>(%FRACTION <a href="#integer-type">INTEGER</a> <a href="#integer-type">INTEGER</a>)</code>

A ratio of integers always in reduced form.

Constructors:
- <code>%FRACTION :: (<a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a> &rarr; <a href="#fraction-type">FRACTION</a>)</code>

<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#fraction-type">FRACTION</a></code>
- <code><a href="#num-class">NUM</a> <a href="#fraction-type">FRACTION</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#fraction-type">FRACTION</a></code>
- <code><a href="#dividable-class">DIVIDABLE</a> <a href="#integer-type">INTEGER</a> <a href="#fraction-type">FRACTION</a></code>
- <code><a href="#dividable-class">DIVIDABLE</a> <a href="#fraction-type">FRACTION</a> <a href="#fraction-type">FRACTION</a></code>
- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#fraction-type">FRACTION</a></code>

</details>


***

#### <code>OPTIONAL :A</code> <sup><sub>[TYPE]</sub></sup><a name="optional-type"></a>
- <code>(SOME :A)</code>
- <code>NONE</code>

Represents something that may not have a value.

Constructors:
- <code>SOME :: (:A &rarr; (<a href="#optional-type">OPTIONAL</a> :A))</code>
- <code>NONE :: (<a href="#optional-type">OPTIONAL</a> :A)</code>

<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> :A &rArr; <a href="#eq-class">EQ</a> (<a href="#optional-type">OPTIONAL</a> :A)</code>
- <code><a href="#iso-class">ISO</a> (<a href="#result-type">RESULT</a> <a href="#unit-type">UNIT</a> :A) (<a href="#optional-type">OPTIONAL</a> :A)</code>
- <code><a href="#ord-class">ORD</a> :A &rArr; <a href="#ord-class">ORD</a> (<a href="#optional-type">OPTIONAL</a> :A)</code>
- <code><a href="#into-class">INTO</a> (<a href="#optional-type">OPTIONAL</a> :A) (<a href="#result-type">RESULT</a> <a href="#unit-type">UNIT</a> :A)</code>
- <code><a href="#into-class">INTO</a> (<a href="#result-type">RESULT</a> :A :B) (<a href="#optional-type">OPTIONAL</a> :B)</code>
- <code><a href="#monad-class">MONAD</a> <a href="#optional-type">OPTIONAL</a></code>
- <code><a href="#monoid-class">MONOID</a> :A &rArr; <a href="#monoid-class">MONOID</a> (<a href="#optional-type">OPTIONAL</a> :A)</code>
- <code><a href="#functor-class">FUNCTOR</a> <a href="#optional-type">OPTIONAL</a></code>
- <code><a href="#monadfail-class">MONADFAIL</a> <a href="#optional-type">OPTIONAL</a></code>
- <code><a href="#semigroup-class">SEMIGROUP</a> :A &rArr; <a href="#semigroup-class">SEMIGROUP</a> (<a href="#optional-type">OPTIONAL</a> :A)</code>
- <code><a href="#alternative-class">ALTERNATIVE</a> <a href="#optional-type">OPTIONAL</a></code>
- <code><a href="#applicative-class">APPLICATIVE</a> <a href="#optional-type">OPTIONAL</a></code>
- <code><a href="#unwrappable-class">UNWRAPPABLE</a> <a href="#optional-type">OPTIONAL</a></code>

</details>


***

### Values

#### <code>NOT</code> <sup><sub>[FUNCTION]</sub></sup><a name="not-function"></a>
<code>(<a href="#boolean-type">BOOLEAN</a> &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Synonym for BOOLEAN-NOT.


***

#### <code>XOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="xor-function"></a>
<code>(<a href="#boolean-type">BOOLEAN</a> &rarr; <a href="#boolean-type">BOOLEAN</a> &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Synonym for BOOLEAN-XOR.


***

#### <code>BOOLEAN-OR</code> <sup><sub>[FUNCTION]</sub></sup><a name="boolean-or-function"></a>
<code>(<a href="#boolean-type">BOOLEAN</a> &rarr; <a href="#boolean-type">BOOLEAN</a> &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Is X or Y True? Note that this is a *function* which means both X and Y will be evaluated. Use the OR macro for short-circuiting behavior.


***

#### <code>BOOLEAN-AND</code> <sup><sub>[FUNCTION]</sub></sup><a name="boolean-and-function"></a>
<code>(<a href="#boolean-type">BOOLEAN</a> &rarr; <a href="#boolean-type">BOOLEAN</a> &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Are X and Y True? Note that this is a *function* which means both X and Y will be evaluated. Use the AND macro for short-circuiting behavior.


***

#### <code>BOOLEAN-NOT</code> <sup><sub>[FUNCTION]</sub></sup><a name="boolean-not-function"></a>
<code>(<a href="#boolean-type">BOOLEAN</a> &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Is X False?


***

#### <code>BOOLEAN-XOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="boolean-xor-function"></a>
<code>(<a href="#boolean-type">BOOLEAN</a> &rarr; <a href="#boolean-type">BOOLEAN</a> &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Are X or Y True, but not both?


***

## File: [builtin.lisp](../src/library/builtin.lisp)

### Values

#### <code>ID</code> <sup><sub>[FUNCTION]</sub></sup><a name="id-function"></a>
<code>&forall; :A. (:A &rarr; :A)</code>

A function that always returns its argument


***

#### <code>FIX</code> <sup><sub>[FUNCTION]</sub></sup><a name="fix-function"></a>
<code>&forall; :A :B. (((:A &rarr; :B) &rarr; :A &rarr; :B) &rarr; :A &rarr; :B)</code>

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

#### <code>FLIP</code> <sup><sub>[FUNCTION]</sub></sup><a name="flip-function"></a>
<code>&forall; :A :B :C. ((:A &rarr; :B &rarr; :C) &rarr; :B &rarr; :A &rarr; :C)</code>

FLIP reverses the arguments to F


***

#### <code>CONST</code> <sup><sub>[FUNCTION]</sub></sup><a name="const-function"></a>
<code>&forall; :A :B. (:A &rarr; :B &rarr; :A)</code>

A function that always returns its first argument


***

#### <code>ERROR</code> <sup><sub>[FUNCTION]</sub></sup><a name="error-function"></a>
<code>&forall; :A. (<a href="#string-type">STRING</a> &rarr; :A)</code>

Signal an error by calling CL:ERROR


***

#### <code>UNDEFINED</code> <sup><sub>[FUNCTION]</sub></sup><a name="undefined-function"></a>
<code>&forall; :A :B. (:A &rarr; :B)</code>

A function which can be used in place of any value, throwing an error at runtime.


***

## File: [classes.lisp](../src/library/classes.lisp)

### Types

#### <code>ORD</code> <sup><sub>[TYPE]</sub></sup><a name="ord-type"></a>
- <code>LT</code>
- <code>GT</code>
- <code>EQ</code>

Constructors:
- <code>LT :: <a href="#ord-type">ORD</a></code>
- <code>GT :: <a href="#ord-type">ORD</a></code>
- <code>EQ :: <a href="#ord-type">ORD</a></code>

<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#ord-type">ORD</a></code>

</details>


***

#### <code>QUANTIZATION :A</code> <sup><sub>[TYPE]</sub></sup><a name="quantization-type"></a>
- <code>(QUANTIZATION :A <a href="#integer-type">INTEGER</a> :A <a href="#integer-type">INTEGER</a> :A)</code>

Represents an integer quantization of `:t`. See the `Quantizable` typeclass.

The fields are defined as follows:

1. A value of type `:t`.

2. The greatest integer less than or equal to a particular value.

3. The remainder of this as a value of type `:t`.

4. The least integer greater than or equal to a particular value.

5. The remainder of this as a value of type `:t`.


Constructors:
- <code>QUANTIZATION :: (:A &rarr; <a href="#integer-type">INTEGER</a> &rarr; :A &rarr; <a href="#integer-type">INTEGER</a> &rarr; :A &rarr; (<a href="#quantization-type">QUANTIZATION</a> :A))</code>


***

### Classes

#### <code>EQ</code> <sup><sub>[CLASS]</sub></sup><a name="eq-class"></a>
<code><a href="#eq-class">EQ</a> :A</code>

Types which have equality defined.

Methods:
- <code>== :: (:A &rarr; :A &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#ord-type">ORD</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#boolean-type">BOOLEAN</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#u8-type">U8</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#u16-type">U16</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#u32-type">U32</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#u64-type">U64</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#i8-type">I8</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#i16-type">I16</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#i32-type">I32</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#i64-type">I64</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#fraction-type">FRACTION</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#char-type">CHAR</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#string-type">STRING</a></code>
- <code><a href="#eq-class">EQ</a> :A &rArr; <a href="#eq-class">EQ</a> (<a href="#optional-type">OPTIONAL</a> :A)</code>
- <code><a href="#eq-class">EQ</a> :A &rArr; <a href="#eq-class">EQ</a> (<a href="#list-type">LIST</a> :A)</code>
- <code>(<a href="#eq-class">EQ</a> :A) (<a href="#eq-class">EQ</a> :B) &rArr; <a href="#eq-class">EQ</a> (<a href="#tuple-type">TUPLE</a> :A :B)</code>
- <code>(<a href="#eq-class">EQ</a> :A) (<a href="#eq-class">EQ</a> :B) (<a href="#eq-class">EQ</a> :C) &rArr; <a href="#eq-class">EQ</a> (<a href="#tuple3-type">TUPLE3</a> :A :B :C)</code>
- <code>(<a href="#eq-class">EQ</a> :A) (<a href="#eq-class">EQ</a> :B) (<a href="#eq-class">EQ</a> :C) (<a href="#eq-class">EQ</a> :D) &rArr; <a href="#eq-class">EQ</a> (<a href="#tuple4-type">TUPLE4</a> :A :B :C :D)</code>
- <code>(<a href="#eq-class">EQ</a> :A) (<a href="#eq-class">EQ</a> :B) (<a href="#eq-class">EQ</a> :C) (<a href="#eq-class">EQ</a> :D) (<a href="#eq-class">EQ</a> :E) &rArr; <a href="#eq-class">EQ</a> (<a href="#tuple5-type">TUPLE5</a> :A :B :C :D :E)</code>
- <code>(<a href="#eq-class">EQ</a> :A) (<a href="#eq-class">EQ</a> :B) &rArr; <a href="#eq-class">EQ</a> (<a href="#result-type">RESULT</a> :A :B)</code>
- <code><a href="#eq-class">EQ</a> :A &rArr; <a href="#eq-class">EQ</a> (<a href="#cell-type">CELL</a> :A)</code>
- <code><a href="#eq-class">EQ</a> :A &rArr; <a href="#eq-class">EQ</a> (<a href="#vector-type">VECTOR</a> :A)</code>
- <code><a href="#eq-class">EQ</a> :A &rArr; <a href="#eq-class">EQ</a> (<a href="#slice-type">SLICE</a> :A)</code>
- <code><a href="#eq-class">EQ</a> <a href="#edgeindex-type">EDGEINDEX</a></code>
- <code><a href="#eq-class">EQ</a> <a href="#nodeindex-type">NODEINDEX</a></code>

</details>


***

#### <code>NUM</code> <sup><sub>[CLASS]</sub></sup><a name="num-class"></a>
<code><a href="#eq-class">EQ</a> :A &rArr; <a href="#num-class">NUM</a> :A</code>

Types which have numeric operations defined.

Methods:
- <code>+ :: (:A &rarr; :A &rarr; :A)</code>
- <code>- :: (:A &rarr; :A &rarr; :A)</code>
- <code>* :: (:A &rarr; :A &rarr; :A)</code>
- <code>FROMINT :: (<a href="#integer-type">INTEGER</a> &rarr; :A)</code>

<details>
<summary>Instances</summary>

- <code><a href="#num-class">NUM</a> <a href="#i8-type">I8</a></code>
- <code><a href="#num-class">NUM</a> <a href="#i16-type">I16</a></code>
- <code><a href="#num-class">NUM</a> <a href="#i32-type">I32</a></code>
- <code><a href="#num-class">NUM</a> <a href="#i64-type">I64</a></code>
- <code><a href="#num-class">NUM</a> <a href="#u8-type">U8</a></code>
- <code><a href="#num-class">NUM</a> <a href="#u16-type">U16</a></code>
- <code><a href="#num-class">NUM</a> <a href="#u32-type">U32</a></code>
- <code><a href="#num-class">NUM</a> <a href="#u64-type">U64</a></code>
- <code><a href="#num-class">NUM</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#num-class">NUM</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#num-class">NUM</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#num-class">NUM</a> <a href="#fraction-type">FRACTION</a></code>
- <code><a href="#num-class">NUM</a> :A &rArr; <a href="#num-class">NUM</a> (<a href="#cell-type">CELL</a> :A)</code>

</details>


***

#### <code>ORD</code> <sup><sub>[CLASS]</sub></sup><a name="ord-class"></a>
<code><a href="#eq-class">EQ</a> :A &rArr; <a href="#ord-class">ORD</a> :A</code>

Types whose values can be ordered.

Methods:
- <code>&lt;=&gt; :: (:A &rarr; :A &rarr; <a href="#ord-type">ORD</a>)</code>

<details>
<summary>Instances</summary>

- <code><a href="#ord-class">ORD</a> <a href="#boolean-type">BOOLEAN</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#u8-type">U8</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#u16-type">U16</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#u32-type">U32</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#u64-type">U64</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#i8-type">I8</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#i16-type">I16</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#i32-type">I32</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#i64-type">I64</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#fraction-type">FRACTION</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#char-type">CHAR</a></code>
- <code><a href="#ord-class">ORD</a> <a href="#string-type">STRING</a></code>
- <code><a href="#ord-class">ORD</a> :A &rArr; <a href="#ord-class">ORD</a> (<a href="#optional-type">OPTIONAL</a> :A)</code>
- <code>(<a href="#ord-class">ORD</a> :A) (<a href="#ord-class">ORD</a> :B) &rArr; <a href="#ord-class">ORD</a> (<a href="#tuple-type">TUPLE</a> :A :B)</code>
- <code>(<a href="#ord-class">ORD</a> :A) (<a href="#ord-class">ORD</a> :B) &rArr; <a href="#ord-class">ORD</a> (<a href="#result-type">RESULT</a> :A :B)</code>

</details>


***

#### <code>INTO</code> <sup><sub>[CLASS]</sub></sup><a name="into-class"></a>
<code><a href="#into-class">INTO</a> :A :B</code>

INTO imples *every* element of :FROM can be represented by an element of :TO. This conversion might not be injective (i.e., there may be elements in :TO that don't correspond to any in :FROM).

Methods:
- <code>INTO :: (:A &rarr; :B)</code>

<details>
<summary>Instances</summary>

- <code><a href="#into-class">INTO</a> :A :A</code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#i8-type">I8</a></code>
- <code><a href="#into-class">INTO</a> <a href="#i8-type">I8</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#i16-type">I16</a></code>
- <code><a href="#into-class">INTO</a> <a href="#i16-type">I16</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#i32-type">I32</a></code>
- <code><a href="#into-class">INTO</a> <a href="#i32-type">I32</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#i64-type">I64</a></code>
- <code><a href="#into-class">INTO</a> <a href="#i64-type">I64</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#u8-type">U8</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u8-type">U8</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u8-type">U8</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u8-type">U8</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#u16-type">U16</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u16-type">U16</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u16-type">U16</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u16-type">U16</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#u32-type">U32</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u32-type">U32</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u32-type">U32</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u32-type">U32</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#u64-type">U64</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u64-type">U64</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u64-type">U64</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#u64-type">U64</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#into-class">INTO</a> <a href="#integer-type">INTEGER</a> <a href="#string-type">STRING</a></code>
- <code><a href="#into-class">INTO</a> <a href="#string-type">STRING</a> (<a href="#list-type">LIST</a> <a href="#char-type">CHAR</a>)</code>
- <code><a href="#into-class">INTO</a> (<a href="#list-type">LIST</a> <a href="#char-type">CHAR</a>) <a href="#string-type">STRING</a></code>
- <code><a href="#into-class">INTO</a> (<a href="#tuple-type">TUPLE</a> :A :B) (<a href="#tuple-type">TUPLE</a> :B :A)</code>
- <code><a href="#into-class">INTO</a> (<a href="#result-type">RESULT</a> :A :B) (<a href="#optional-type">OPTIONAL</a> :B)</code>
- <code><a href="#into-class">INTO</a> (<a href="#optional-type">OPTIONAL</a> :A) (<a href="#result-type">RESULT</a> <a href="#unit-type">UNIT</a> :A)</code>
- <code><a href="#into-class">INTO</a> :A (<a href="#cell-type">CELL</a> :A)</code>
- <code><a href="#into-class">INTO</a> (<a href="#cell-type">CELL</a> :A) :A</code>
- <code><a href="#into-class">INTO</a> (<a href="#list-type">LIST</a> :A) (<a href="#vector-type">VECTOR</a> :A)</code>
- <code><a href="#into-class">INTO</a> (<a href="#vector-type">VECTOR</a> :A) (<a href="#list-type">LIST</a> :A)</code>
- <code><a href="#into-class">INTO</a> (<a href="#slice-type">SLICE</a> :A) (<a href="#vector-type">VECTOR</a> :A)</code>
- <code><a href="#into-class">INTO</a> (<a href="#vector-type">VECTOR</a> :A) (<a href="#slice-type">SLICE</a> :A)</code>
- <code><a href="#into-class">INTO</a> <a href="#edgeindex-type">EDGEINDEX</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#into-class">INTO</a> <a href="#nodeindex-type">NODEINDEX</a> <a href="#integer-type">INTEGER</a></code>

</details>


***

#### <code>MONAD</code> <sup><sub>[CLASS]</sub></sup><a name="monad-class"></a>
<code><a href="#applicative-class">APPLICATIVE</a> :A &rArr; <a href="#monad-class">MONAD</a> :A</code>

Types which are monads as defined in Haskell. See https://wiki.haskell.org/Monad for more information.

Methods:
- <code>&gt;&gt;= :: &forall; :B :C. ((:A :B) &rarr; (:B &rarr; (:A :C)) &rarr; (:A :C))</code>

<details>
<summary>Instances</summary>

- <code><a href="#monad-class">MONAD</a> <a href="#optional-type">OPTIONAL</a></code>
- <code><a href="#monad-class">MONAD</a> <a href="#list-type">LIST</a></code>
- <code><a href="#monad-class">MONAD</a> (<a href="#result-type">RESULT</a> :A)</code>
- <code><a href="#monad-class">MONAD</a> (<a href="#statefulcomputation-type">STATEFULCOMPUTATION</a> :A)</code>

</details>


***

#### <code>MONOID</code> <sup><sub>[CLASS]</sub></sup><a name="monoid-class"></a>
<code><a href="#semigroup-class">SEMIGROUP</a> :A &rArr; <a href="#monoid-class">MONOID</a> :A</code>

Types with an associative binary operation and identity defined.

Methods:
- <code>MEMPTY :: :A</code>

<details>
<summary>Instances</summary>

- <code><a href="#monoid-class">MONOID</a> <a href="#string-type">STRING</a></code>
- <code><a href="#monoid-class">MONOID</a> :A &rArr; <a href="#monoid-class">MONOID</a> (<a href="#optional-type">OPTIONAL</a> :A)</code>
- <code><a href="#monoid-class">MONOID</a> (<a href="#list-type">LIST</a> :A)</code>
- <code><a href="#monoid-class">MONOID</a> :A &rArr; <a href="#monoid-class">MONOID</a> (<a href="#result-type">RESULT</a> :B :A)</code>

</details>


***

#### <code>FUNCTOR</code> <sup><sub>[CLASS]</sub></sup><a name="functor-class"></a>
<code><a href="#functor-class">FUNCTOR</a> :A</code>

Types which can map an inner type where the mapping adheres to the identity and composition laws.

Methods:
- <code>MAP :: &forall; :B :C. ((:B &rarr; :C) &rarr; (:A :B) &rarr; (:A :C))</code>

<details>
<summary>Instances</summary>

- <code><a href="#functor-class">FUNCTOR</a> <a href="#optional-type">OPTIONAL</a></code>
- <code><a href="#functor-class">FUNCTOR</a> <a href="#list-type">LIST</a></code>
- <code><a href="#functor-class">FUNCTOR</a> (<a href="#result-type">RESULT</a> :A)</code>
- <code><a href="#functor-class">FUNCTOR</a> <a href="#cell-type">CELL</a></code>
- <code><a href="#functor-class">FUNCTOR</a> <a href="#vector-type">VECTOR</a></code>
- <code><a href="#functor-class">FUNCTOR</a> (<a href="#statefulcomputation-type">STATEFULCOMPUTATION</a> :A)</code>

</details>


***

#### <code>TRYINTO</code> <sup><sub>[CLASS]</sub></sup><a name="tryinto-class"></a>
<code><a href="#tryinto-class">TRYINTO</a> :A :B</code>

TRY-INTO implies *most* elements of :FROM can be represented exactly by an element of :TO, but sometimes not. If not, an error string is returned.

Methods:
- <code>TRYINTO :: (:A &rarr; (<a href="#result-type">RESULT</a> <a href="#string-type">STRING</a> :B))</code>

<details>
<summary>Instances</summary>

- <code><a href="#tryinto-class">TRYINTO</a> <a href="#string-type">STRING</a> <a href="#integer-type">INTEGER</a></code>

</details>


***

#### <code>DIVIDABLE</code> <sup><sub>[CLASS]</sub></sup><a name="dividable-class"></a>
<code>(<a href="#num-class">NUM</a> :A) (<a href="#num-class">NUM</a> :B) &rArr; <a href="#dividable-class">DIVIDABLE</a> :A :B</code>

The representation of a type such that division within that type possibly results in another type. For instance,


    (Dividable Integer Fraction)


establishes that division of two `Integer`s can result in a `Fraction`, whereas


    (Dividable Single-Float Single-Float)


establishes that division of two `Single-Float`s can result in a `Single-Float`.

Note that `Dividable` does *not* establish a default result type; you must constrain the result type yourself.

The function / is partial, and will error produce a run-time error if the divisor is zero.


Methods:
- <code>/ :: (:A &rarr; :A &rarr; :B)</code>

<details>
<summary>Instances</summary>

- <code><a href="#dividable-class">DIVIDABLE</a> <a href="#fraction-type">FRACTION</a> <a href="#fraction-type">FRACTION</a></code>
- <code><a href="#dividable-class">DIVIDABLE</a> <a href="#single-float-type">SINGLE-FLOAT</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#dividable-class">DIVIDABLE</a> <a href="#double-float-type">DOUBLE-FLOAT</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#dividable-class">DIVIDABLE</a> <a href="#integer-type">INTEGER</a> <a href="#fraction-type">FRACTION</a></code>
- <code><a href="#dividable-class">DIVIDABLE</a> <a href="#integer-type">INTEGER</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#dividable-class">DIVIDABLE</a> <a href="#integer-type">INTEGER</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>

</details>


***

#### <code>MONADFAIL</code> <sup><sub>[CLASS]</sub></sup><a name="monadfail-class"></a>
<code><a href="#monad-class">MONAD</a> :A &rArr; <a href="#monadfail-class">MONADFAIL</a> :A</code>

Methods:
- <code>FAIL :: &forall; :B. (<a href="#string-type">STRING</a> &rarr; (:A :B))</code>

<details>
<summary>Instances</summary>

- <code><a href="#monadfail-class">MONADFAIL</a> <a href="#optional-type">OPTIONAL</a></code>

</details>


***

#### <code>SEMIGROUP</code> <sup><sub>[CLASS]</sub></sup><a name="semigroup-class"></a>
<code><a href="#semigroup-class">SEMIGROUP</a> :A</code>

Types with an associative binary operation defined.

Methods:
- <code>&lt;&gt; :: (:A &rarr; :A &rarr; :A)</code>

<details>
<summary>Instances</summary>

- <code><a href="#semigroup-class">SEMIGROUP</a> <a href="#string-type">STRING</a></code>
- <code><a href="#semigroup-class">SEMIGROUP</a> :A &rArr; <a href="#semigroup-class">SEMIGROUP</a> (<a href="#optional-type">OPTIONAL</a> :A)</code>
- <code><a href="#semigroup-class">SEMIGROUP</a> (<a href="#list-type">LIST</a> :A)</code>
- <code><a href="#semigroup-class">SEMIGROUP</a> :A &rArr; <a href="#semigroup-class">SEMIGROUP</a> (<a href="#result-type">RESULT</a> :B :A)</code>
- <code><a href="#semigroup-class">SEMIGROUP</a> :A &rArr; <a href="#semigroup-class">SEMIGROUP</a> (<a href="#cell-type">CELL</a> :A)</code>
- <code><a href="#semigroup-class">SEMIGROUP</a> (<a href="#vector-type">VECTOR</a> :A)</code>

</details>


***

#### <code>ALTERNATIVE</code> <sup><sub>[CLASS]</sub></sup><a name="alternative-class"></a>
<code><a href="#applicative-class">APPLICATIVE</a> :A &rArr; <a href="#alternative-class">ALTERNATIVE</a> :A</code>

Types which are monoids on applicative functors.

Methods:
- <code>ALT :: &forall; :B. ((:A :B) &rarr; (:A :B) &rarr; (:A :B))</code>
- <code>EMPTY :: &forall; :B. (:A :B)</code>

<details>
<summary>Instances</summary>

- <code><a href="#alternative-class">ALTERNATIVE</a> <a href="#optional-type">OPTIONAL</a></code>
- <code><a href="#alternative-class">ALTERNATIVE</a> <a href="#list-type">LIST</a></code>

</details>


***

#### <code>APPLICATIVE</code> <sup><sub>[CLASS]</sub></sup><a name="applicative-class"></a>
<code><a href="#functor-class">FUNCTOR</a> :A &rArr; <a href="#applicative-class">APPLICATIVE</a> :A</code>

Types which are a functor which can embed pure expressions and sequence operations.

Methods:
- <code>PURE :: &forall; :B. (:B &rarr; (:A :B))</code>
- <code>LIFTA2 :: &forall; :B :C :D. ((:B &rarr; :C &rarr; :D) &rarr; (:A :B) &rarr; (:A :C) &rarr; (:A :D))</code>

<details>
<summary>Instances</summary>

- <code><a href="#applicative-class">APPLICATIVE</a> <a href="#optional-type">OPTIONAL</a></code>
- <code><a href="#applicative-class">APPLICATIVE</a> <a href="#list-type">LIST</a></code>
- <code><a href="#applicative-class">APPLICATIVE</a> (<a href="#result-type">RESULT</a> :A)</code>
- <code><a href="#applicative-class">APPLICATIVE</a> <a href="#cell-type">CELL</a></code>
- <code><a href="#applicative-class">APPLICATIVE</a> (<a href="#statefulcomputation-type">STATEFULCOMPUTATION</a> :A)</code>

</details>


***

#### <code>QUANTIZABLE</code> <sup><sub>[CLASS]</sub></sup><a name="quantizable-class"></a>
<code>(<a href="#ord-class">ORD</a> :A) (<a href="#num-class">NUM</a> :A) &rArr; <a href="#quantizable-class">QUANTIZABLE</a> :A</code>

The representation of a type that allows "quantizing", "snapping to integers", or "rounding." (All of these concepts are roughly equivalent.)


Methods:
- <code>QUANTIZE :: (:A &rarr; (<a href="#quantization-type">QUANTIZATION</a> :A))</code>

<details>
<summary>Instances</summary>

- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#integer-type">INTEGER</a></code>
- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#i32-type">I32</a></code>
- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#i64-type">I64</a></code>
- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#u8-type">U8</a></code>
- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#u32-type">U32</a></code>
- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#u64-type">U64</a></code>
- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#single-float-type">SINGLE-FLOAT</a></code>
- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#double-float-type">DOUBLE-FLOAT</a></code>
- <code><a href="#quantizable-class">QUANTIZABLE</a> <a href="#fraction-type">FRACTION</a></code>

</details>


***

#### <code>UNWRAPPABLE</code> <sup><sub>[CLASS]</sub></sup><a name="unwrappable-class"></a>
<code><a href="#unwrappable-class">UNWRAPPABLE</a> :A</code>

Types which might be able to be unwrapped, otherwise returning a default value.

Methods:
- <code>WITHDEFAULT :: &forall; :B. (:B &rarr; (:A :B) &rarr; :B)</code>
- <code>UNWRAP :: &forall; :B. ((:A :B) &rarr; :B)</code>

<details>
<summary>Instances</summary>

- <code><a href="#unwrappable-class">UNWRAPPABLE</a> <a href="#optional-type">OPTIONAL</a></code>
- <code><a href="#unwrappable-class">UNWRAPPABLE</a> (<a href="#result-type">RESULT</a> :A)</code>

</details>


***

### Values

#### <code>&lt;</code> <sup><sub>[FUNCTION]</sub></sup><a name="&lt;-function"></a>
<code>&forall; :A. <a href="#ord-class">ORD</a> :A &rArr; (:A &rarr; :A &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Is X less than Y?


***

#### <code>&gt;</code> <sup><sub>[FUNCTION]</sub></sup><a name="&gt;-function"></a>
<code>&forall; :A. <a href="#ord-class">ORD</a> :A &rArr; (:A &rarr; :A &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Is X greater than Y?


***

#### <code>/=</code> <sup><sub>[FUNCTION]</sub></sup><a name="/=-function"></a>
<code>&forall; :A. <a href="#eq-class">EQ</a> :A &rArr; (:A &rarr; :A &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

***

#### <code>&lt;=</code> <sup><sub>[FUNCTION]</sub></sup><a name="&lt;=-function"></a>
<code>&forall; :A. <a href="#ord-class">ORD</a> :A &rArr; (:A &rarr; :A &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Is X less than or equal to Y?


***

#### <code>&gt;=</code> <sup><sub>[FUNCTION]</sub></sup><a name="&gt;=-function"></a>
<code>&forall; :A. <a href="#ord-class">ORD</a> :A &rArr; (:A &rarr; :A &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Is X greater than or equal to Y?


***

#### <code>&gt;&gt;</code> <sup><sub>[FUNCTION]</sub></sup><a name="&gt;&gt;-function"></a>
<code>&forall; :A :B :C. <a href="#monad-class">MONAD</a> :A &rArr; ((:A :B) &rarr; (:A :C) &rarr; (:A :C))</code>

***

#### <code>MAX</code> <sup><sub>[FUNCTION]</sub></sup><a name="max-function"></a>
<code>&forall; :A. <a href="#ord-class">ORD</a> :A &rArr; (:A &rarr; :A &rarr; :A)</code>

Returns the greater element of X and Y.


***

#### <code>MIN</code> <sup><sub>[FUNCTION]</sub></sup><a name="min-function"></a>
<code>&forall; :A. <a href="#ord-class">ORD</a> :A &rArr; (:A &rarr; :A &rarr; :A)</code>

Returns the lesser element of X and Y.


***

## File: [fraction.lisp](../src/library/fraction.lisp)

### Values

#### <code>NUMERATOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="numerator-function"></a>
<code>(<a href="#fraction-type">FRACTION</a> &rarr; <a href="#integer-type">INTEGER</a>)</code>

The numerator of a fraction.


***

#### <code>RECIPROCAL</code> <sup><sub>[FUNCTION]</sub></sup><a name="reciprocal-function"></a>
<code>(<a href="#fraction-type">FRACTION</a> &rarr; <a href="#fraction-type">FRACTION</a>)</code>

The reciprocal of a fraction.


***

#### <code>DENOMINATOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="denominator-function"></a>
<code>(<a href="#fraction-type">FRACTION</a> &rarr; <a href="#integer-type">INTEGER</a>)</code>

The denominator of a fraction.


***

## File: [arith.lisp](../src/library/arith.lisp)

### Values

#### <code>ABS</code> <sup><sub>[FUNCTION]</sub></sup><a name="abs-function"></a>
<code>&forall; :A. (<a href="#num-class">NUM</a> :A) (<a href="#ord-class">ORD</a> :A) &rArr; (:A &rarr; :A)</code>

Absolute value of X.


***

#### <code>ASH</code> <sup><sub>[FUNCTION]</sub></sup><a name="ash-function"></a>
<code>(<a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a>)</code>

Compute the "arithmetic shift" of X by N. 


***

#### <code>GCD</code> <sup><sub>[FUNCTION]</sub></sup><a name="gcd-function"></a>
<code>(<a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a>)</code>

Compute the greatest common divisor of A and B.


***

#### <code>LCM</code> <sup><sub>[FUNCTION]</sub></sup><a name="lcm-function"></a>
<code>(<a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a>)</code>

Compute the least common multiple of A and B.


***

#### <code>MOD</code> <sup><sub>[FUNCTION]</sub></sup><a name="mod-function"></a>
<code>(<a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a>)</code>

Compute NUM modulo BASE.


***

#### <code>ODD</code> <sup><sub>[FUNCTION]</sub></sup><a name="odd-function"></a>
<code>(<a href="#integer-type">INTEGER</a> &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Is N odd?


***

#### <code>EVEN</code> <sup><sub>[FUNCTION]</sub></sup><a name="even-function"></a>
<code>(<a href="#integer-type">INTEGER</a> &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Is N even?


***

#### <code>EXPT</code> <sup><sub>[FUNCTION]</sub></sup><a name="expt-function"></a>
<code>(<a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a>)</code>

Exponentiate BASE to a non-negative POWER.


***

#### <code>SIGN</code> <sup><sub>[FUNCTION]</sub></sup><a name="sign-function"></a>
<code>&forall; :A. (<a href="#num-class">NUM</a> :A) (<a href="#ord-class">ORD</a> :A) &rArr; (:A &rarr; <a href="#integer-type">INTEGER</a>)</code>

The sign of X.


***

#### <code>NEGATE</code> <sup><sub>[FUNCTION]</sub></sup><a name="negate-function"></a>
<code>&forall; :A. <a href="#num-class">NUM</a> :A &rArr; (:A &rarr; :A)</code>

***

#### <code>DOUBLE-FLOAT-&gt;INTEGER</code> <sup><sub>[FUNCTION]</sub></sup><a name="double-float-&gt;integer-function"></a>
<code>(<a href="#double-float-type">DOUBLE-FLOAT</a> &rarr; (<a href="#optional-type">OPTIONAL</a> <a href="#integer-type">INTEGER</a>))</code>

Round a Double-Float to the nearest Integer.


***

#### <code>INTEGER-&gt;DOUBLE-FLOAT</code> <sup><sub>[FUNCTION]</sub></sup><a name="integer-&gt;double-float-function"></a>
<code>(<a href="#integer-type">INTEGER</a> &rarr; <a href="#double-float-type">DOUBLE-FLOAT</a>)</code>

***

#### <code>INTEGER-&gt;SINGLE-FLOAT</code> <sup><sub>[FUNCTION]</sub></sup><a name="integer-&gt;single-float-function"></a>
<code>(<a href="#integer-type">INTEGER</a> &rarr; <a href="#single-float-type">SINGLE-FLOAT</a>)</code>

***

#### <code>SINGLE-FLOAT-&gt;INTEGER</code> <sup><sub>[FUNCTION]</sub></sup><a name="single-float-&gt;integer-function"></a>
<code>(<a href="#single-float-type">SINGLE-FLOAT</a> &rarr; (<a href="#optional-type">OPTIONAL</a> <a href="#integer-type">INTEGER</a>))</code>

Round a Single-Float to the nearest Integer.


***

## File: [string.lisp](../src/library/string.lisp)

### Values

#### <code>PARSE-INT</code> <sup><sub>[FUNCTION]</sub></sup><a name="parse-int-function"></a>
<code>(<a href="#string-type">STRING</a> &rarr; (<a href="#optional-type">OPTIONAL</a> <a href="#integer-type">INTEGER</a>))</code>

Parse the integer in string STR.


***

#### <code>SUBSTRING</code> <sup><sub>[FUNCTION]</sub></sup><a name="substring-function"></a>
<code>(<a href="#string-type">STRING</a> &rarr; <a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a> &rarr; <a href="#string-type">STRING</a>)</code>

Compute a substring of a string bounded by given indices.


***

#### <code>CONCAT-STRING</code> <sup><sub>[FUNCTION]</sub></sup><a name="concat-string-function"></a>
<code>(<a href="#string-type">STRING</a> &rarr; <a href="#string-type">STRING</a> &rarr; <a href="#string-type">STRING</a>)</code>

Concatenate STR1 and STR2 together, returning a new string.


***

#### <code>STRING-LENGTH</code> <sup><sub>[FUNCTION]</sub></sup><a name="string-length-function"></a>
<code>(<a href="#string-type">STRING</a> &rarr; <a href="#integer-type">INTEGER</a>)</code>

The length of a string STR.


***

#### <code>REVERSE-STRING</code> <sup><sub>[FUNCTION]</sub></sup><a name="reverse-string-function"></a>
<code>(<a href="#string-type">STRING</a> &rarr; <a href="#string-type">STRING</a>)</code>

Reverse a string.


***

## File: [optional.lisp](../src/library/optional.lisp)

### Values

#### <code>ISNONE</code> <sup><sub>[FUNCTION]</sub></sup><a name="isnone-function"></a>
<code>&forall; :A. ((<a href="#optional-type">OPTIONAL</a> :A) &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Is X None?


***

#### <code>ISSOME</code> <sup><sub>[FUNCTION]</sub></sup><a name="issome-function"></a>
<code>&forall; :A. ((<a href="#optional-type">OPTIONAL</a> :A) &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Is X Some?


***

#### <code>FROMSOME</code> <sup><sub>[FUNCTION]</sub></sup><a name="fromsome-function"></a>
<code>&forall; :A. (<a href="#string-type">STRING</a> &rarr; (<a href="#optional-type">OPTIONAL</a> :A) &rarr; :A)</code>

Get the value of OPT, erroring with the provided string if it is None.


***

## File: [list.lisp](../src/library/list.lisp)

### Values

#### <code>ALL</code> <sup><sub>[FUNCTION]</sub></sup><a name="all-function"></a>
<code>&forall; :A. ((:A &rarr; <a href="#boolean-type">BOOLEAN</a>) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Returns TRUE if every element in XS matches F.


***

#### <code>ANY</code> <sup><sub>[FUNCTION]</sub></sup><a name="any-function"></a>
<code>&forall; :A. ((:A &rarr; <a href="#boolean-type">BOOLEAN</a>) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Returns TRUE if at least one element in XS matches F.


***

#### <code>CAR</code> <sup><sub>[FUNCTION]</sub></sup><a name="car-function"></a>
<code>&forall; :A. ((<a href="#list-type">LIST</a> :A) &rarr; :A)</code>

Return the traditional car of a list XS. (Error when there is no car.)


***

#### <code>CDR</code> <sup><sub>[FUNCTION]</sub></sup><a name="cdr-function"></a>
<code>&forall; :A. ((<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Return the traditional cdr of a list XS.


***

#### <code>NTH</code> <sup><sub>[FUNCTION]</sub></sup><a name="nth-function"></a>
<code>&forall; :A. (<a href="#integer-type">INTEGER</a> &rarr; (<a href="#list-type">LIST</a> :A) &rarr; :A)</code>

Like INDEX, but errors if the index is not found.


***

#### <code>SUM</code> <sup><sub>[FUNCTION]</sub></sup><a name="sum-function"></a>
<code>&forall; :A. <a href="#num-class">NUM</a> :A &rArr; ((<a href="#list-type">LIST</a> :A) &rarr; :A)</code>

Returns the sum of XS


***

#### <code>ZIP</code> <sup><sub>[FUNCTION]</sub></sup><a name="zip-function"></a>
<code>&forall; :A :B. ((<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :B) &rarr; (<a href="#list-type">LIST</a> (<a href="#tuple-type">TUPLE</a> :A :B)))</code>

Builds a list of tuples with the elements of XS and YS.


***

#### <code>DROP</code> <sup><sub>[FUNCTION]</sub></sup><a name="drop-function"></a>
<code>&forall; :A. (<a href="#integer-type">INTEGER</a> &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Returns a list with the first N elements of XS removed


***

#### <code>FIND</code> <sup><sub>[FUNCTION]</sub></sup><a name="find-function"></a>
<code>&forall; :A. ((:A &rarr; <a href="#boolean-type">BOOLEAN</a>) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#optional-type">OPTIONAL</a> :A))</code>

Returns the first element in a list matching the predicate function F.


***

#### <code>FOLD</code> <sup><sub>[FUNCTION]</sub></sup><a name="fold-function"></a>
<code>&forall; :A :B. ((:A &rarr; :B &rarr; :B) &rarr; :B &rarr; (<a href="#list-type">LIST</a> :A) &rarr; :B)</code>

Tail recursive left fold on lists.


***

#### <code>HEAD</code> <sup><sub>[FUNCTION]</sub></sup><a name="head-function"></a>
<code>&forall; :A. ((<a href="#list-type">LIST</a> :A) &rarr; (<a href="#optional-type">OPTIONAL</a> :A))</code>

Returns the first element of a list.


***

#### <code>INIT</code> <sup><sub>[FUNCTION]</sub></sup><a name="init-function"></a>
<code>&forall; :A. ((<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Returns every element except the last in a list.


***

#### <code>LAST</code> <sup><sub>[FUNCTION]</sub></sup><a name="last-function"></a>
<code>&forall; :A. ((<a href="#list-type">LIST</a> :A) &rarr; (<a href="#optional-type">OPTIONAL</a> :A))</code>

Returns the last element of a list.


***

#### <code>NULL</code> <sup><sub>[FUNCTION]</sub></sup><a name="null-function"></a>
<code>&forall; :A. ((<a href="#list-type">LIST</a> :A) &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Returns TRUE if XS is an empty list.


***

#### <code>SORT</code> <sup><sub>[FUNCTION]</sub></sup><a name="sort-function"></a>
<code>&forall; :A. <a href="#ord-class">ORD</a> :A &rArr; ((<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Performs a stable sort of XS.


***

#### <code>TAIL</code> <sup><sub>[FUNCTION]</sub></sup><a name="tail-function"></a>
<code>&forall; :A. ((<a href="#list-type">LIST</a> :A) &rarr; (<a href="#optional-type">OPTIONAL</a> (<a href="#list-type">LIST</a> :A)))</code>

Returns every element except the first in a list.


***

#### <code>TAKE</code> <sup><sub>[FUNCTION]</sub></sup><a name="take-function"></a>
<code>&forall; :A. (<a href="#integer-type">INTEGER</a> &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Returns the first N elements of XS


***

#### <code>FOLDR</code> <sup><sub>[FUNCTION]</sub></sup><a name="foldr-function"></a>
<code>&forall; :A :B. ((:A &rarr; :B &rarr; :B) &rarr; :B &rarr; (<a href="#list-type">LIST</a> :A) &rarr; :B)</code>

Right fold on lists. Is not tail recursive.


***

#### <code>INDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="index-function"></a>
<code>&forall; :A. ((<a href="#list-type">LIST</a> :A) &rarr; <a href="#integer-type">INTEGER</a> &rarr; (<a href="#optional-type">OPTIONAL</a> :A))</code>

Returns the Ith element of XS.


***

#### <code>RANGE</code> <sup><sub>[FUNCTION]</sub></sup><a name="range-function"></a>
<code>(<a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a> &rarr; (<a href="#list-type">LIST</a> <a href="#integer-type">INTEGER</a>))</code>

Returns a list containing the numbers from START to END inclusive.


***

#### <code>SPLIT</code> <sup><sub>[FUNCTION]</sub></sup><a name="split-function"></a>
<code>(<a href="#char-type">CHAR</a> &rarr; <a href="#string-type">STRING</a> &rarr; (<a href="#list-type">LIST</a> <a href="#string-type">STRING</a>))</code>

***

#### <code>UNION</code> <sup><sub>[FUNCTION]</sub></sup><a name="union-function"></a>
<code>&forall; :A. <a href="#eq-class">EQ</a> :A &rArr; ((<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Returns a new list with the elements from both XS and YS and without duplicates.


***

#### <code>APPEND</code> <sup><sub>[FUNCTION]</sub></sup><a name="append-function"></a>
<code>&forall; :A. ((<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Appends two lists together and returns a new list.


***

#### <code>CONCAT</code> <sup><sub>[FUNCTION]</sub></sup><a name="concat-function"></a>
<code>&forall; :A. ((<a href="#list-type">LIST</a> (<a href="#list-type">LIST</a> :A)) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Appends a list of lists together into a single new list.


***

#### <code>DELETE</code> <sup><sub>[FUNCTION]</sub></sup><a name="delete-function"></a>
<code>&forall; :A. <a href="#eq-class">EQ</a> :A &rArr; (:A &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Return a new list with the first element equal to X removed.


***

#### <code>FILTER</code> <sup><sub>[FUNCTION]</sub></sup><a name="filter-function"></a>
<code>&forall; :A. ((:A &rarr; <a href="#boolean-type">BOOLEAN</a>) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Returns a new list containing every element of XS that matches the predicate function F in the same order.


***

#### <code>INSERT</code> <sup><sub>[FUNCTION]</sub></sup><a name="insert-function"></a>
<code>&forall; :A. <a href="#ord-class">ORD</a> :A &rArr; (:A &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Inserts an element into a list at the first place it is less than or equal to the next element.


***

#### <code>LENGTH</code> <sup><sub>[FUNCTION]</sub></sup><a name="length-function"></a>
<code>&forall; :A. ((<a href="#list-type">LIST</a> :A) &rarr; <a href="#integer-type">INTEGER</a>)</code>

Returns the length of a list.


***

#### <code>LOOKUP</code> <sup><sub>[FUNCTION]</sub></sup><a name="lookup-function"></a>
<code>&forall; :A :B. <a href="#eq-class">EQ</a> :A &rArr; (:A &rarr; (<a href="#list-type">LIST</a> (<a href="#tuple-type">TUPLE</a> :A :B)) &rarr; (<a href="#optional-type">OPTIONAL</a> :B))</code>

Returns the value of the first (key, value) tuple in XS where the key matches E.


***

#### <code>MEMBER</code> <sup><sub>[FUNCTION]</sub></sup><a name="member-function"></a>
<code>&forall; :A. <a href="#eq-class">EQ</a> :A &rArr; (:A &rarr; (<a href="#list-type">LIST</a> :A) &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Returns true if any element of XS is equal to E.


***

#### <code>REPEAT</code> <sup><sub>[FUNCTION]</sub></sup><a name="repeat-function"></a>
<code>&forall; :A. (<a href="#integer-type">INTEGER</a> &rarr; :A &rarr; (<a href="#list-type">LIST</a> :A))</code>

Returns a list with X repeated N times.


***

#### <code>SORTBY</code> <sup><sub>[FUNCTION]</sub></sup><a name="sortby-function"></a>
<code>&forall; :A. ((:A &rarr; :A &rarr; <a href="#ord-type">ORD</a>) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Generic version of sort


***

#### <code>COUNTBY</code> <sup><sub>[FUNCTION]</sub></sup><a name="countby-function"></a>
<code>&forall; :A. ((:A &rarr; <a href="#boolean-type">BOOLEAN</a>) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; <a href="#integer-type">INTEGER</a>)</code>

Count the number of items in THINGS that satisfy the predicate F.


***

#### <code>MAXIMUM</code> <sup><sub>[FUNCTION]</sub></sup><a name="maximum-function"></a>
<code>&forall; :A. <a href="#ord-class">ORD</a> :A &rArr; ((<a href="#list-type">LIST</a> :A) &rarr; (<a href="#optional-type">OPTIONAL</a> :A))</code>

Returns a greatest element of a list, or None.


***

#### <code>MINIMUM</code> <sup><sub>[FUNCTION]</sub></sup><a name="minimum-function"></a>
<code>&forall; :A. <a href="#ord-class">ORD</a> :A &rArr; ((<a href="#list-type">LIST</a> :A) &rarr; (<a href="#optional-type">OPTIONAL</a> :A))</code>

Returns a least element of a list, or None.


***

#### <code>PRODUCT</code> <sup><sub>[FUNCTION]</sub></sup><a name="product-function"></a>
<code>&forall; :A. <a href="#num-class">NUM</a> :A &rArr; ((<a href="#list-type">LIST</a> :A) &rarr; :A)</code>

Returns the product of XS


***

#### <code>REVERSE</code> <sup><sub>[FUNCTION]</sub></sup><a name="reverse-function"></a>
<code>&forall; :A. ((<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Returns a new list containing the same elements in reverse order.


***

#### <code>ZIPWITH</code> <sup><sub>[FUNCTION]</sub></sup><a name="zipwith-function"></a>
<code>&forall; :A :B :C. ((:A &rarr; :B &rarr; :C) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :B) &rarr; (<a href="#list-type">LIST</a> :C))</code>

Builds a new list by calling F with elements of XS and YS.


***

#### <code>INSERTBY</code> <sup><sub>[FUNCTION]</sub></sup><a name="insertby-function"></a>
<code>&forall; :A. ((:A &rarr; :A &rarr; <a href="#ord-type">ORD</a>) &rarr; :A &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Generic version of insert


***

#### <code>ZIPWITH3</code> <sup><sub>[FUNCTION]</sub></sup><a name="zipwith3-function"></a>
<code>&forall; :A :B :C :D. ((:A &rarr; :B &rarr; :C &rarr; :D) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :B) &rarr; (<a href="#list-type">LIST</a> :C) &rarr; (<a href="#list-type">LIST</a> :D))</code>

Build a new list by calling F with elements of XS, YS and ZS


***

#### <code>ZIPWITH4</code> <sup><sub>[FUNCTION]</sub></sup><a name="zipwith4-function"></a>
<code>&forall; :A :B :C :D :E. ((:A &rarr; :B &rarr; :C &rarr; :D &rarr; :E) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :B) &rarr; (<a href="#list-type">LIST</a> :C) &rarr; (<a href="#list-type">LIST</a> :D) &rarr; (<a href="#list-type">LIST</a> :E))</code>

Build a new list by calling F with elements of AS, BS, CS and DS


***

#### <code>ZIPWITH5</code> <sup><sub>[FUNCTION]</sub></sup><a name="zipwith5-function"></a>
<code>&forall; :A :B :C :D :E :F. ((:A &rarr; :B &rarr; :C &rarr; :D &rarr; :E &rarr; :F) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :B) &rarr; (<a href="#list-type">LIST</a> :C) &rarr; (<a href="#list-type">LIST</a> :D) &rarr; (<a href="#list-type">LIST</a> :E) &rarr; (<a href="#list-type">LIST</a> :F))</code>

Build a new list by calling F with elements of AS, BS, CS, DS and ES


***

#### <code>CONCATMAP</code> <sup><sub>[FUNCTION]</sub></sup><a name="concatmap-function"></a>
<code>&forall; :A :B. ((:A &rarr; (<a href="#list-type">LIST</a> :B)) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :B))</code>

Apply F to each element in XS and concatenate the results.


***

#### <code>ELEMINDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="elemindex-function"></a>
<code>&forall; :A. <a href="#eq-class">EQ</a> :A &rArr; (:A &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#optional-type">OPTIONAL</a> <a href="#integer-type">INTEGER</a>))</code>

***

#### <code>FINDINDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="findindex-function"></a>
<code>&forall; :A. ((:A &rarr; <a href="#boolean-type">BOOLEAN</a>) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#optional-type">OPTIONAL</a> <a href="#integer-type">INTEGER</a>))</code>

***

#### <code>OPTIMUMBY</code> <sup><sub>[FUNCTION]</sub></sup><a name="optimumby-function"></a>
<code>&forall; :A. ((:A &rarr; :A &rarr; <a href="#boolean-type">BOOLEAN</a>) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#optional-type">OPTIONAL</a> :A))</code>

Returns an optimum according to a total order.


***

#### <code>PARTITION</code> <sup><sub>[FUNCTION]</sub></sup><a name="partition-function"></a>
<code>&forall; :A. ((:A &rarr; <a href="#boolean-type">BOOLEAN</a>) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#tuple-type">TUPLE</a> (<a href="#list-type">LIST</a> :A) (<a href="#list-type">LIST</a> :A)))</code>

Splits a list into two new lists. The first list contains elements matching predicate F.


***

#### <code>SINGLETON</code> <sup><sub>[FUNCTION]</sub></sup><a name="singleton-function"></a>
<code>&forall; :A. (:A &rarr; (<a href="#list-type">LIST</a> :A))</code>

Returns a single element list containg only X.


***

#### <code>TRANSPOSE</code> <sup><sub>[FUNCTION]</sub></sup><a name="transpose-function"></a>
<code>&forall; :A. ((<a href="#list-type">LIST</a> (<a href="#list-type">LIST</a> :A)) &rarr; (<a href="#list-type">LIST</a> (<a href="#list-type">LIST</a> :A)))</code>

Transposes a matrix represented by a list of lists.


***

#### <code>INTERCALATE</code> <sup><sub>[FUNCTION]</sub></sup><a name="intercalate-function"></a>
<code>&forall; :A. ((<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> (<a href="#list-type">LIST</a> :A)) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Intersperses XS into XSS and then concatenates the result.


***

#### <code>INTERSPERSE</code> <sup><sub>[FUNCTION]</sub></sup><a name="intersperse-function"></a>
<code>&forall; :A. (:A &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Returns a new list where every other element is E.


***

#### <code>INTERSECTION</code> <sup><sub>[FUNCTION]</sub></sup><a name="intersection-function"></a>
<code>&forall; :A. <a href="#eq-class">EQ</a> :A &rArr; ((<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Returns elements which occur in both lists. Does not return duplicates and does not guarantee order.


***

#### <code>LIST-DIFFERENCE</code> <sup><sub>[FUNCTION]</sub></sup><a name="list-difference-function"></a>
<code>&forall; :A. <a href="#eq-class">EQ</a> :A &rArr; ((<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Returns a new list with the first occurence of each element in YS deleted from XS.


***

#### <code>REMOVE-DUPLICATES</code> <sup><sub>[FUNCTION]</sub></sup><a name="remove-duplicates-function"></a>
<code>&forall; :A. <a href="#eq-class">EQ</a> :A &rArr; ((<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> :A))</code>

Returns a new list without duplicate elements.


***

#### <code>EQUIVALENCE-CLASSES</code> <sup><sub>[FUNCTION]</sub></sup><a name="equivalence-classes-function"></a>
<code>&forall; :A. <a href="#eq-class">EQ</a> :A &rArr; ((<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> (<a href="#list-type">LIST</a> :A)))</code>

***

#### <code>EQUIVALENCE-CLASSES-BY</code> <sup><sub>[FUNCTION]</sub></sup><a name="equivalence-classes-by-function"></a>
<code>&forall; :A. ((:A &rarr; :A &rarr; <a href="#boolean-type">BOOLEAN</a>) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (<a href="#list-type">LIST</a> (<a href="#list-type">LIST</a> :A)))</code>

Break a list into a list of equivalence classes according to an equivalence relation.


***

## File: [tuple.lisp](../src/library/tuple.lisp)

### Values

#### <code>FST</code> <sup><sub>[FUNCTION]</sub></sup><a name="fst-function"></a>
<code>&forall; :A :B. ((<a href="#tuple-type">TUPLE</a> :A :B) &rarr; :A)</code>

Get the first element of a tuple.


***

#### <code>SND</code> <sup><sub>[FUNCTION]</sub></sup><a name="snd-function"></a>
<code>&forall; :A :B. ((<a href="#tuple-type">TUPLE</a> :A :B) &rarr; :B)</code>

Get the second element of a tuple.


***

## File: [result.lisp](../src/library/result.lisp)

### Values

#### <code>ISOK</code> <sup><sub>[FUNCTION]</sub></sup><a name="isok-function"></a>
<code>&forall; :A :B. ((<a href="#result-type">RESULT</a> :A :B) &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Returns TRUE if X is ERR


***

#### <code>ISERR</code> <sup><sub>[FUNCTION]</sub></sup><a name="iserr-function"></a>
<code>&forall; :A :B. ((<a href="#result-type">RESULT</a> :A :B) &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Returns TRUE if X is ERR


***

#### <code>MAPERR</code> <sup><sub>[FUNCTION]</sub></sup><a name="maperr-function"></a>
<code>&forall; :A :B :C. ((:A &rarr; :B) &rarr; (<a href="#result-type">RESULT</a> :A :C) &rarr; (<a href="#result-type">RESULT</a> :B :C))</code>

Map over the ERR case


***

## File: [functions.lisp](../src/library/functions.lisp)

### Values

#### <code>ASUM</code> <sup><sub>[FUNCTION]</sub></sup><a name="asum-function"></a>
<code>&forall; :A :B. <a href="#alternative-class">ALTERNATIVE</a> :A &rArr; ((<a href="#list-type">LIST</a> (:A :B)) &rarr; (:A :B))</code>

Fold over a list using alt


***

#### <code>TRACE</code> <sup><sub>[FUNCTION]</sub></sup><a name="trace-function"></a>
<code>(<a href="#string-type">STRING</a> &rarr; <a href="#unit-type">UNIT</a>)</code>

Print a line to *STANDARD-OUTPUT*


***

#### <code>COMPOSE</code> <sup><sub>[FUNCTION]</sub></sup><a name="compose-function"></a>
<code>&forall; :A :B :C. ((:A &rarr; :B) &rarr; (:C &rarr; :A) &rarr; :C &rarr; :B)</code>

***

#### <code>CONJOIN</code> <sup><sub>[FUNCTION]</sub></sup><a name="conjoin-function"></a>
<code>&forall; :A. ((:A &rarr; <a href="#boolean-type">BOOLEAN</a>) &rarr; (:A &rarr; <a href="#boolean-type">BOOLEAN</a>) &rarr; :A &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Compute the conjunction of two unary Boolean functions.


***

#### <code>DISJOIN</code> <sup><sub>[FUNCTION]</sub></sup><a name="disjoin-function"></a>
<code>&forall; :A. ((:A &rarr; <a href="#boolean-type">BOOLEAN</a>) &rarr; (:A &rarr; <a href="#boolean-type">BOOLEAN</a>) &rarr; :A &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Compute the disjunction of two unary Boolean functions.


***

#### <code>SEQUENCE</code> <sup><sub>[FUNCTION]</sub></sup><a name="sequence-function"></a>
<code>&forall; :A :B. <a href="#applicative-class">APPLICATIVE</a> :A &rArr; ((<a href="#list-type">LIST</a> (:A :B)) &rarr; (:A (<a href="#list-type">LIST</a> :B)))</code>

***

#### <code>TRAVERSE</code> <sup><sub>[FUNCTION]</sub></sup><a name="traverse-function"></a>
<code>&forall; :A :B :C. <a href="#applicative-class">APPLICATIVE</a> :B &rArr; ((:A &rarr; (:B :C)) &rarr; (<a href="#list-type">LIST</a> :A) &rarr; (:B (<a href="#list-type">LIST</a> :C)))</code>

Map the elements of XS with F from left to right, collecting the results.


***

#### <code>COMPLEMENT</code> <sup><sub>[FUNCTION]</sub></sup><a name="complement-function"></a>
<code>&forall; :A. ((:A &rarr; <a href="#boolean-type">BOOLEAN</a>) &rarr; :A &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Compute the complement of a unary Boolean function.


***

#### <code>TRACEOBJECT</code> <sup><sub>[FUNCTION]</sub></sup><a name="traceobject-function"></a>
<code>&forall; :A. (<a href="#string-type">STRING</a> &rarr; :A &rarr; <a href="#unit-type">UNIT</a>)</code>

Print a line to *STANDARD-OUTPUT* in the form "{STR}: {ITEM}"


***

## File: [quantize.lisp](../src/library/quantize.lisp)

### Values

#### <code>FLOOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="floor-function"></a>
<code>&forall; :A. <a href="#quantizable-class">QUANTIZABLE</a> :A &rArr; (:A &rarr; <a href="#integer-type">INTEGER</a>)</code>

Return the greatest integer less than or equal to X.


***

#### <code>ROUND</code> <sup><sub>[FUNCTION]</sub></sup><a name="round-function"></a>
<code>&forall; :A. <a href="#quantizable-class">QUANTIZABLE</a> :A &rArr; (:A &rarr; <a href="#integer-type">INTEGER</a>)</code>

Return the nearest integer to X, with ties breaking toward positive infinity.


***

#### <code>SAFE/</code> <sup><sub>[FUNCTION]</sub></sup><a name="safe/-function"></a>
<code>&forall; :A :B. <a href="#dividable-class">DIVIDABLE</a> :A :B &rArr; (:A &rarr; :A &rarr; (<a href="#optional-type">OPTIONAL</a> :B))</code>

Safely divide X by Y, returning None if Y is zero.


***

#### <code>EXACT/</code> <sup><sub>[FUNCTION]</sub></sup><a name="exact/-function"></a>
<code>(<a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a> &rarr; <a href="#fraction-type">FRACTION</a>)</code>

Exactly divide two integers and produce a fraction.


***

#### <code>FLOOR/</code> <sup><sub>[FUNCTION]</sub></sup><a name="floor/-function"></a>
<code>(<a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a>)</code>

Divide two integers and compute the floor of the quotient.


***

#### <code>ROUND/</code> <sup><sub>[FUNCTION]</sub></sup><a name="round/-function"></a>
<code>(<a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a>)</code>

Divide two integers and round the quotient.


***

#### <code>CEILING</code> <sup><sub>[FUNCTION]</sub></sup><a name="ceiling-function"></a>
<code>&forall; :A. <a href="#quantizable-class">QUANTIZABLE</a> :A &rArr; (:A &rarr; <a href="#integer-type">INTEGER</a>)</code>

Return the least integer greater than or equal to X.


***

#### <code>DOUBLE/</code> <sup><sub>[FUNCTION]</sub></sup><a name="double/-function"></a>
<code>(<a href="#double-float-type">DOUBLE-FLOAT</a> &rarr; <a href="#double-float-type">DOUBLE-FLOAT</a> &rarr; <a href="#double-float-type">DOUBLE-FLOAT</a>)</code>

Compute the quotient of single-precision floats A and B as a single-precision float.


***

#### <code>SINGLE/</code> <sup><sub>[FUNCTION]</sub></sup><a name="single/-function"></a>
<code>(<a href="#single-float-type">SINGLE-FLOAT</a> &rarr; <a href="#single-float-type">SINGLE-FLOAT</a> &rarr; <a href="#single-float-type">SINGLE-FLOAT</a>)</code>

Compute the quotient of single-precision floats A and B as a single-precision float.


***

#### <code>CEILING/</code> <sup><sub>[FUNCTION]</sub></sup><a name="ceiling/-function"></a>
<code>(<a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a>)</code>

Divide two integers and compute the ceiling of the quotient.


***

#### <code>INEXACT/</code> <sup><sub>[FUNCTION]</sub></sup><a name="inexact/-function"></a>
<code>(<a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a> &rarr; <a href="#double-float-type">DOUBLE-FLOAT</a>)</code>

Compute the quotient of integers A and B as a double-precision float.

Note: This does *not* divide double-float arguments.


***

## File: [cell.lisp](../src/library/cell.lisp)

### Types

#### <code>CELL :A</code> <sup><sub>[TYPE]</sub></sup><a name="cell-type"></a>
- <code>(CELL <a href="#lisp-object-type">LISP-OBJECT</a>)</code>

Internally mutable cell

Constructors:
- <code>CELL :: (<a href="#lisp-object-type">LISP-OBJECT</a> &rarr; (<a href="#cell-type">CELL</a> :A))</code>

<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> :A &rArr; <a href="#eq-class">EQ</a> (<a href="#cell-type">CELL</a> :A)</code>
- <code><a href="#num-class">NUM</a> :A &rArr; <a href="#num-class">NUM</a> (<a href="#cell-type">CELL</a> :A)</code>
- <code><a href="#into-class">INTO</a> (<a href="#cell-type">CELL</a> :A) :A</code>
- <code><a href="#into-class">INTO</a> :A (<a href="#cell-type">CELL</a> :A)</code>
- <code><a href="#functor-class">FUNCTOR</a> <a href="#cell-type">CELL</a></code>
- <code><a href="#semigroup-class">SEMIGROUP</a> :A &rArr; <a href="#semigroup-class">SEMIGROUP</a> (<a href="#cell-type">CELL</a> :A)</code>
- <code><a href="#applicative-class">APPLICATIVE</a> <a href="#cell-type">CELL</a></code>

</details>


***

### Values

#### <code>CELL-READ</code> <sup><sub>[FUNCTION]</sub></sup><a name="cell-read-function"></a>
<code>&forall; :A. ((<a href="#cell-type">CELL</a> :A) &rarr; :A)</code>

Read the value of a mutable cell


***

#### <code>CELL-SWAP</code> <sup><sub>[FUNCTION]</sub></sup><a name="cell-swap-function"></a>
<code>&forall; :A. (:A &rarr; (<a href="#cell-type">CELL</a> :A) &rarr; :A)</code>

Replace the value of a mutable cell with a new value, then return the old value


***

#### <code>MAKE-CELL</code> <sup><sub>[FUNCTION]</sub></sup><a name="make-cell-function"></a>
<code>&forall; :A. (:A &rarr; (<a href="#cell-type">CELL</a> :A))</code>

Create a new mutable cell


***

#### <code>CELL-WRITE</code> <sup><sub>[FUNCTION]</sub></sup><a name="cell-write-function"></a>
<code>&forall; :A. (:A &rarr; (<a href="#cell-type">CELL</a> :A) &rarr; <a href="#unit-type">UNIT</a>)</code>

Set the value of a mutable cell


***

#### <code>CELL-UPDATE</code> <sup><sub>[FUNCTION]</sub></sup><a name="cell-update-function"></a>
<code>&forall; :A. ((:A &rarr; :A) &rarr; (<a href="#cell-type">CELL</a> :A) &rarr; <a href="#unit-type">UNIT</a>)</code>

***

## File: [vector.lisp](../src/library/vector.lisp)

### Types

#### <code>VECTOR :A</code> <sup><sub>[TYPE]</sub></sup><a name="vector-type"></a>
- <code>(VECTOR <a href="#lisp-object-type">LISP-OBJECT</a>)</code>

Constructors:
- <code>VECTOR :: (<a href="#lisp-object-type">LISP-OBJECT</a> &rarr; (<a href="#vector-type">VECTOR</a> :A))</code>

<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> :A &rArr; <a href="#eq-class">EQ</a> (<a href="#vector-type">VECTOR</a> :A)</code>
- <code><a href="#iso-class">ISO</a> (<a href="#slice-type">SLICE</a> :A) (<a href="#vector-type">VECTOR</a> :A)</code>
- <code><a href="#iso-class">ISO</a> (<a href="#vector-type">VECTOR</a> :A) (<a href="#list-type">LIST</a> :A)</code>
- <code><a href="#into-class">INTO</a> (<a href="#vector-type">VECTOR</a> :A) (<a href="#slice-type">SLICE</a> :A)</code>
- <code><a href="#into-class">INTO</a> (<a href="#slice-type">SLICE</a> :A) (<a href="#vector-type">VECTOR</a> :A)</code>
- <code><a href="#into-class">INTO</a> (<a href="#vector-type">VECTOR</a> :A) (<a href="#list-type">LIST</a> :A)</code>
- <code><a href="#into-class">INTO</a> (<a href="#list-type">LIST</a> :A) (<a href="#vector-type">VECTOR</a> :A)</code>
- <code><a href="#functor-class">FUNCTOR</a> <a href="#vector-type">VECTOR</a></code>
- <code><a href="#semigroup-class">SEMIGROUP</a> (<a href="#vector-type">VECTOR</a> :A)</code>

</details>


***

### Values

#### <code>VECTOR-POP</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-pop-function"></a>
<code>&forall; :A. ((<a href="#vector-type">VECTOR</a> :A) &rarr; (<a href="#optional-type">OPTIONAL</a> :A))</code>

Remove and return the first item of V


***

#### <code>VECTOR-SET</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-set-function"></a>
<code>&forall; :A. (<a href="#integer-type">INTEGER</a> &rarr; :A &rarr; (<a href="#vector-type">VECTOR</a> :A) &rarr; <a href="#unit-type">UNIT</a>)</code>

Set the INDEXth element of V to ITEM. This function left intentionally unsafe because it does not have a return value to check.


***

#### <code>MAKE-VECTOR</code> <sup><sub>[FUNCTION]</sub></sup><a name="make-vector-function"></a>
<code>&forall; :A. (<a href="#unit-type">UNIT</a> &rarr; (<a href="#vector-type">VECTOR</a> :A))</code>

Create a new empty vector


***

#### <code>VECTOR-COPY</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-copy-function"></a>
<code>&forall; :A. ((<a href="#vector-type">VECTOR</a> :A) &rarr; (<a href="#vector-type">VECTOR</a> :A))</code>

Return a new vector containing the same elements as V


***

#### <code>VECTOR-HEAD</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-head-function"></a>
<code>&forall; :A. ((<a href="#vector-type">VECTOR</a> :A) &rarr; (<a href="#optional-type">OPTIONAL</a> :A))</code>

Return the first item of V


***

#### <code>VECTOR-LAST</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-last-function"></a>
<code>&forall; :A. ((<a href="#vector-type">VECTOR</a> :A) &rarr; (<a href="#optional-type">OPTIONAL</a> :A))</code>

Return the last element of V


***

#### <code>VECTOR-PUSH</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-push-function"></a>
<code>&forall; :A. (:A &rarr; (<a href="#vector-type">VECTOR</a> :A) &rarr; <a href="#integer-type">INTEGER</a>)</code>

Append ITEM to V and resize V if necessary


***

#### <code>VECTOR-SORT</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-sort-function"></a>
<code>&forall; :A. <a href="#ord-class">ORD</a> :A &rArr; ((<a href="#vector-type">VECTOR</a> :A) &rarr; <a href="#unit-type">UNIT</a>)</code>

Sort a vector inplace


***

#### <code>VECTOR-EMPTY</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-empty-function"></a>
<code>&forall; :A. ((<a href="#vector-type">VECTOR</a> :A) &rarr; <a href="#boolean-type">BOOLEAN</a>)</code>

Returns TRUE if V is empty


***

#### <code>VECTOR-INDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-index-function"></a>
<code>&forall; :A. (<a href="#integer-type">INTEGER</a> &rarr; (<a href="#vector-type">VECTOR</a> :A) &rarr; (<a href="#optional-type">OPTIONAL</a> :A))</code>

Return the INDEXth element of V


***

#### <code>VECTOR-APPEND</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-append-function"></a>
<code>&forall; :A. ((<a href="#vector-type">VECTOR</a> :A) &rarr; (<a href="#vector-type">VECTOR</a> :A) &rarr; (<a href="#vector-type">VECTOR</a> :A))</code>

Create a new VECTOR containing the elements of v1 followed by the elements of v2


***

#### <code>VECTOR-LENGTH</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-length-function"></a>
<code>&forall; :A. ((<a href="#vector-type">VECTOR</a> :A) &rarr; <a href="#integer-type">INTEGER</a>)</code>

Returns the length of V


***

#### <code>VECTOR-FOREACH</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-foreach-function"></a>
<code>&forall; :A :B. ((:A &rarr; :B) &rarr; (<a href="#vector-type">VECTOR</a> :A) &rarr; <a href="#unit-type">UNIT</a>)</code>

Call the function F once for each item in V


***

#### <code>VECTOR-SORT-BY</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-sort-by-function"></a>
<code>&forall; :A. ((:A &rarr; :A &rarr; <a href="#boolean-type">BOOLEAN</a>) &rarr; (<a href="#vector-type">VECTOR</a> :A) &rarr; <a href="#unit-type">UNIT</a>)</code>

Sort a vector with predicate function F


***

#### <code>VECTOR-CAPACITY</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-capacity-function"></a>
<code>&forall; :A. ((<a href="#vector-type">VECTOR</a> :A) &rarr; <a href="#integer-type">INTEGER</a>)</code>

Returns the number of elements that V can store without resizing


***

#### <code>VECTOR-FOREACH2</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-foreach2-function"></a>
<code>&forall; :A :B :C. ((:A &rarr; :B &rarr; :C) &rarr; (<a href="#vector-type">VECTOR</a> :A) &rarr; (<a href="#vector-type">VECTOR</a> :B) &rarr; <a href="#unit-type">UNIT</a>)</code>

Like vector-foreach but twice as good


***

#### <code>VECTOR-POP-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-pop-unsafe-function"></a>
<code>&forall; :A. ((<a href="#vector-type">VECTOR</a> :A) &rarr; :A)</code>

Remove and return the first item of V without checking if the vector is empty


***

#### <code>VECTOR-HEAD-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-head-unsafe-function"></a>
<code>&forall; :A. ((<a href="#vector-type">VECTOR</a> :A) &rarr; :A)</code>

Return the first item of V without first checking if V is empty


***

#### <code>VECTOR-LAST-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-last-unsafe-function"></a>
<code>&forall; :A. ((<a href="#vector-type">VECTOR</a> :A) &rarr; :A)</code>

Return the last element of V without first checking if V is empty


***

#### <code>VECTOR-SWAP-REMOVE</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-swap-remove-function"></a>
<code>&forall; :A. (<a href="#integer-type">INTEGER</a> &rarr; (<a href="#vector-type">VECTOR</a> :A) &rarr; (<a href="#optional-type">OPTIONAL</a> :A))</code>

Remove the element IDX from VEC and replace it with the last element in VEC. Then return the removed element.


***

#### <code>VECTOR-INDEX-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-index-unsafe-function"></a>
<code>&forall; :A. (<a href="#integer-type">INTEGER</a> &rarr; (<a href="#vector-type">VECTOR</a> :A) &rarr; :A)</code>

Return the INDEXth element of V without checking if the element exists


***

#### <code>MAKE-VECTOR-CAPACITY</code> <sup><sub>[FUNCTION]</sub></sup><a name="make-vector-capacity-function"></a>
<code>&forall; :A. (<a href="#integer-type">INTEGER</a> &rarr; (<a href="#vector-type">VECTOR</a> :A))</code>

Create a new vector with N elements preallocated


***

#### <code>VECTOR-FOREACH-INDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-foreach-index-function"></a>
<code>&forall; :A :B. ((<a href="#integer-type">INTEGER</a> &rarr; :A &rarr; :B) &rarr; (<a href="#vector-type">VECTOR</a> :A) &rarr; <a href="#unit-type">UNIT</a>)</code>

Call the function F once for each item in V with its index


***

#### <code>VECTOR-SWAP-REMOVE-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-swap-remove-unsafe-function"></a>
<code>&forall; :A. (<a href="#integer-type">INTEGER</a> &rarr; (<a href="#vector-type">VECTOR</a> :A) &rarr; :A)</code>

Remove the element IDX from VEC and replace it with the last element in VEC without bounds checking. Then return the removed element.


***

## File: [slice.lisp](../src/library/slice.lisp)

### Types

#### <code>SLICE :A</code> <sup><sub>[TYPE]</sub></sup><a name="slice-type"></a>
- <code>(SLICE <a href="#lisp-object-type">LISP-OBJECT</a>)</code>

Constructors:
- <code>SLICE :: (<a href="#lisp-object-type">LISP-OBJECT</a> &rarr; (<a href="#slice-type">SLICE</a> :A))</code>

<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> :A &rArr; <a href="#eq-class">EQ</a> (<a href="#slice-type">SLICE</a> :A)</code>
- <code><a href="#iso-class">ISO</a> (<a href="#slice-type">SLICE</a> :A) (<a href="#vector-type">VECTOR</a> :A)</code>
- <code><a href="#into-class">INTO</a> (<a href="#vector-type">VECTOR</a> :A) (<a href="#slice-type">SLICE</a> :A)</code>
- <code><a href="#into-class">INTO</a> (<a href="#slice-type">SLICE</a> :A) (<a href="#vector-type">VECTOR</a> :A)</code>

</details>


***

### Values

#### <code>SLICE-SET</code> <sup><sub>[FUNCTION]</sub></sup><a name="slice-set-function"></a>
<code>&forall; :A. (<a href="#integer-type">INTEGER</a> &rarr; :A &rarr; (<a href="#slice-type">SLICE</a> :A) &rarr; <a href="#unit-type">UNIT</a>)</code>

Set the element at INDEX in S to ITEM


***

#### <code>MAKE-SLICE</code> <sup><sub>[FUNCTION]</sub></sup><a name="make-slice-function"></a>
<code>&forall; :A. (<a href="#integer-type">INTEGER</a> &rarr; <a href="#integer-type">INTEGER</a> &rarr; (<a href="#vector-type">VECTOR</a> :A) &rarr; (<a href="#slice-type">SLICE</a> :A))</code>

***

#### <code>SLICE-COPY</code> <sup><sub>[FUNCTION]</sub></sup><a name="slice-copy-function"></a>
<code>&forall; :A. ((<a href="#slice-type">SLICE</a> :A) &rarr; (<a href="#slice-type">SLICE</a> :A))</code>

Returns a new slice containg the same elements as S


***

#### <code>SLICE-INDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="slice-index-function"></a>
<code>&forall; :A. (<a href="#integer-type">INTEGER</a> &rarr; (<a href="#slice-type">SLICE</a> :A) &rarr; (<a href="#optional-type">OPTIONAL</a> :A))</code>

Lookup the element at INDEX in S


***

#### <code>SLICE-LENGTH</code> <sup><sub>[FUNCTION]</sub></sup><a name="slice-length-function"></a>
<code>&forall; :A. ((<a href="#slice-type">SLICE</a> :A) &rarr; <a href="#integer-type">INTEGER</a>)</code>

Returns the length of S


***

#### <code>SLICE-FOREACH</code> <sup><sub>[FUNCTION]</sub></sup><a name="slice-foreach-function"></a>
<code>&forall; :A :B. ((:A &rarr; :B) &rarr; (<a href="#slice-type">SLICE</a> :A) &rarr; <a href="#unit-type">UNIT</a>)</code>

Call the function F once for each item in S


***

#### <code>SLICE-FOREACH2</code> <sup><sub>[FUNCTION]</sub></sup><a name="slice-foreach2-function"></a>
<code>&forall; :A :B :C. ((:A &rarr; :B &rarr; :C) &rarr; (<a href="#slice-type">SLICE</a> :A) &rarr; (<a href="#slice-type">SLICE</a> :B) &rarr; <a href="#unit-type">UNIT</a>)</code>

Iterate over S1 and S2 calling F once on each iteration


***

#### <code>VECTOR-CHUNKED</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-chunked-function"></a>
<code>&forall; :A :B. (((<a href="#slice-type">SLICE</a> :A) &rarr; :B) &rarr; <a href="#integer-type">INTEGER</a> &rarr; (<a href="#vector-type">VECTOR</a> :A) &rarr; <a href="#unit-type">UNIT</a>)</code>

Chunked iteration over a vector. Ignores elements at the end if the vector does not evenly divide by the chunk size.


***

#### <code>VECTOR-SLIDING</code> <sup><sub>[FUNCTION]</sub></sup><a name="vector-sliding-function"></a>
<code>&forall; :A :B. (((<a href="#slice-type">SLICE</a> :A) &rarr; :B) &rarr; <a href="#integer-type">INTEGER</a> &rarr; (<a href="#vector-type">VECTOR</a> :A) &rarr; <a href="#unit-type">UNIT</a>)</code>

Sliding iteration over a vector


***

#### <code>SLICE-INDEX-UNSAFE</code> <sup><sub>[FUNCTION]</sub></sup><a name="slice-index-unsafe-function"></a>
<code>&forall; :A. (<a href="#integer-type">INTEGER</a> &rarr; (<a href="#slice-type">SLICE</a> :A) &rarr; :A)</code>

Lookup the element at INDEX in S without bounds checking


***

#### <code>SLICE-FOREACH-INDEX</code> <sup><sub>[FUNCTION]</sub></sup><a name="slice-foreach-index-function"></a>
<code>&forall; :A :B. ((<a href="#integer-type">INTEGER</a> &rarr; :A &rarr; :B) &rarr; (<a href="#slice-type">SLICE</a> :A) &rarr; <a href="#unit-type">UNIT</a>)</code>

Call the function F once for each item in S with its index


***

## File: [hashtable.lisp](../src/library/hashtable.lisp)

### Types

#### <code>HASHTABLE :A :B</code> <sup><sub>[TYPE]</sub></sup><a name="hashtable-type"></a>
- <code>(HASHTABLE <a href="#lisp-object-type">LISP-OBJECT</a>)</code>

Constructors:
- <code>HASHTABLE :: (<a href="#lisp-object-type">LISP-OBJECT</a> &rarr; (<a href="#hashtable-type">HASHTABLE</a> :A :B))</code>


***

### Values

#### <code>HASHTABLE-GET</code> <sup><sub>[FUNCTION]</sub></sup><a name="hashtable-get-function"></a>
<code>&forall; :A :B. (:A &rarr; (<a href="#hashtable-type">HASHTABLE</a> :A :B) &rarr; (<a href="#optional-type">OPTIONAL</a> :B))</code>

Lookup KEY in TABLE


***

#### <code>HASHTABLE-SET</code> <sup><sub>[FUNCTION]</sub></sup><a name="hashtable-set-function"></a>
<code>&forall; :A :B. (:A &rarr; :B &rarr; (<a href="#hashtable-type">HASHTABLE</a> :A :B) &rarr; <a href="#unit-type">UNIT</a>)</code>

Set KEY to VALUE in TABLE


***

#### <code>HASHTABLE-KEYS</code> <sup><sub>[FUNCTION]</sub></sup><a name="hashtable-keys-function"></a>
<code>&forall; :A :B. ((<a href="#hashtable-type">HASHTABLE</a> :A :B) &rarr; (<a href="#vector-type">VECTOR</a> :A))</code>

Returns the keys in TABLE as a vector


***

#### <code>MAKE-HASHTABLE</code> <sup><sub>[FUNCTION]</sub></sup><a name="make-hashtable-function"></a>
<code>&forall; :A :B. (<a href="#unit-type">UNIT</a> &rarr; (<a href="#hashtable-type">HASHTABLE</a> :A :B))</code>

Create a new empty hashtable


***

#### <code>HASHTABLE-COUNT</code> <sup><sub>[FUNCTION]</sub></sup><a name="hashtable-count-function"></a>
<code>&forall; :A :B. ((<a href="#hashtable-type">HASHTABLE</a> :A :B) &rarr; <a href="#integer-type">INTEGER</a>)</code>

Returns the number of entries in TABLE


***

#### <code>HASHTABLE-REMOVE</code> <sup><sub>[FUNCTION]</sub></sup><a name="hashtable-remove-function"></a>
<code>&forall; :A :B. (:A &rarr; (<a href="#hashtable-type">HASHTABLE</a> :A :B) &rarr; <a href="#unit-type">UNIT</a>)</code>

Remove the entry at KEY from TABLE


***

#### <code>HASHTABLE-VALUES</code> <sup><sub>[FUNCTION]</sub></sup><a name="hashtable-values-function"></a>
<code>&forall; :A :B. ((<a href="#hashtable-type">HASHTABLE</a> :A :B) &rarr; (<a href="#vector-type">VECTOR</a> :B))</code>

Returns the values in TABLE as a vector


***

#### <code>HASHTABLE-ENTRIES</code> <sup><sub>[FUNCTION]</sub></sup><a name="hashtable-entries-function"></a>
<code>&forall; :A :B. ((<a href="#hashtable-type">HASHTABLE</a> :A :B) &rarr; (<a href="#vector-type">VECTOR</a> (<a href="#tuple-type">TUPLE</a> :A :B)))</code>

Returns the keys and values in TABLE as a vector


***

#### <code>HASHTABLE-FOREACH</code> <sup><sub>[FUNCTION]</sub></sup><a name="hashtable-foreach-function"></a>
<code>&forall; :A :B :C. ((:A &rarr; :B &rarr; :C) &rarr; (<a href="#hashtable-type">HASHTABLE</a> :A :B) &rarr; <a href="#unit-type">UNIT</a>)</code>

Call F once for each key value pair in TABLE


***

#### <code>MAKE-HASHTABLE-CAPACITY</code> <sup><sub>[FUNCTION]</sub></sup><a name="make-hashtable-capacity-function"></a>
<code>&forall; :A :B. (<a href="#integer-type">INTEGER</a> &rarr; (<a href="#hashtable-type">HASHTABLE</a> :A :B))</code>

Crate a new empty hashtable with a given capacity


***

## File: [graph.lisp](../src/library/graph.lisp)

### Types

#### <code>GRAPH :A :B</code> <sup><sub>[TYPE]</sub></sup><a name="graph-type"></a>
- <code>(GRAPH <a href="#graphtype-type">GRAPHTYPE</a> (<a href="#vector-type">VECTOR</a> (<a href="#node-type">NODE</a> :A)) (<a href="#vector-type">VECTOR</a> (<a href="#edge-type">EDGE</a> :B)))</code>

A graph using adjacency list representation

Constructors:
- <code>GRAPH :: (<a href="#graphtype-type">GRAPHTYPE</a> &rarr; (<a href="#vector-type">VECTOR</a> (<a href="#node-type">NODE</a> :A)) &rarr; (<a href="#vector-type">VECTOR</a> (<a href="#edge-type">EDGE</a> :B)) &rarr; (<a href="#graph-type">GRAPH</a> :A :B))</code>


***

#### <code>EDGEINDEX</code> <sup><sub>[TYPE]</sub></sup><a name="edgeindex-type"></a>
- <code>(EDGEINDEX <a href="#integer-type">INTEGER</a>)</code>

Constructors:
- <code>EDGEINDEX :: (<a href="#integer-type">INTEGER</a> &rarr; <a href="#edgeindex-type">EDGEINDEX</a>)</code>

<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#edgeindex-type">EDGEINDEX</a></code>
- <code><a href="#into-class">INTO</a> <a href="#edgeindex-type">EDGEINDEX</a> <a href="#integer-type">INTEGER</a></code>

</details>


***

#### <code>GRAPHTYPE</code> <sup><sub>[TYPE]</sub></sup><a name="graphtype-type"></a>
- <code>UNDIRECTED</code>
- <code>DIRECTED</code>

Constructors:
- <code>UNDIRECTED :: <a href="#graphtype-type">GRAPHTYPE</a></code>
- <code>DIRECTED :: <a href="#graphtype-type">GRAPHTYPE</a></code>


***

#### <code>NODEINDEX</code> <sup><sub>[TYPE]</sub></sup><a name="nodeindex-type"></a>
- <code>(NODEINDEX <a href="#integer-type">INTEGER</a>)</code>

Constructors:
- <code>NODEINDEX :: (<a href="#integer-type">INTEGER</a> &rarr; <a href="#nodeindex-type">NODEINDEX</a>)</code>

<details>
<summary>Instances</summary>

- <code><a href="#eq-class">EQ</a> <a href="#nodeindex-type">NODEINDEX</a></code>
- <code><a href="#into-class">INTO</a> <a href="#nodeindex-type">NODEINDEX</a> <a href="#integer-type">INTEGER</a></code>

</details>


***

### Values

#### <code>MAKE-GRAPH</code> <sup><sub>[FUNCTION]</sub></sup><a name="make-graph-function"></a>
<code>&forall; :A :B. (<a href="#unit-type">UNIT</a> &rarr; (<a href="#graph-type">GRAPH</a> :A :B))</code>

Create a new empty undirected graph


***

#### <code>GRAPH-EDGES</code> <sup><sub>[FUNCTION]</sub></sup><a name="graph-edges-function"></a>
<code>&forall; :A :B. ((<a href="#graph-type">GRAPH</a> :A :B) &rarr; (<a href="#vector-type">VECTOR</a> (<a href="#edge-type">EDGE</a> :B)))</code>

Returns the edges in a graph


***

#### <code>GRAPH-NODES</code> <sup><sub>[FUNCTION]</sub></sup><a name="graph-nodes-function"></a>
<code>&forall; :A :B. ((<a href="#graph-type">GRAPH</a> :A :B) &rarr; (<a href="#vector-type">VECTOR</a> (<a href="#node-type">NODE</a> :A)))</code>

Returns the nodes in a graph


***

#### <code>MAKE-DIGRAPH</code> <sup><sub>[FUNCTION]</sub></sup><a name="make-digraph-function"></a>
<code>&forall; :A :B. (<a href="#unit-type">UNIT</a> &rarr; (<a href="#graph-type">GRAPH</a> :A :B))</code>

Create a new directed graph


***

#### <code>GRAPH-ADD-EDGE</code> <sup><sub>[FUNCTION]</sub></sup><a name="graph-add-edge-function"></a>
<code>&forall; :A :B. (:A &rarr; <a href="#nodeindex-type">NODEINDEX</a> &rarr; <a href="#nodeindex-type">NODEINDEX</a> &rarr; (<a href="#graph-type">GRAPH</a> :B :A) &rarr; <a href="#edgeindex-type">EDGEINDEX</a>)</code>

Add an edge with associated data from node FROM to node TO in the graph.


***

#### <code>GRAPH-ADD-NODE</code> <sup><sub>[FUNCTION]</sub></sup><a name="graph-add-node-function"></a>
<code>&forall; :A :B. (:A &rarr; (<a href="#graph-type">GRAPH</a> :A :B) &rarr; <a href="#nodeindex-type">NODEINDEX</a>)</code>

Add a node with associated data to the graph, returning the index of the new node.


***

#### <code>GRAPH-EDGE-COUNT</code> <sup><sub>[FUNCTION]</sub></sup><a name="graph-edge-count-function"></a>
<code>&forall; :A :B. ((<a href="#graph-type">GRAPH</a> :A :B) &rarr; <a href="#integer-type">INTEGER</a>)</code>

Returns the number of edges in a graph


***

#### <code>GRAPH-LOOKUP-EDGE</code> <sup><sub>[FUNCTION]</sub></sup><a name="graph-lookup-edge-function"></a>
<code>&forall; :A :B. (<a href="#edgeindex-type">EDGEINDEX</a> &rarr; (<a href="#graph-type">GRAPH</a> :A :B) &rarr; (<a href="#optional-type">OPTIONAL</a> (<a href="#edge-type">EDGE</a> :B)))</code>

Lookup a node with index IDX in graph G


***

#### <code>GRAPH-LOOKUP-NODE</code> <sup><sub>[FUNCTION]</sub></sup><a name="graph-lookup-node-function"></a>
<code>&forall; :A :B. (<a href="#nodeindex-type">NODEINDEX</a> &rarr; (<a href="#graph-type">GRAPH</a> :A :B) &rarr; (<a href="#optional-type">OPTIONAL</a> (<a href="#node-type">NODE</a> :A)))</code>

Lookup a node with index IDX in graph G


***

#### <code>GRAPH-REMOVE-EDGE</code> <sup><sub>[FUNCTION]</sub></sup><a name="graph-remove-edge-function"></a>
<code>&forall; :A :B. (<a href="#edgeindex-type">EDGEINDEX</a> &rarr; (<a href="#graph-type">GRAPH</a> :A :B) &rarr; (<a href="#optional-type">OPTIONAL</a> :B))</code>

Remove an edge from GRAPH


***

#### <code>GRAPH-REMOVE-NODE</code> <sup><sub>[FUNCTION]</sub></sup><a name="graph-remove-node-function"></a>
<code>&forall; :A :B. (<a href="#nodeindex-type">NODEINDEX</a> &rarr; (<a href="#graph-type">GRAPH</a> :A :B) &rarr; (<a href="#optional-type">OPTIONAL</a> :A))</code>

Remove a node and all edges connecting to it from GRAPH


***

## File: [stateful-computation.lisp](../src/library/stateful-computation.lisp)

### Types

#### <code>STATEFULCOMPUTATION :A :B</code> <sup><sub>[TYPE]</sub></sup><a name="statefulcomputation-type"></a>
- <code>(STATEFULCOMPUTATION (:A &rarr; (<a href="#tuple-type">TUPLE</a> :A :B)))</code>

A computation of a value which may affect the state.
Represented as a closure from initial state to updated state and value.

Constructors:
- <code>STATEFULCOMPUTATION :: ((:A &rarr; (<a href="#tuple-type">TUPLE</a> :A :B)) &rarr; (<a href="#statefulcomputation-type">STATEFULCOMPUTATION</a> :A :B))</code>

<details>
<summary>Instances</summary>

- <code><a href="#monad-class">MONAD</a> (<a href="#statefulcomputation-type">STATEFULCOMPUTATION</a> :A)</code>
- <code><a href="#functor-class">FUNCTOR</a> (<a href="#statefulcomputation-type">STATEFULCOMPUTATION</a> :A)</code>
- <code><a href="#applicative-class">APPLICATIVE</a> (<a href="#statefulcomputation-type">STATEFULCOMPUTATION</a> :A)</code>

</details>


***

### Values

#### <code>STATEFUL-COMPUTATION-GET</code> <sup><sub>[VALUE]</sub></sup><a name="stateful-computation-get-function"></a>
<code>&forall; :A. (<a href="#statefulcomputation-type">STATEFULCOMPUTATION</a> :A :A)</code>

A StatefulComputation which returns the current state as the value.


***

#### <code>STATEFUL-COMPUTATION-PUT</code> <sup><sub>[FUNCTION]</sub></sup><a name="stateful-computation-put-function"></a>
<code>&forall; :A. (:A &rarr; (<a href="#statefulcomputation-type">STATEFULCOMPUTATION</a> :A <a href="#unit-type">UNIT</a>))</code>

A StatefulComputation with state set to be given state. The returned value is Unit.


***

#### <code>STATEFUL-COMPUTATION-RUN</code> <sup><sub>[FUNCTION]</sub></sup><a name="stateful-computation-run-function"></a>
<code>&forall; :A :B. ((<a href="#statefulcomputation-type">STATEFULCOMPUTATION</a> :A :B) &rarr; :A &rarr; (<a href="#tuple-type">TUPLE</a> :A :B))</code>

Runs a StatefulComputation to produce a final updated state and value given an initial state


***

