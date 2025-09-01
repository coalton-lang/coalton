# Intro to Coalton (日本語訳)

Coalton は、Common Lisp に埋め込まれ、Common Lisp にコンパイルされる静的型付け言語です。

このドキュメントは、関数型プログラミング言語に既に精通している方を対象としています。Common Lispでの経験がありCoaltonとの違いを知りたい人は[用語集](./glossary.md)も参照してください。

## システムとパッケージ

CoaltonのコードはCommon Lispと同じくパッケージ（およびASDFシステム）を使います。Coaltonを使うプロジェクトでは、ASDの`:depends-on`に`"coalton"`と`"named-readtables"`を追加してください。named-readtablesはエラーメッセージをわかりやすくするために必要です。

Coaltonの標準ライブラリは、一連のパッケージ群として提供されています。たとえば、文字列に関するものは`#:coalton-library/string`パッケージにあります。すべての標準ライブラリパッケージの一覧は[Coalton Reference](https://coalton-lang.github.io/reference)を参照してください。

Coaltonを使うパッケージでは、必要に応じて標準ライブラリをインポートします。このとき _`#:cl`または`#:common-lisp`パッケージは`:use`しないでください！_ 代わりに、少なくとも`#:coalton`パッケージ（コア言語機能用）と`#:coalton-prelude`（非常に一般的な標準ライブラリ定義用）の両方を`:use`します：

```lisp
(defpackage #:my-package
  (:use 
   #:coalton 
   #:coalton-prelude))
```

`#:coalton-prelude` パッケージには、標準ライブラリ内に存在しない機能は含まれておらず、単に利便性のためだけのものです。

ただ、この最小限のパッケージ定義だけでは実用的ではありません。たとえば、標準ライブラリの関数 `strip-prefix`（文字列から接頭辞を削除する関数）を使いたい場合、プログラム内で `coalton-library/string:strip-prefix` と入力しなければなりません。この長いパッケージ名を使う代わりに、このパッケージのローカルなニックネームを追加します：

```lisp
(defpackage #:my-package
  (:use 
   #:coalton)
  (:local-nicknames 
   (#:str #:coalton-library/string)))
```

これにより、単に `str:strip-prefix` と入力するだけですみます。Coalton 標準ライブラリだけでなくサードパーティのライブラリからであっても、そのパッケージのニックネームを `:local-nicknames` リストに追加できます。たとえば：

```lisp
(defpackage #:my-package
  (:use 
   #:coalton)
  (:local-nicknames 
   (#:str  #:coalton-library/string)
   (#:vec  #:coalton-library/vector)
   (#:math #:coalton-library/math)))
```

**`#:coalton` と `#:coalton-prelude` 以外の Coalton の組み込みパッケージを `:use` することは推奨しません。** Common Lisp では、パッケージが外部からシンボルをインポートすることで、歴史的に後方互換性の問題が発生してきました。さらに、Coalton は異なるパッケージで似た機能を持つ関数に対して同じシンボル名を使用しています。たとえば、stringパッケージとvectorパッケージの両方に `length` という名前のシンボルが存在し、つまり `str:length` と `vec:length` が両方存在します。これらを `:use` してしまうと `length` というシンボルが複数のパッケージ間で衝突してしまうことになります。

### REPLでCoaltonを使う

CoaltonをREPLで使いたい場合、パッケージを`#:coalton-user`に変更してください。具体的には次のようにします：

```lisp
(in-package #:coalton-user)
```

これは、REPLでの使用や実験のためだけに定義された便利なパッケージです。（このパッケージに依存するソフトウェアは存在すべきではありません。`#:cl-user`と同様です。）

次の章で説明しますが、`#:coalton-user` パッケージに変更したからといって、そのまま Coalton コードを入力できるようになるわけではありません。Coaltonのコードは `coalton-toplevel` または `coalton` フォームで囲う必要があります。

また、`#:coalton-user` パッケージは `#:common-lisp` パッケージを `:use` していないため、Common Lispのシンボルを使用する際は `cl:` で修飾する必要があります。

## プログラム構造

パッケージ `#:my-package` を作成したあとに、そのパッケージに切り替えるには次のようにします:

```lisp
(in-package #:my-package)

(named-readtables:in-readtable coalton:coalton)
```

`named-readtables:in-readtable` は必須ではありませんが、推奨されています。Coalton のリードテーブルを使うことで、コンパイラのエラーがソースコードの位置を正確に読み取り、正しい行番号を提供できるようになるからです。

Coaltonコードを記述するには2つのフォームを使います。

1つ目は定義などを記述するための主要なエントリポイントとなるもの、`coalton-toplevel` というトップレベルフォームです。

```lisp
(coalton-toplevel
  ;; <Coalton definition forms>
  )
```

現在、SLIME/SLYでは、`coalton-toplevel` 内の一部だけを `C-c-c` で実行する方法はありません。

2つ目の主要なエントリポイントは、Common LispのコードでCoaltonを呼び出す`coalton`演算子です：

```lisp
;; Lisp code
;; ...
     (coalton #|coalton expression|#)
;; ...
```

注意：`coalton` では、新しい定義を作成することはできず、式を評価するのみです。

`coalton-toplevel` では 1 つ以上のトップレベル定義または宣言を取りますが、`coalton` は単一の式を受け取って現在の環境に対して評価し、(結果として得られる) Common Lisp での値を返します。これは、REPL から Coalton を使用する際などに便利です。

Coalton パッケージ（`#:coalton-user` を含む）は、`#:common-lisp`/`#:cl` パッケージを `:use` していないことに注意してください。Common Lisp のシンボルを使用する場合は、`cl:` を接頭辞として付ける必要があります。

## 変数と関数

変数と関数は`define`で定義します。以下に変数の定義の例を示します。

```lisp
(coalton-toplevel
  ;; Variables are defined with the define keyword
  (define x 5)
  (define y 6)
  (define z (+ x y))
  (define p (Tuple 1.0 2.0))

  ;; Coalton supports integers, strings, booleans, and unit as primitive types
  (define name "Alyssa P. Hacker")
  (define hungry True)
  (define data Unit))
```

上記のタプル `p` の最初の要素は、REPL で `coalton` 演算子を使用して取り出せます:

```lisp
(coalton (fst p))
```

**注意**: `coalton` を省略してREPL で直接 `(fst p)` を評価したくなるかもしれませんが、この動作に依存しないでください！

関数は変数と同様に定義します。Common Lispとは違い、Coaltonの関数は変数と同じ名前空間を使います。これにより、高階関数プログラミングがより容易になります。

```lisp
(coalton-toplevel
  ;; Functions are also defined with the define keyword
  (define (add2 x)
    (+ 2 x))

  ;; Functions exist in the same namespace as variables
  (define addTwo add2)

  (define x (addTwo 3))

  ;; Anonymous functions can be defined with fn
  (define z (map (fn (x) (+ 2 x)) (make-list 1 2 3 4))))
```

### 関数とカリー化

Coalton の _すべての_ 関数は、_1 つの入力_ を受け取り、_1 つの出力_ を生成します。次の関数を考えてみましょう：

```lisp
(coalton-toplevel
  (define (fma a b c)
    (+ c (* a b))))
```

実は、`fma` は厳密には _1つの引数 `a`_ のみを受け取ります。Common Lisp ではこの関数はおよそ次のような関数に相当します：

```lisp
(defun fma (a)
  (lambda (b)
    (lambda (c)
      (+ c (* a b)))))
```

ただしCoaltonでは、必要な場合を除いて、この内部実装を隠蔽しています：

```lisp
(coalton-toplevel
  (define fma1 (fma 2))      ; equiv: b -> (c -> (c + 2*b))
  (define fma2 (fma 2 3))    ; equiv: c -> (c + 6)
  (define fma3 (fma 2 3 4))) ; equiv: 10
```

`fma`を3つの引数を持つ関数として呼び出すことができますが、これは単なる便利な構文に過ぎません。より少ない引数で呼び出すことも可能です。この性質は、_カリー化された関数_ と呼ばれることもあります。

Coaltonでは、この機能を最適化して、可能な限りクロージャを作らないようにします。実際、`(fma x y z)`は、クロージャを作らず単純な加算と乗算としてコンパイルされます。

以下は、カリー関数を使用してリストを変換する例です。

```lisp
(coalton-toplevel
  ;; Lists can be created with the make-list macro
  (define nums (make-list 2 3 4 5)))

(coalton
  ;; Functions in coalton are curried
  (map (+ 2) nums)) ;; 4 5 6 7
```

### パイプ構文と関数合成

関数合成のために `pipe` と `nest` マクロという便利な構文が用意されています。

```lisp
(nest f g ... h x)

;; is equivalent to

(f (g (... (h x))))

;; is equivalent to

(pipe x h ... g f)
```

これらは、コードの可読性を向上させるのに便利です。

これらのマクロは（可変長引数であることからそうわかるように）、高階関数としては使用できません。高階関数として使いたい場合は、カリー化または`compose`関数を検討してください。

### 関数の引数の無視

以下のような関数について考えましょう。

```lisp
(coalton-toplevel
  (define (f x y)
    x))
```

ここで、`y` は使用されていないため以下のような警告が発生します：

```
COMMON-LISP:WARNING: warn: Unused variable
  --> <unknown>:3:15
   |
 3 |    (define (f x y)
   |                 ^ variable defined here
help: prefix the variable with '_' to declare it unused
 3 |   (define (f x _y)
   |                --
```

提案されたように、`y` を `_y` に置き換えることで、Coalton コンパイラにその引数が意図的に使用されていないことを伝えることができます。

**注意**: `_` で始まる変数（例: `_y`）は通常の変数であり、読み取ることができます。以下のコードは有効な Coalton です:

```lisp
(define (f _x) _x)
```

アンダーバーで始まる変数は、可能な限り無視されるものとみなすべきです。使用される可能性がある場合は、アンダーバーで始まらない名前を使用すべきです。アンダーバーで始まる変数の読み取りは許容されています。これにより、生成されたコード（例：マクロや読み取り条件式を使用する場合）は、一部のコンパイル環境では使用されるが他の環境では使用されない変数に対して、未使用変数警告を回避できます。

## データ型

Coaltonでは、型パラメータ付き代数的データ型を定義できます。

```lisp
(coalton-toplevel
  ;; New types are created with the DEFINE-TYPE operator
  (define-type Point3D (Point3D Integer Integer Integer))

  ;; Coalton supports sum types
  (define-type Color
    Red
    Blue
    Green)

  ;; Coalton supports generic type variables
  ;;
  ;; Type parameters are defined using keyword arguments
  (define-type (Tree :a)
    (Branch (Tree :a) :a (Tree :a))
    (Leaf :a)))
```

型定義は型コンストラクタも定義します。たとえば、次のようにクリスマスツリーのような木を作れます。

```lisp
(coalton
  (Branch (Leaf Red)
          Green
          (Branch (Leaf Red) Green (Leaf Red))))
```

このドキュメントの後半では、`match` を使用してこれらの型を展開する方法について説明します。

## 型エイリアス

Coaltonでは、パラメータ付き型エイリアスの定義が可能です。型エイリアスは、プリミティブ型および`define-type`または`define-type-alias`で定義された型に対して定義できます。

```lisp
(coalton-toplevel
  ;; New type aliases are created with the DEFINE-TYPE-ALIAS operator
  (define-type-alias Coordinate Integer)
  (define-type-alias (Pair :a) (Tuple :a :a))
  (define-type-alias Translation (Pair Coordinate -> Pair Coordinate))

  (declare shift-right Translation)
  (define (shift-right (Tuple x y))
    (Tuple (1+ x) y))

  (define shifted-coordinate (shift-right (Tuple 0 0))))

  ;; Type aliases can have multiple parameters
  (define-type-alias (MyTuple3 :a :b :c) (Tuple :a (Tuple :b :c)))

  ;; Type aliases can have parameters that do not have a kind of *
  (define-type-alias (IntegerCollection :col) (:col Integer))

  ;; Type aliases can alias types that do not have a kind of *
  (define-type-alias MyCollection List)
```

パラメータ付き型エイリアスの引数はすべて指定する必要があります。

```lisp
(coalton-toplevel

  (define-type (T :a) (ConstrT (:a Integer)))

  (define-type-alias (MyCollection1 :a) (List :a))
  (define-type-alias MyCollection2 List)

  ;; This line will not compile, because MyCollection1 has a
  ;; parameter :A which is not applied
  (define-type-alias A (T MyCollection1))

  ;; However, this line will compile
  (define-type-alias A (T MyCollection2)))
```

型エイリアスを扱う際に役立つデバッグツールがいくつかあります。Coalton式の外では、`describe-type-of` を使用すると、シンボルの型（エイリアスを含む）を表示し、その型を返します。`describe-type-alias` は、エイリアスとエイリアス先の型を表示し、エイリアス先の型を返します。さらに、Coalton は、シンボルに関連付けられた型を表示する際、エイリアスのみを表示するか、型のみを表示するか、または両方を表示するように設定できます。この設定は、Coalton をコンパイルする前に `(setf (get ':coalton-config ':type-printing-mode) mode)` を使用して設定できます。指定できる `mode` は `:types`、`:aliases`、`:types-and-aliases` のいずれかです。その後、関数 `set-type-printing-mode` を使用して、これらの3つのオプションの間でモードを変更できます。デフォルトのモードは `:types` です。

```lisp
COALTON-USER> (coalton-toplevel
                (define-type-alias A Integer)
                (define x (the A 5)))
; No value

COALTON-USER> (set-type-printing-mode :aliases)
:ALIASES

COALTON-USER> (type-of 'x)
A

COALTON-USER> (set-type-printing-mode :types-and-aliases)
:TYPES-AND-ALIASES

COALTON-USER> (type-of 'x)
[A := INTEGER]

COALTON-USER> (set-type-printing-mode :types)
:TYPES

COALTON-USER> shifted-coordinate ;; from the example above
#.(TUPLE 1 0)

COALTON-USER> (type-of 'shifted-coordinate)
(TUPLE INTEGER INTEGER)

COALTON-USER> (describe-type-of 'shifted-coordinate)
[(PAIR COORDINATE) := (TUPLE [COORDINATE := INTEGER] [COORDINATE := INTEGER])]

COALTON-USER> (describe-type-alias 'Pair)
[(PAIR :A) := (TUPLE :A :A)]
```

### 構造体

構造体は以下のように定義できます。

```lisp
(coalton-toplevel
  (define-struct Point
    (x Integer)
    (y Integer)))
```

構造体は単一コンストラクタを持つ抽象データ型（ADT）に似ており、同様の方法でオブジェクトを作成できます：

```lisp
(coalton (Point 1 2))
```

フィールドアクセサは、個々のフィールドを読み取るために使います：

```lisp
(coalton (.x (Point 1 2)))
```

フィールドアクセサは値として関数に渡すこともできます:

```
(coalton (map .x (make-list (Point 1 2) (Point 2 3))))
```

構造体にも型パラメータをつけられます：

```lisp
(coalton-toplevel
  (define-struct (Pair :a)
    (first :a)
    (second :a)))
```

## ループと反復処理

Coaltonにはループを実行する方法が主に3つあります：

* 末尾再帰と `rec`
* ビルトインのIteratorベースのループ
* 実験的なLispのような繰り返しマクロ (例: `dotimes`)

### 末尾再帰と `rec`

Coaltonは末尾再帰が可能な限り除去されるようにします。そのため、Coaltonでは慣用的に末尾再帰を使ってループします。

```lisp
(coalton-toplevel
  (define (find-integer predicate limit)
    (cond
      ((negative? limit) False)
      ((predicate limit) True)
      (True              (find-integer predicate (1- limit))))))
```

この関数はスタックを増やさないため、任意の大きな`limit`で実行できます：

```lisp
> (coalton (find-integer (fn (x) (== 65536 (* x x))) 1000000))
COMMON-LISP:T
```

CoaltonにはSchemeの「named-let」風の反復処理を行うための特別な組み込み演算子`rec`があります。その構文は次の通りです：

```lisp
(rec <name> (<binding>*)
  <body>)

;; <name> := <symbol>
;;         | (<symbol> <type>)
;;
;; <binding> := (<symbol> <value>)
```

`<name>` は本質的に `<body>` 内のローカル（末尾再帰）関数です。加えてこの関数の型を宣言すると不必要なポリモーフィズムも避けることができます。`rec` 演算子は呼び出しが末尾位置にある必要はありません。

Coaltonでは、他に妥当な名前がなければ`%`を使うのが慣例です。

以下は `rec` を使ってリストの要素を逆順にするコード例です。

```lisp
(coalton-toplevel
  (define (rev l)
    (rec % ((result Nil) (remaining l))
      (match remaining
        ((Nil) result)
        ((Cons x xs) (% (Cons x result) xs))))))
```

以下は、任意の数値を受け取ってフィボナッチ数を返す関数の例です：

```lisp
(coalton-toplevel
  (define (fib-polymorphic n)
    (rec % ((i n) (a 0) (b 1))
      (cond
        ((== 0 i) a)
        ((== 1 i) b)
        (True (% (- i 1) b (+ a b)))))))
```

この関数の型は `∀ A B. (NUM B) (NUM A) ⇒ (A → B)` で、あらゆる数値型に使用できます:

```
> (coalton (the F32 (fib-polymorphic (the Integer 10))))
55.0
```

再帰関数の型を宣言すると`Integer`用の単一型にできます。

```lisp
(define (fib-monomorphic n)
  (rec (% (Integer -> Integer -> Integer -> Integer))
       ((i n)
        (a 0)
        (b 1))
    (cond
      ((== 0 i) a)
      ((== 1 i) b)
      (True (% (- i 1) b (+ a b))))))
```

これで、使用時に型宣言が不要になります:

```
> (coalton (fib-monomorphic 10))
55
```

### 組み込みループ構文

Coaltonは無限ループ、条件付きループ、および`for`ループスタイルの反復処理をサポートしています。

### `loop`、`while`、`while-let`、`for`

以下は無限ループの例です。

```lisp
(loop (trace "hi"))
```

以下は、ある条件が真である間だけループします。

```lisp
(coalton
 (let ((counter (cell:new 0))
       (limit 10))
   (while (< (cell:read counter) limit)
     (trace "hi") 
     (cell:increment! counter))))
```

以下は、パターンがマッチする限り、ループします。

```lisp
(coalton
 (let ((xs (vector:make 4 3 2 1)))
   (while-let (Some x) = (vector:pop! xs)
              (traceobject "x" x))))
```

`IntoIterator` のインスタンスに対してループするには以下のようにします。

```lisp
(coalton
 (for x in "coalton"
      (traceobject "x" x)))
```

### `break` と `continue`

上記のすべてのループは、`break` と `continue` をサポートしています。

`break` は、反復処理を即座に終了します。  以下のコードは、`c`、`o`、`a` を表示して終了します。

```lisp
(coalton
 (for x in "coalton"
      (when (== x #\l)
        (break))
      (traceobject "x" x)))
```

`continue` 構文はループの残りの本体をスキップし、次の反復を開始します。
以下の例では、`l` の出力をスキップし、`c`、`o`、`a`、`t`、`o`、および `n` を出力します。

```lisp
(coalton
 (for x in "coalton"
      (when (== x #\l)
        (continue))
      (traceobject "x" x)))
```

### ループラベル

上記の各ループ形式は、任意のループラベルをとります。これらのラベルは、`break`と`continue`と組み合わせて使用することで、複雑な制御フローを実現できます。

ループラベルはループの開始直後におけます：

```lisp 
(loop :outer (do-stuff))

(while :a-label (is-true?) (do-stuff))

(while-let :another-label 
   (Some thing) = (get-something)
   (do-stuff thing))

(for :iter word in words 
   (do-stuff-with word))
```

以下の完全に実用的ではない例では、最も外側のループは`:outer`というラベルでマークされています。このラベルは、内側の`while`ループ内から`break`で参照され、 `acc` と `counter` の合計が500を超えるたびに反復を終了させます。`:outer`ラベルがなければ、`break`は内側の`while`ループからしか抜け出せません。

```lisp
(coalton 
  (let ((counter (cell:new 0))
        (acc (cell:new Nil)))
    (loop :outer
          (while (< (cell:increment! counter) 10)
            (let x = (fold + (cell:read counter) (cell:read acc)))
            (when (< 500 x)
              (break :outer))
            (when (== 0 (mod (cell:read counter) 3)) 
              (continue))
            (cell:push! acc x))
          (when (< (length (cell:read acc)) 500)
            (cell:swap! counter 0)
            Unit))
    (cell:read acc)))
```

## 数値

Coaltonはいくつかの数値型をサポートしています。主なものは`Integer`、`F32`、および`F64`です。

```lisp
(coalton-toplevel
  (define num-int 5)
  (define num-sf 5.0f0)
  (define num-df 5.0d0))
```

接尾辞を省略して `5.0` とだけ書くこともできます。これは、`cl:*read-default-float-format*` のread-time値に応じて精度が決まります。
この変数はデフォルトで `cl:single-float` に設定されており、装飾のない浮動小数点数は
single floatで表されます。

固定幅の符号付き整数型（`I32` と `I64`）や固定幅の符号なし整数型（`U8`、
`U32`、`U64`）など、より制限された整数型も存在します。

最後に、`Fraction` (分数)という名前のratio型があります。これは、2つの `Integer` 値の比率を表すものです。

数値は`Num`型クラスを実装しており、この型クラスには`+`、`-`、`*`、および`fromInt`メソッドが定義されています。

### 割り算 (概略)

割り算は複雑です。詳細は次のセクションを参照してください。ただし、以下にいくつかの簡単なヒントをまとめました。

* 除算演算子 `/` およびその仲間は、除数が 0 の場合に実行時エラーが発生します。`Optional` 型の返り値が欲しい場合は `safe/` を使用してください。
* 整数からdouble floatを取得したい場合は `inexact/` を使用してください。（single floatの場合は `into` を使います。）
* 整数から整数の結果が欲しい場合は、`floor/`、`ceiling/`、または `round/` を使用してください。
* 2つの整数を正確な有理数で割る場合は、`exact/` を使用してください。
* 割り算を使用する汎用的なコードを書いている場合、または抽象性を保ちたい場合は、`/` をどのように正しく扱うかを学びましょう。

### 割り算 (詳細)

なぜ`Num`型クラスには除算（`/`）が含まれていないのでしょうか？

Coaltonには除算演算子`/`は存在しますが、これは他とは違った、やや複雑なものです。除算が難しい理由は2つあります：

1. 0のような数で割ると除算が失敗する。
2. 2つの数を割っても、必ずしも同じ型の結果が得られるとは限りません。（実際、除算自体が不可能な場合もあります！）

1.の場合、除算は実行時エラーを引き起こす可能性があります。私たちは数学的な文脈では扱いが非常に面倒なので`Optional` を使用しません。（除算によるゼロ除算チェックを行い、`Optional` を返すバリエーションとして `safe/` を使用することも可能です。）

2.の問題に対応するため、新しい型クラス`Dividable`を紹介します。型式：

```
(Dividable :s :t)
```

これは `:s` 型の2つの数での除算は `:t` 型の項目を生成しうる、という意味です。これらすべてを考慮すると、`/` の最終的な型が得られます。

```
COALTON-USER> (type-of '/)
∀ :A :B. DIVIDABLE :A :B ⇒ (:A → :A → :B)
```

[インスタンスのデフォルト設定](#instance-defaulting)のため、指定がない場合、`Integer`定数の除算はデフォルトで`F64`除算になります：

```
COALTON-USER> (coalton (/ 1 2))
0.5d0
```

Coalton に、私たちの定数が別の型であることを伝えるには、`the` で制約を課すか、型推論に頼ります。例えば、`Integer` 入力から F64 以外の結果を得るには、結果の型を任意の型に制約する必要があります（その型が `Dividable` 型クラスの定義済みインスタンスを持っていれば）：

```
COALTON-USER> (coalton (the F32 (/ 4 2)))
2.0
COALTON-USER> (coalton (the Fraction (/ 4 2)))
#.(COALTON-LIBRARY::%FRACTION 2 1)
```

`/` 演算子による除算の結果として `Integer` 型を返すことはできません。これは、インスタンス `Dividable Integer Integer` が定義されていないためです:

```
COALTON-USER> (coalton (the Integer (/ 4 2)))
; error: Unable to codegen
;   --> repl input:1:22
;    |
;  1 |  (COALTON (THE INTEGER (/ 4 2)))
;    |                        ^^^^^^^ expression has type ∀. (RECIPROCABLE INTEGER) => INTEGER with unresolved constraint (RECIPROCABLE INTEGER)
;    |                        ------- Add a type assertion with THE to resolve ambiguity
;    [Condition of type COALTON-IMPL/TYPECHECKER/BASE:TC-ERROR]
```

なぜ単に `2` にならないのでしょうか？残念ながら、その理由は `/` が常に整数 `2` を生成するとは限らないためです。また、正確に割り切れない場合、Coalton は特定の丸め方を強制しません。したがって、正確に割り切る適切な方法は、 `floor`、`ceiling`、または `round` を使用して丸めることです。

```
COALTON-USER> (coalton (floor (the Fraction (/ 4 2))))
2
```

値が均等に割り切れない場合、何が起こるかを見てみましょう。

```
COALTON-USER> (coalton (floor (the Fraction (/ 3 2))))
1
COALTON-USER> (coalton (ceiling (the Fraction (/ 3 2))))
2
COALTON-USER> (coalton (round (the Fraction (/ 3 2))))
2
```

これらのユースケースはすべて一般的なため簡略表現があります：

* `safe/` は除算によるゼロ除算チェックを行い、その場合 `None` を返します。
* `exact/` は整数から分数への除算（上記の `the Fraction` に関する処理にすべて置き換えられます）、
* `inexact/` は整数から倍精度浮動小数点数への除算、
* `floor/`、`ceiling/`、および `round/` は整数から整数への除算

分数は、`fromfrac` を使用して他の徐算可能な形式に変換できます（注：これにより精度が失われる可能性があります）：

```
COALTON-LIBRARY/MATH/REAL> (coalton (the F64 (fromfrac 1/2)))
0.5d0
COALTON-LIBRARY/MATH/REAL> (coalton (the F32 (fromfrac 999/1000)))
0.999
```

## リスト

Coaltonは内部でLispのリストを使用しています。リストは`make-list`で作成できます。

```lisp
(coalton-toplevel
  (define x (make-list 1 2 3))
  (define y (make-list "a" "b" "c")))
```

リストのすべての要素の型は同じでなければいけません。つまり、以下のコードは型エラーを引き起こします。

```
COALTON-USER> (coalton-toplevel
                (define wut (make-list 1.0d0 2.0d0 3.0)))
; error: Type mismatch
;   --> repl input:3:4
;    |
;  3 |      (MAKE-LIST 1.0d0 2.0d0 3.0)))
;    |      ^^^^^^^^^^^^^^^^^^^^^^^^^^^ Expected type '(LIST DOUBLE-FLOAT)' but got '(LIST SINGLE-FLOAT)' 
;    [Condition of type COALTON-IMPL/TYPECHECKER/BASE:TC-ERROR]
```

リストは`match`を使用して分解することもできます。

```lisp
(coalton-toplevel
  (define (is-empty lst)
    (match lst
      ((Cons _ _) "is not empty")
      ((Nil) "is empty"))))
```

## 静的型付け

Coalton コードは静的に型チェックされます。型は推論されます。

```lisp
(coalton-toplevel
  (define (fun x)
    (map (+ 2) (str:parse-int x))))
```

変数または関数の型は、`coalton:type-of` を使用して確認できます。

```
COALTON-USER> (type-of 'fun)
(STRING -> (OPTIONAL INTEGER)
```

型宣言を手動で追加することもできます。

```lisp
(coalton-toplevel
  (declare fun (String -> (Optional Integer)))
  (define (fun x)
    (map (+ 2) (str:parse-int x))))
```

型宣言は、let 式にも追加できます。

```lisp
(coalton-toplevel
  (define (f a b)
    (let ((declare g (Integer -> Integer -> Integer))
          (g +))
      (g a b))))
```

### 完全な型変換と不完全な型変換

Coaltonは、Common Lispの関数`cl:coerce`と同様の型変換を、`#:coalton-library/classes`パッケージの`Into`という型クラスとその唯一のメソッド`into`で実現しています。ただし、`into`メソッドは単一の引数のみを受け取ります。Coalton は、どのデータ型に変換すべきかをどのように判断するのでしょうか？これは、周囲の文脈による型推論（例えば、この例では `substring` が `String` を期待している場合）によって決定されます。

```lisp
(coalton-toplevel
  (define integer-part (substring (into 12.34d0) 0 2)))

;; ==> "12"
```

または、`the` を使った明示的な宣言を使うと：

```lisp
(coalton-toplevel
  (define unique-letters
    (remove-duplicates (the (List Char) (into "mississippi")))))

;; ==> (#\m #\s #\p #\i)
```

`the` と `into` パターンは非常に一般的であるため、Coalton では `as` という省略形が提供されています。上記の例は、次のようにより簡潔に記述できます：

```lisp
(coalton-toplevel
  (define unique-letters
    (remove-duplicates (as (List Char) "mississippi"))))

;; ==> (#\m #\s #\p #\i)
```

`into` メソッドは、ある型から別の型への変換が常に可能である場合のみ使用されます。もし型の値が変換できない場合、`TryInto` という別の型クラスと `tryInto` メソッドが使用されます。`tryInto` メソッドは、変換が成功したかどうかを示す `Result` 型を返します。

**注意：`as` は `into` 経由の変換（つまり、完全な変換）にのみ機能します。** `tryInto` に対応する構文は存在しません。

## パターンマッチング

`match` 式は、パターンマッチングと代数的データ型の分解に使用できます。

```lisp
(coalton-toplevel
  (define-type Color
    Red
    Blue
    Green)

  ;; Constructors must be wrapped in parentheses
  (declare color-to-string (Color -> String))
  (define (color-to-string c)
    (match c
      ((Red) "Red")
      ((Blue) "Blue")
      ((Green) "Green")))

  ;; Variables are not wrapped in parentheses
  (declare map-optional ((:a -> :b) -> (Optional :a) -> (Optional :b)))
  (define (map-optional f x)
    (match x
      ((Some x_) (Some (f x_)))
      ((None) None)))

  ;; Patterns can be nested, and wildcard "_" patterns are supported
  (declare flatten-optional ((Optional (Optional :a)) -> (Optional :a)))
  (define (flatten-optional x)
    (match x
      ((Some (Some x_)) (Some x_))
      (_ None)))

  ;; Submatches can be captured in a variable
  (declare dedup-head (Eq :a => List :a))
  (define (dedup-head xs)
    "If the first and second member of list are equal, drop the first"
    (match xs
      ((Cons a (= tl1 (Cons b _))) 
       (if (== a b) tl1 xs))
      (_ xs)))

  ;; Integers or Strings can also be matched on
  (define (is-5-or-7 x)
    (match x
      (5 True)
      (7 True)
      (_ False)))

  (define (is-five-or-seven x)
    (match x
      ("five"  True)
      ("seven" True)
      (_ False))))
```

関数は引数に対してパターンマッチングを行うことができますが、すべてのパターンを網羅する必要があります。

```lisp
(coalton-toplevel
  (declare first (Tuple :a :b -> :a))
  (define (first (Tuple a _)) a)

  (declare second (Tuple :a :b -> :b))
  (define second (fn ((Tuple _ b)) b))

  ;; pattern capture works here too
  (declare nest-right (Tuple :a :b -> (Tuple :a (Tuple :a :b))))
  (define (nest-right (= tpl (Tuple a _))) (Tuple a tpl)))
```

 `coalton-library:if` は、真偽値の比較を行う際の省略形として使えます：

```lisp
(coalton-toplevel
  (define (is-even x)
    (if (== 0 x)
        True
        (is-odd (- x 1))))

  (define (is-odd x)
    (if (== 0 x)
        False
        (is-even (- x 1)))))
```

複数の`if`式は`cond`で書くこともできます：

```lisp
(coalton-toplevel
  (define (fizz-buzz n)
    (cond
      ((and (== 0 (mod n 5))
            (== 0 (mod n 3)))
            "Fizzbuzz")
      ((== 0 (mod n 3))
        "Fizz")
      ((== 0 (mod n 5))
        "Buzz")
      (True (into n)))))
```

`coalton-library` の真偽演算子 `and` と `or` は、実際には必要最小限だけ評価する可変引数マクロです。関数バージョンの `boolean-and` と `boolean-or` もあります。

```lisp
(coalton
  (or (cheap 5) True (really-expensive (expt 2 1000000))))
```

この場合、`really-expensive` は評価が省略され決して呼び出されません。`and` と `or` はどちらも1つ以上の引数を取ります。

## `COALTON-LIBRARY:PROGN`

Coaltonには、Common Lispの `cl:progn` に似た`coalton-library:progn`構文が用意されています。

```lisp
(coalton-toplevel
  (declare f (Integer -> Integer -> (Tuple Integer Integer)))
  (define (f x y)
    (progn
      (+ x y)
      (* x y)
      (Tuple x y))))
```

Coaltonの`progn`では、特殊な`let`構文を使用できます。

```lisp
(coalton-toplevel
 (declare f (Integer -> Integer -> String))
 (define (f x y)
   (progn
     (let x_ = (into x))
     (let y_ = (into y))
     (<> x_ y_))))
```

このフラットなlet式ではパターンマッチングもできます:

```lisp
(coalton-toplevel
  (declare f (Tuple Integer Integer -> Integer))
  (define (f t)
    (let (Tuple fst snd) = t)
    (+ fst snd)))
```

フラットな let 式は再帰的ではなく、let の多相性をサポートしません。したがって、以下の式は無効です：

```
(coalton
  (progn
    (let id = (fn (x) x))
    (id Unit)
    (id "hello")))
```

これは、標準のlet式を使って以下のように書けます：

```
(coalton
  (let ((id (fn (x) x)))
    (progn
      (id Unit)
      (id "hello"))))
```

関数定義は暗黙の `progn` ブロックを持ちます

```lisp
(coalton-toplevel
  (declare f (Integer -> Integer -> String))
  (define (f x y)
    (let x_ = (into x))
    (let y_ = (into y))
    (<> x_ y_)))
```

## `Unless` と `When`

`coalton-library` パッケージには、Common Lisp での定義と類似した動作をする `unless` と `when` も含まれています。
これらの演算子は、副作用を含む操作を条件分岐するためだけに使用することを推奨します。

```lisp
(coalton-toplevel
  (define (f x)
    (when (== x 5)
      (error "I only want the number 5"))))
```

`unless` と `when` はどちらも暗黙の `progn` ブロックを持ちます。

```lisp
(coalton-toplevel
  (define (f b)
    (when b
      (let x = 5)
      (let y = 7)
      (traceObject "sum" (+ x y)))))
```

## 即時Return

関数は`return`を使用して早期に終了することができます。

```lisp
(coalton-toplevel
  (define (fizz-buzz n)
    (when (== 0 (mod n 15))
      (return "fizzbuzz"))

    (when (== 0 (mod n 3))
      (return "fizz"))

    (when (== 0 (mod n 5))
      (return "buzz"))

    (into n)))
```

## 型クラス

Coaltonは型クラスをサポートしています。

現在、各型クラスインスタンスに対して、_すべての_ メンバー関数を定義する必要があります。

```lisp
(coalton-toplevel
  ;; Type classes are defined with the define-class keyword
  (define-class (Eq :a)
    (== (:a -> :a -> Boolean)))

  (define-type Color
    Red
    Green
    Blue)

  ;; Type class instances are defined with the define-instance keyword
  (define-instance (Eq Color)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Red) (Red)) True)
        ((Tuple (Blue) (Blue)) True)
        ((Tuple (Green) (Green)) True)
        (_ False)))
    (define (/= a b) (not (== a b))))

  ;; Type declarations can have constraints
  (declare is-eql (Eq :a => (:a -> :a -> String)))
  (define (is-eql a b)
    (if (== a b)
      "They are equal"
      "They are not equal"))

  ;; Multiple constraints must be wrapped in parentheses
  (declare double-is-eql ((Eq :a) (Eq :b) => (:a -> :a -> :b -> :b -> String)))
  (define (double-is-eql a b c d)
    (if (and (== a b) (== c d))
      "Both pairs are equal"
      "The pairs are not both equal")))
```

## 組み込みの型クラス

以下のものは、標準ライブラリで定義されている主な型クラスです。

* `Eq` - 比較可能な型として定義される
* `Ord` - 順序付け可能な型として定義される
* `Num` - 数値型として定義される
* `Semigroup` - 結合法則を満たす二項演算をサポートする型として定義される
* `Monoid` - 結合的二項演算を持ち、かつ単位元を持つ型として定義される

以下の各型クラスは、Haskellの同名のクラスとほぼ同じ構造を持っていますが、わずかな違いがあります。

* `Functor` - `fmap` は Coalton では単に `map` です
* `Applicative`
* `Monad` - モナドには `return` はありません。代わりに Applicative の `pure` を使用してください
* `Alternative` - `<|>` は Coalton では `alt` と呼ばれます
* `Foldable`
* `Traversable`

以下の型クラスは、Rustの同名のトレイトから着想を得たものです：

* `Into` - 1つの型から別の型への完全な変換
* `TryInto` - 1つの型から別の型への不完全な変換

## 派生した型クラスインスタンスの自動定義

一部の型クラスのインスタンスは、ユーザー定義の型に対して自動的にメンバー関数が定義されるように派生させることができます。

```lisp
(coalton-toplevel
  (derive Eq Hash)
  (define-struct Point
    (x UFix)
    (y UFix)))
```

これで`Point`構造体に対して`==`メソッドと`hash`メソッドが使えます:

```lisp
(coalton
  ;; Test `Point' equality
  (== (Point 1 2) (Point 3 3))

  ;; Make a map using `Point' as a key
  (let map = (the (hashmap:HashMap Point UFix) hashmap:empty))
  (hashmap:insert map (Point 0 0) 1))
```

派生により生成されるインスタンスは、それぞれにおいて「自明な」ものとなります。たとえば、等価性チェックでは、すべてのサブフィールドが等しいかどうかが検証されます。これはすべての型に対して望ましいとは限らないため、場合によってはカスタムインスタンスが必要になることがあります。

### 組み込みの派生型クラス

以下のクラスのインスタンスを派生させることができます：

* `Eq`
* `Hash`
* `Default`

今のところ、標準ライブラリで派生可能なクラスはこれらのみですが、将来的に追加されるかもしれません。

ユーザー定義の型クラスを派生させるための公式のAPIはまだ存在しませんが、冒険心のある方であれば比較的簡単に実現可能です。詳細な手順については、[derivers.lisp](./../library/derivers.lisp) を参照してください。

## `Do` マクロ

Coaltonには、Haskellの `do` 表現と類似した動作をする`do`マクロが用意されています。

```lisp
(coalton-toplevel
  (define (f ax bx)
    (do
      (a <- ax)
      (b <- bx)
      (let c = (+ a b))
      (pure c)))

    ;; [6+3, 5+3, 4+3, 6+2, 5+2, 4+2, 6+1, 5+1, 4+1]
   (define xs (f (make-list 1 2 3) (make-list 4 5 6))))
```

## インライン型アノテーション

型クラスを使用する際の曖昧さを解消するために、インライン型アノテーションを追加することができます。

```lisp
(coalton-toplevel
  (define f (the U32 (+ (fromInt 5) (fromInt 7)))))
```

## 省略関数構文

Coaltonには引数なし関数のための特別な構文はありません。ただし、型シグネチャが`Unit -> *`である関数は、Coaltonでは明示的に`Unit`を渡さなくても呼び出すことができます。たとえば、`(coalton-library/vector:new)`は、`(coalton-library/vector:new Unit)`の省略形です。


関数は、`(fn () 5)` のような形式で暗黙のパラメーターを使用して定義することもできます。これで暗黙に `Unit` を1つパラメーターとして取る関数が作成されます。

## Coaltonでのデバッグ

`coalton` パッケージは、いくつかのデバッグ関数を定義しています。

`type-of` と `kind-of` は、定義の型と種類を確認するために使えます。

```
COALTON-USER> (type-of 'map)
∀ :A :B :C. FUNCTOR :C ⇒ ((:A → :B) → (:C :A) → (:C :B))
COALTON-USER> (kind-of 'Result)
* -> (* -> *)
```

以下の関数はすべて、任意のパッケージを引数に取ります。

* `print-type-db` - すべての既知の型を表示する
* `print-value-db` - すべてのトップレベル値の型を表示する
* `print-class-db` - すべてのクラスとそのメソッドを表示する
* `print-instance-db` - すべてのクラスのインスタンスを表示する

## インスタンスのデフォルト設定

CoaltonはHaskellと同様の[型デフォルトシステム](https://www.haskell.org/onlinereport/decls.html#sect4.3.4)を採用しています。型デフォルトは、暗黙的に型付けされた定義と`coalton`マクロでコンパイルされたコードに対して適用されます。デフォルトは、曖昧な型変数を有効な型に解決する目的で、曖昧な述語のセットに適用されます。Coaltonは、述語の1つ以上が数値型クラス（`Num`、`Quantizable`、`Reciprocable`、`ComplexComponent`、`Remainder`、`Integral`）の場合のみデフォルトを適用でき、曖昧な変数を`Integer`、`F32`、または`F64`のいずれかにデフォルト設定します。これは、その型変数を参照するすべての述語に対して有効な最初の型を選択するものです。Coaltonは、曖昧な変数を含む述語の1つ以上が複数パラメータ型クラスである場合、デフォルト設定を行いません。

Haskell 98 との違い。Haskell は `Num (List :a)` を曖昧なものと見なしますが、Coalton はデフォルトで `Num Integer` とします。Haskell は (`Num :a` `CustomTypeClass :a`) を曖昧なものと見なしますが、Coalton は `CustomTypeClass Integer` が有効なインスタンスであると仮定し、デフォルトで (`Num Integer` `CustomTypeClass Integer`) とします。

## 関数従属

関数従属は、クラスの型変数の関係を指定することで、型推論を改善するものです。

クラス `C` には、次のように関数従属 `(:a -> :b)` を定義できます:

`(define-class (C :a :b (:a -> :b)))`

`(:a -> :b)` はこのように解釈できます：各 `:a` に対して、対応する `:b` はただ一つに定まる、言い換えれば `:b` の値は `:a` によって一意に決定されます。

インスタンス `(C String Integer)` が定義されている場合、`(C String Char)` を定義することはできません。なぜなら、同じ `:a` の値に対して `:b` の値が複数存在するからです。

クラスには複数の関数従属が存在し、各依存関係は `(:a :b -> :c :d :e)` のように両側に複数のクラス変数を列挙できます。また、依存関係は `(:a -> :b) (:b -> :a)` のように再帰的でも構いません。

## 特殊化 (Specialization)

Coaltonは、型に基づく関数の特殊化をサポートしています。関数の特殊化は`specialize`形式で宣言されます：

```
(coalton-toplevel
  (declare inc (Num :a => :a -> :a))
  (define (inc x)
    (trace "standard call")
    (+ x 1))

  (declare inc-int (Integer -> Integer))
  (define (inc-int x)
    (trace "int specialized call")
    (+ x 1))

  (specialize inc inc-int (Integer -> Integer)))
```

`inc` が整数引数で呼び出された場合、その関数呼び出しは `inc-int` に置き換えられます。

```
COALTON-USER> (coalton (inc 1.2))
standard call
2.2
COALTON-USER> (coalton (inc 1))
int specialized call
2
```

特殊化は、呼び出し時の引数の型が既知の場合にのみ適用可能です。特殊化されることは保証されないため、特殊化した関数はその非特殊版と同じ動作をしなければなりません。特殊化はパフォーマンス向上のためだけに使用すべきです。以下の例を見てください：

```
(coalton-toplevel
  (declare inc2 (Num :a => :a -> :a))
  (define (inc2 x)
    (inc x)))
```

`inc2` の引数の `x` の型はわからないので、特殊化はされません。

```
COALTON-USER> (coalton (inc2 1))
standard call
2
```

特殊化は、`print-specializations` コマンドでREPLに表示できます。

## Common Lispとの違い

* Coaltonでは`True`（`t`ではなく）を使います。そのため、`t`は通常の変数名として使用可能です。
* 等価性を判定する際、Coaltonでは二重等号`==`を使います。
* Coaltonのリストの要素の型は同一でなければなりません。
* 無名関数を表す際、Coaltonでは`fn`（`lambda`ではなく）を使います。
* `+`などの数値演算子は2つの引数のみを受け付けます。
* 否定は `negate` で行います。 `(- x)` は、`(fn (z) (- x z))` と同等のカリー関数です。

詳しくは [glossary](./glossary.md) を見てください。

# 実験的な機能

Coaltonにはさらに以下の機能をありますが、さらに改善の余地があります。

## 例外処理 (Exception)

Coaltonには、例外の定義、シグナル、ハンドルおよび例外のリスタート(Resumption)を行うための構文が用意されています。

要約すると、関連する構文は以下の通りです：

* `define-exception`: 例外の種類を定義します。名前を除けば、構文は `define-type` と同一です。
* `define-resumption`: 名前付きリスタートを定義します。
* `catch`: 例外をキャッチして処理する式です。ハンドラーは例外コンストラクターに対してパターンマッチングを行います。
* `throw`: 例外を発生させます。
* `resumable`: 例外を中断してリスタートする式です。リスタートは中断された再開コンストラクタでパターンマッチングにより実行されます。
* `resume-to`: リスタートインスタンスを受け取る式です。指定されたリスタートのハンドラーを含む `resumable` ブロックに制御を移します。

Coaltonの例外処理システムは未完成であり、現在も進化の途中です。この設計は実験を行い、機能の成熟のための先方互換性のために追加されました。以下の「注意事項」セクションも参照してください。

### 例外の定義、throwとcatch

Common Lispのエラーを含むすべての例外をキャッチしたい場合は、ワイルドカードパターンを使用できます：

```lisp
(declare divide-by-random (Integer -> Integer -> Integer))
(define (divide-by-random r m)
    "Divide `r` by a random integer between `0` and `m`. 
     If the divisor is `0`, then print the divide by zero error
     and then return `0.0`"
    (catch (lisp Integer (r m) (cl:/ r (cl:random m)))
        (_ (trace "An error was received")
           0)))
```

より一般的に

```lisp 
(define-type Egg
    ;;     cracked? cooked?
    (Goose Boolean Boolean)
    (Xenomorph))

  ;; We define an exception type BadEgg with a few variants 
  (define-exception BadEgg
    (UnCracked Egg)
    (DeadlyEgg Egg))

  ;; If we try to crack open a Xenomorph egg, throw a DeadlyEgg error
  (declare crack (Egg -> Egg))
  (define (crack egg)
    (match egg
      ((Goose _ cooked?)
       (Goose True cooked?))
      ((Xenomorph)
       (throw (DeadlyEgg egg)))))

  ;; crack an egg open safely. 
  (declare crack-safely (Egg -> (Result BadEgg Egg)))
  (define (crack-safely egg)
    (catch (Ok (crack egg))
      ((DeadlyEgg _) (Err (DeadlyEgg egg)))
      ((UnCracked _) (Err (UnCracked egg)))))
```

### リスタート (Resumption) の定義、発行、および処理

リスタート機能により、Coaltonプログラマーはコールスタックを巻き戻すことなくエラーから復帰できます。

`define-resumption` 形式は単一の「コンストラクタ」を取ります。コンストラクタの名前は、リスタートの型の名前と同じです。

以下の例は、上記の例をより明確にしたものです。

```lisp
(define-resumption SkipEgg)
  (define-resumption (ServeRaw Egg) 
    "Suggest the egg be served raw.")

  (declare cook (Egg -> Egg))
  (define (cook egg)
    (let ((badegg (Uncracked egg)))     ; exceptions can be constructed outside throw
      (match egg
        ((Goose (True) _)  (Goose True True))
        ((Goose (False) _) (throw badegg))
        ((Xenomorph)       (throw (DeadlyEgg egg))))))

  ;; Return None if a SkipEgg resumption is received.
  (declare make-breakfast-with (Egg -> (Optional Egg)))
  (define (make-breakfast-with egg)
    (resumable (Some (cook (crack egg)))
      ((SkipEgg) None)))
```

次に、`n` 人分の朝食を作る関数を定義します。この関数は各卵を調理しようとしますが、致命的な卵に遭遇してエラーが発生した場合、その卵をスキップして `make-breakfast` を再開します。

```lisp 
(declare make-breakfast-for (UFix -> (Vector Egg)))
  (define (make-breakfast-for n)
    (let ((eggs (vector:make))
          (skip SkipEgg))              ; can construct outside of resume-to
      (for i in (iter:up-to n)
        (let egg = (if (== 0 (mod i 5)) Xenomorph (Goose False False)))
        (do
         (cooked <- (catch (make-breakfast-with egg)
                      ((DeadlyEgg _)    (resume-to skip))))
         (pure (vector:push! cooked eggs))))
      eggs))
```

毎回5個目の卵は有毒なので、10人分の朝食を作る場合、8個の卵が調理されます。

コールスタックは次のようになっています。

```
make-breakfast-for 
      │
      └─ make-breakfast-with 
                │
                └─ cook
```

しかし、`cook` は `Xenomorph` の卵に対して `DeadlyEgg` エラーを発生させます。
`make-breakfast-for` はそのエラーをキャッチし、`SkipEgg` を実行します。そこで `make-breakfast-with` はそのリスタートを受け取って処理します。

### 注意事項

当面、以下のことに注意してください。

1. `throw` や `resume-to` 式に対するポリモーフィズムのサポートはありません。例えば、次のコードは型注釈なしではコンパイルできません：
   - `(define (th a) (throw a))`
   - `(define (res a) (resume-to a))`
2. `catch`ハンドラーの中で、Lispのエラーを`catch`して変数にバインドすることはできません。Lispのエラーはワイルドカードパターンを使用してキャッチできます。つまり、これはLispの例外をリスタートすることはできないことを意味します。例外を再度投げるには再度作り直す必要があります。例:
   - `(catch (bad-thing) (_ Unit))`
   - `(catch (bad-thing) ((MyBad x) (trace 「my bad」) (throw (MyBad x))))`
3. `resumable` はさらに厳格に制限されます。リスタートコンストラクタパターン以外の
パターンと一致させることはできません。
4. 例外のシグナルには型クラスが関連付けられていません。私たちは、コールスタックを飛び越える可能性のある形式の静的チェックに異なるアプローチも検討しています。最終的に、型クラスが採用されるかもしれません。いずれにせよ、既存の構文と意味論と互換性を保つよう努めます。
