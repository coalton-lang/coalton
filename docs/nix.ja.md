# Nix プロジェクトで Coalton を使う

この記事では、Nix flake を使って最新の Coalton を導入し、SBCL から利用する方法を紹介します。

## Nix と Coalton

nix とは、クロスプラットフォームで動作する純粋関数型パッケージ管理システムです。
パッケージの全ての状態を厳密に区別し、状態ごとにパッケージを富豪的に格納することで、依存関係地獄が発生しない利点があります。純粋関数型を標榜するパッケージマネージャは Haskell に強く影響をうけた Coalton を使うユーザーにとっても魅力的でしょう。さて、詳しい説明は Nix 公式のウェブページに任せます。ひとまず便利なパッケージ管理ツールがあると理解していただければ問題ありません。ここでは Nix で Coalton をインストールする方法について解説します。

## Quicklisp 版 Coalton の課題

Nix には、たくさんのパッケージが登録されており、もちろん SBCL や CCL といった Common Lisp の処理系も Nix でインストールすることができます。さらに、Quicklisp もミラーされており、Quicklisp に登録されているライブラリを Quicklisp なしに SBCL から使える環境を構築されています。そして Coalton も Quicklisp に登録されていることから Nix 経由で Coalton をインストールすることができます。

しかし、Quicklisp 経由で Nixpkgs (Nix の標準的なパッケージリポジトリ) に登録された Coalton には問題があります。第一に Quicklisp が Coalton の更新を取り込む頻度が不定期であること。本稿執筆時点 (2025 年 10 月) 時点での最新の更新は 2025 年 6 月の Coalton です。

第二に、Nix の Quicklisp のミラーリングも不定期であること。Nix には膨大なパッケージが登録されていることもあり、限られたリソースで Nixpkgs に Quicklisp のミラーリング (ビルドエラーの解決を含む) を行うタイミングが不定期になってしまうのはしかたありません。

さてこの 2 つ問題から Nix に登録されている Coalton は常に古い Coalton となってしまっています。とくに Coalton は現在活発に更新されており、GitHub にあるソースコードから直接インストールすることを推奨されている現状を踏まえると、
Nixpkgs にある Coalton を使うことは厳しいでしょう。

## Flake による解決

われわれは 開発リポジトリに `flake.nix` を置くことでこの問題を解決します。
[github:coalton-lang/coalton](https://github.com/coalton-lang/coalton) 自体を Nix のサードパティリポジトリとして公開することにしました。これにより、Coalton のユーザーは自身の開発環境にに `"github:coalton-lang/coalton"` を登録することで、最新の Coalton をインストールすることができるようになります。

ここでは Nix flake をつかった 設定方法を説明します。

## リポジトリの追加方法

ここでは 純粋関数型のパッケージ定義の入力 として `coalton` を設定します。
ただし Coalton 側のパッケージで便宜上定義されている、ベースとなる中央リポジトリ (つまり Nixpkgs) を上書きする指示 `inputs.nixpkgs.follows = "nixpkgs"` を追加します。これにより、最新の Coalton を使いつつ、ベースとなる中央リポジトリを柔軟に変更することができます。

```nix
inputs = {
  nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  coalton = {
    url = "github:coalton-lang/coalton";
    inputs.nixpkgs.follows = "nixpkgs";
  }
}
```

## オーバーレイの追加

オーバーレイの概念を理解するのは少し複雑です。なぜなら、Common Lisp と Nix の間にある複雑な関係を理解する必要があるからです。ご存知の通り Common Lisp のライブラリの依存関係の解決のデファクトスタンダードは ASDF です。
この ASDF は、Common Lisp の処理系内部に密接に結合しています。
この蜜結合は、Common Lisp の処理系の傘下にライブラリが配置される、というような依存関係を描きます。
つまり、Cのように実行ファイルや実行環境とライブラリが対等に存在する形にはなりません。
要するに、Common Lisp はライブラリを **処理系の中に取り込んで** 使うため、Nix 側でも**Coalton 入りの SBCL** という特別なパッケージを動的に作る必要があります。この既存のパッケージ（SBCL）をカスタマイズする仕組みがオーバーレイです。

以下は Nixpkgs 内の各 Common Lisp 処理系を Coalton を追加した状態に上書きするコードです。

```nix
let
  pkgs = import nixpkgs {
    inherit system;
    overlays = [
      inputs.coalton.overlays.default
    ];
  };
```

以上で 最新の Coalton を nixpkgs に追加する操作は終わりです。

## Coalton を SBCL で有効化する。

上記までで Coalton を追加することはできました。
最後に 追加したパッケージを有効化する方法を以下に示します。
冒頭で述べたとおり、Quicklisp に登録されているライブラリは
ミラーされているので、Coalton 同様に有効化することができます。

```nix
let
  sbcl-with-coalton = pkgs.sbcl.withPackages (ps: with ps; [
    coalton
    fiasco
    named-readtables
  ]);
```

## 簡単な例

では、実際に SBCL での簡単な使い方を示します。

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    coalton = {
      url = "github:coalton-lang/coalton";
      inputs.nixpkgs.follows = "nixpkgs";
    }
  };
  outputs = { self, nixpkgs, coalton, ... }:
    let
      system = "x86_64-linux";  # or "aarch64-darwin"
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          coalton.overlays.default
        ];
      };
      sbcl-with-coalton = pkgs.sbcl.withPackages (ps: with ps; [
        coalton
      ]);
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = [ sbcl-with-coalton ];
      };
    };
  };
}
```

## Nix を経由した Coalton の使い方

Nix flake の設定が完了したら、開発シェルを起動して Coalton を利用できる環境に入ります。
以下の手順で実際に確認してみましょう。

```sh
$ nix develop
```

このコマンドを実行すると、`flake.nix` に定義された環境（`devShell`）が立ち上がり、
`sbcl-with-coalton` が含まれた状態のシェルに入ります。

次に、Common Lisp の処理系 SBCL を起動します。


```sh
$ sbcl
```

以下のように SBCL 上で Coalton を読み込みます。

```lisp
* (require :asdf)
* (require :coalton)
* (in-package :coalton-user)
* ;; you can use coalton!
```

これで Coalton を SBCL 上で利用できるようになります。
上記の手順を終えると、`coalton-user` パッケージに入り、
通常の Common Lisp の REPL から Coalton の構文や関数を使って開発を進められます。

