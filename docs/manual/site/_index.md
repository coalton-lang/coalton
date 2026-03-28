---
title: "Coalton Language Manual"
outputs:
  - HTML
hideMeta: true
hideSectionList: true
weight: 1
---

Coalton is a statically typed functional language embedded in Common Lisp. This manual
is intended to be a flat, succinct guide to Coalton. It is hosted on
[GitHub](https://github.com/coalton-lang/coalton/tree/main/docs/manual), so if you find
an error or or something is unclear, please fix it and submit a PR.

## Articles

- [Introduction and Getting Started](/manual/topics/introduction/)
- [Whirlwind Tour of Coalton](/manual/topics/whirlwind-tour/)
- [Configuring Coalton](/manual/topics/configuring-coalton/)
- [Lisp Interop](/manual/topics/lisp-interop/)
- [Debugging Operators](/manual/topics/debugging/)
- [Standard Library Reference](/reference/)

## Operators And Symbols

Variables in Coalton can be named with almost any character; they typically have
no special meaning. For `+`, `/`, `==`, `<=>`, `>>=`, and other library operators,
see the [Standard Library Reference](/reference/).

|   |   |   |   |
| --- | --- | --- | --- |
| **Entry Points** | [`coalton`](/manual/operators/coalton/) | [`coalton-toplevel`](/manual/operators/coalton-toplevel/) |  |
| **Definitions** | [`define`](/manual/operators/define/) | [`define-class`](/manual/operators/define-class/) | [`define-exception`](/manual/operators/define-exception/) |
|  | [`define-instance`](/manual/operators/define-instance/) | [`define-resumption`](/manual/operators/define-resumption/) | [`define-struct`](/manual/operators/define-struct/) |
|  | [`define-type`](/manual/operators/define-type/) | [`define-type-alias`](/manual/operators/define-type-alias/) |  |
| **Directives** | [`declare`](/manual/operators/declare/) | [`derive`](/manual/operators/derive/) | [`inline`](/manual/operators/inline/) |
|  | [`likely`](/manual/operators/likely/) | [`monomorphize`](/manual/operators/monomorphize/) | [`noinline`](/manual/operators/noinline/) |
|  | [`repr`](/manual/operators/repr/) | [`specialize`](/manual/operators/specialize/) | [`unlikely`](/manual/operators/unlikely/) |
|  | [`unsafe`](/manual/operators/unsafe/) |  |  |
| **Types** | [`&key`](/manual/operators/keyword-tail/) | [`*`](/manual/operators/star/) | [`->`](/manual/operators/arrow/) |
|  | [`=>`](/manual/operators/constraint-arrow/) | [`forall`](/manual/operators/forall/) | [`the`](/manual/operators/the/) |
| **Expressions** | [`=`](/manual/operators/pattern-bind/) | [`[=>]`](/manual/operators/association-builder/) | [`[]`](/manual/operators/collection-builder/) |
|  | [`_`](/manual/operators/wildcard/) | [`dynamic-bind`](/manual/operators/dynamic-bind/) | [`fn`](/manual/operators/fn/) |
|  | [`let`](/manual/operators/let/) | [`let*`](/manual/operators/let-star/) | [`lisp`](/manual/operators/lisp/) |
|  | [`match`](/manual/operators/match/) | [`values`](/manual/operators/values/) |  |
| **Conditionals** | [`and`](/manual/operators/and/) | [`cond`](/manual/operators/cond/) | [`if`](/manual/operators/if/) |
|  | [`or`](/manual/operators/or/) | [`unless`](/manual/operators/unless/) | [`when`](/manual/operators/when/) |
| **Looping** | [`break`](/manual/operators/break/) | [`continue`](/manual/operators/continue/) | [`for`](/manual/operators/for/) |
|  | [`for*`](/manual/operators/for-star/) | [`rec`](/manual/operators/rec/) |  |
| **Control** | [`<-`](/manual/operators/do-bind/) | [`do`](/manual/operators/do/) | [`progn`](/manual/operators/progn/) |
|  | [`return`](/manual/operators/return/) |  |  |
| **Exceptions** | [`catch`](/manual/operators/catch/) | [`resumable`](/manual/operators/resumable/) | [`resume-to`](/manual/operators/resume-to/) |
|  | [`throw`](/manual/operators/throw/) |  |  |
| **Convenience** | [`.<`](/manual/operators/compose-right/) | [`.>`](/manual/operators/compose-left/) | [`as`](/manual/operators/as/) |
|  | [`nest`](/manual/operators/nest/) | [`pipe`](/manual/operators/pipe/) | [`try-as`](/manual/operators/try-as/) |
|  | [`unwrap-as`](/manual/operators/unwrap-as/) |  |  |
