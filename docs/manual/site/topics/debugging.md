---
title: "Debugging"
description: "Type inspection, specialization introspection, and code generation tools."
hideMeta: true
weight: 55
---

Coalton is debugged as any other Lisp code. It has readable stack traces, works with profilers,
and so on. In addition, Coalton exposes a small set of debugging and inspection helpers. Some are used
inside Coalton expressions, while others are Common Lisp REPL tools for
inspecting the global environment or generated code.

If you are familiar with Common Lisp, then replace any `coalton-toplevel` with `pprint-coalton-codegen`
to look at the actual generated code. This is sometimes helpful in understanding what Coalton is actually
doing.

|   |   |   |   |
| --- | --- | --- | --- |
| [`coalton-codegen`](/manual/operators/coalton-codegen/) | [`coalton-codegen-ast`](/manual/operators/coalton-codegen-ast/) | [`coalton-codegen-types`](/manual/operators/coalton-codegen-types/) | [`describe-type-alias`](/manual/operators/describe-type-alias/) |
| [`describe-type-of`](/manual/operators/describe-type-of/) | [`kind-of`](/manual/operators/kind-of/) | [`pprint-coalton-codegen`](/manual/operators/pprint-coalton-codegen/) | [`pprint-coalton-codegen-ast`](/manual/operators/pprint-coalton-codegen-ast/) |
| [`pprint-coalton-codegen-types`](/manual/operators/pprint-coalton-codegen-types/) | [`print-specializations`](/manual/operators/print-specializations/) | [`set-type-printing-mode`](/manual/operators/set-type-printing-mode/) | [`type-of`](/manual/operators/type-of/) |
