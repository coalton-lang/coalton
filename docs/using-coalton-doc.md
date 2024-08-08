# Using coalton/doc

Coalton/doc is a tool to generate markdown documentation for coalton code.

## Usage

```lisp
(ql:quickload :coalton/doc)
(coalton/doc:write-documentation-to-file
  "filename.md"
  :packages '(package-1 package-2)) ;; packages to generate documentation for as a list of symbols
```
