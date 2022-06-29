# Using coalton/doc

Coalton/doc is a tool to generate markdown documentation for coalton code.

## Usage

```lisp
(ql:quickload :coalton/doc)
(coalton-doc:write-documentation-to-file
  "filename.md"
  :packages '(package-1 package-2) ;; packages to generate documentation for as a list of symbols
  :asdf-system :system-a ;; asdf system as a symbol
  :file-link-prefix "https://github.com/you/your-project/tree/main/src/" ;; prefix for links to source files, note the trailing slash
```
