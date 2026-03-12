# Using coalton/doc

`coalton/doc` generates reference documentation for Coalton packages.

## Usage

```lisp
(ql:quickload :coalton/doc)

;; Generate docs for your own packages.
(coalton/doc:write-documentation
  #P"/tmp/my-library-reference.md"
  (list
   (coalton/doc/model:make-coalton-package
    (find-package '#:my-library))))

;; Generate the standard library reference.
(coalton/doc:write-stdlib-documentation-to-file
  #P"/tmp/coalton-reference.md")
```

`write-documentation` takes a filename and a list of
`coalton/doc/model:coalton-package` objects. Use
`coalton/doc/model:make-coalton-package` to wrap a Common Lisp package.

You can choose the output format with `:backend`:

- `:markdown` writes standalone Markdown.
- `:html` writes standalone HTML.
- `:hugo` writes Markdown with Hugo front matter for the website.

For example:

```lisp
(coalton/doc:write-stdlib-documentation-to-file
  #P"/tmp/coalton-reference.html"
  :backend :html)
```
