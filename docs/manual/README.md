# Coalton Manual Source

This directory is the source of truth for the Coalton language manual that is
published on the Hugo website.

## Layout

- `site/` contains Hugo-ready content.
- `site/_index.md` is the manual landing page.
- `site/operators/` contains one page per operator or operator-like form.
- `site/topics/` contains broader topic pages.

## Publishing

The website CI workflow copies `docs/manual/site/` into the
`coalton-website` repository at `content/manual/` and opens a pull request with
the updated manual alongside the generated standard library reference.
