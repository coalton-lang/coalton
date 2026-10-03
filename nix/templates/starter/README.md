# Coalton Starter Template
A template to ease the creation of new applications with Coalton and Common Lisp.

## Requirements
Ensure you have direnv installed.


## Getting Started

1. Replace references to `my-app` with your project's name:
   - .asd files/file name itself
   - .lisp file package definitions
   - flake.nix
2. `git init`: Create a repository and commit everything.
3. `direnv enable`: Allow the `.envrc`
4. Rewrite this README into something useful.


## Template Details
This template uses `cl-nix-lite` inside the flake to define a lisp system.
This includes a framework to use `fiasco` for testing the package.

The whole template is equipped with an `.envrc` file to allow use with `.direnv`, and a gitignore pre-configured for lisp, nix, and direnv.

## Structure

```
.
├── flake.nix
├── my-app.asd
├── README.md
├── src
│   └── main.lisp
└── tests
    └── main.lisp
```
