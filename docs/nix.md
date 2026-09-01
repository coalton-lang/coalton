# Quickstart/TLDR
```sh
nix flake new --template "github:coalton-lang/coalton#starter" my-app
cd my-app
direnv allow
```

# Background Information
## Nix and Coalton

Nix is a purely functional package management system that works across platforms.
It isolates every package state precisely and stores each state independently, which eliminates “dependency hell.”
Since Coalton itself is heavily influenced by Haskell — another purely functional language — this approach should feel familiar and appealing to Coalton users.

For a detailed explanation of Nix, refer to its official website.
For now, it’s enough to understand that Nix is a convenient package management tool.
This article focuses on how to install Coalton using Nix.

## Issues with the Quicklisp Version of Coalton

Nix provides a huge number of registered packages, including Common Lisp implementations such as SBCL and CCL.
Quicklisp itself is mirrored, so it’s possible to use libraries from Quicklisp directly through SBCL without having Quicklisp installed.
Because Coalton is also registered in Quicklisp, it can be installed via Nix as well.

However, the Coalton package available through Quicklisp in Nixpkgs (the standard Nix package repository) has some problems.
First, Quicklisp updates Coalton irregularly. As of this writing (October 2025), the latest update is from June 2025.
Second, Nix mirrors Quicklisp irregularly as well.
Given the sheer number of packages in Nixpkgs and the limited resources for resolving build errors during mirroring, this delay is unavoidable.

As a result, the version of Coalton available in Nixpkgs is almost always outdated.
Since Coalton is currently under active development and it’s recommended to install it directly from the GitHub repository, using the Nixpkgs version is not ideal.
Nixpkgs itself is also not the ideal way to deal with lisp packages.
Quicklisp packages in nix sometimes fail to build properly, and the complex process can be hard to debug.


## Solving It with Flakes

We can solve this problem by adding a `flake.nix` file to our project repository.
The Coalton team has made the [github:coalton-lang/coalton](https://github.com/coalton-lang/coalton) repository available as a Nix third-party source.
This means users can simply add `"github:coalton-lang/coalton"` to their environment to install the latest version of Coalton.

This flake uses [`cl-nix-lite`](https://github.com/hraban/cl-nix-lite) to provide an easy path to using coalton from nix.
The [starter template's flake](../nix/templates/starter/flake.nix) is a good minimal example.
Install it with `nix flake init` or `nix flake new`.
