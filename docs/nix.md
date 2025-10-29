# Using Coalton in a Nix Project

This article explains how to install the latest version of Coalton using a Nix flake and how to use it from SBCL.

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

## Solving It with Flakes

We can solve this problem by adding a `flake.nix` file to our project repository.
The Coalton team has made the [github:coalton-lang/coalton](https://github.com/coalton-lang/coalton) repository available as a Nix third-party source.
This means users can simply add `"github:coalton-lang/coalton"` to their environment to install the latest version of Coalton.

The following section explains how to configure this using a Nix flake.

## Adding the Repository

We’ll define `coalton` as an input to our purely functional package definition.
However, we’ll also add a directive `inputs.nixpkgs.follows = "nixpkgs"` to make Coalton follow our base repository (Nixpkgs).
This ensures that while using the latest Coalton, we can flexibly change the base repository.

```nix
inputs = {
  nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  coalton = {
    url = "github:coalton-lang/coalton";
    inputs.nixpkgs.follows = "nixpkgs";
  };
};
```

## Adding an Overlay

The concept of overlays can be a bit tricky, because it requires understanding the complex relationship between Common Lisp and Nix.
As you may know, Common Lisp uses ASDF as the de facto standard for managing library dependencies.
ASDF is tightly coupled with the Lisp implementation itself, meaning libraries are often installed under the implementation’s directory tree.
In contrast to languages like C, where libraries and executables exist independently, Common Lisp loads libraries **into** the implementation.

Therefore, in Nix, we must dynamically create a special package — for example, **SBCL with Coalton preloaded**.
Overlays provide the mechanism to customize existing packages (like SBCL) in this way.

The following code overlays Coalton into each Common Lisp implementation in Nixpkgs:

```nix
let
  pkgs = import nixpkgs {
    inherit system;
    overlays = [
      inputs.coalton.overlays.default
    ];
  };
```

With this, the latest Coalton has been added to Nixpkgs.

## Enabling Coalton in SBCL

Now that Coalton has been added, we can enable it in SBCL.
As mentioned earlier, Quicklisp-mirrored libraries can be enabled in the same way.

```nix
let
  sbcl-with-coalton = pkgs.sbcl.withPackages (ps: with ps; [
    coalton
    fiasco
    named-readtables
  ]);
```

## A Simple Example

Here’s a simple example showing how to use Coalton in SBCL:

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-25.05";
    coalton = {
      url = "github:coalton-lang/coalton";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs:
    let
      system = "x86_64-linux";  # or "aarch64-darwin"
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.coalton.overlays.default
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
}
```

## Using Coalton via Nix

Once your Nix flake configuration is ready, you can enter the development shell and start using Coalton.

```sh
$ nix develop
```

This command launches the development environment defined in your `flake.nix`, entering a shell that includes `sbcl-with-coalton`.

Next, start the SBCL Common Lisp environment:

```sh
$ sbcl
```

Then load Coalton in SBCL:

```lisp
* (require :asdf)
* (require :coalton)
* (in-package :coalton-user)
* ;; you can use coalton!
```

Now Coalton is available in SBCL.
You’re inside the `coalton-user` package and can use Coalton’s syntax and functions directly from the Common Lisp REPL.

