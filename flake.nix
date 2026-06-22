{
  description = "A flake for coalton";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    cl-nix-lite.url = "github:hraban/cl-nix-lite/v0";
    cl-nix-lite.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, cl-nix-lite, ... }:
  let
    allSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    lib = nixpkgs.lib;

    coalton-packages = # Function to build Coalton for a particular lisp
      { lispPackagesLiteFor, lib, mpfr }:
      lisp':
          with lispPackagesLiteFor lisp'; rec {
            source-error = lispDerivation {
              lispSystem = "source-error";
              src = ./source-error;
              lispDependencies = [
                alexandria
              ];
            };

            coalton-compiler = lispDerivation {
              lispSystem = "coalton-compiler";
              src = lib.cleanSource ./.;
              lispDependencies = [
                alexandria
                concrete-syntax-tree
                eclector
                eclector-concrete-syntax-tree
                float-features
                named-readtables
                source-error
                trivial-gray-streams
              ];
            };

            inherit (lispMultiDerivation {
              src = lib.cleanSource ./.;
              systems = {
                coalton-library = {
                  lispSystem = "coalton/library";
                  lispDependencies = [
                    coalton-compiler
                    trivial-garbage
                    alexandria
                  ];
                };

                coalton = {
                  lispDependencies = [
                    coalton-compiler
                    coalton-library
                  ];
                  lispCheckDependencies = [
                    fiasco
                    coalton-examples
                  ];
                };
               
                coalton-examples = {
                  lispSystems = [
                    "quil-coalton"
                    "small-coalton-programs"
                    "thih-coalton"
                  ];
                  lispDependencies = [
                    coalton
                  ];
                  lispCheckDependencies = [ fiasco ];
                };

                coalton-xmath = {
                  lispSystem = "coalton/xmath";
                  lispDependencies = [
                    coalton
                    coalton-library
                    computable-reals
                  ];
                };

                coalton-doc = {
                  lispSystem = "coalton/doc";
                  lispDependencies = [
                    coalton
                    coalton-xmath
                    html-entities
                    yason
                    spinneret
                  ];
                };
              };

              propagatedBuildInputs =
                systems:
                lib.optionals (builtins.elem "coalton/xmath" systems) [
                  mpfr
                ];
              preBuild =
                let
                  testDirectories = [
                    "$PWD/examples/coalton-testing-example-project"
                    "$PWD/examples/fractal"
                    "$PWD/examples/quil-coalton"
                    "$PWD/examples/small-coalton-programs"
                    "$PWD/examples/thih"
                  ];
                  testPaths = lib.concatStringsSep ":" testDirectories;
                in
                  ''
                    export CL_SOURCE_REGISTRY="${testPaths}:$CL_SOURCE_REGISTRY"
                  '';
            })
              coalton
              coalton-library
              coalton-doc
              coalton-xmath
              coalton-examples
            ;
          };

    overlay = final: prev: rec {
      coaltonFor = (final.callPackage coalton-packages {});
      sbclPackages = prev.sbcl or {} // coaltonFor final.sbcl;
    };

    forAllSystems = f: nixpkgs.lib.genAttrs allSystems (system: f {
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          cl-nix-lite.overlays.default
          overlay
        ];
      };
      packages = self.packages.${system};
    });
      in
    {

      # Re-output packages built with sbcl
      packages = forAllSystems ({ pkgs, ... }:
        (pkgs.coaltonFor pkgs.sbcl)
      );

      overlays.default = lib.composeExtensions cl-nix-lite.overlays.default overlay;

      devShells = forAllSystems ({ pkgs, packages }: {
        default = packages.coalton;
      });
    };
}
