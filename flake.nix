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

    packages = # Function to build Coalton for a particular lisp
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

            coalton-asdf = lispDerivation {
              lispSystem = "coalton-asdf";
              # Restricting the fileset prevents needless recompilation.
              src = lib.fileset.toSource { 
                root = ./.;
                fileset = lib.fileset.unions [
                  ./coalton-asdf.asd
                  ./coalton-asdf.lisp
                  ./VERSION.txt
                ];
              };

              lispDependencies = [ coalton-compiler ];
            };

            inherit (lispMultiDerivation {
              src = lib.cleanSource ./.;
              systems = {
                coalton = {
                  lispSystems = [ "coalton/library" "coalton" "coalton/xmath" "coalton/doc" ];
                  lispDependencies = [
                    coalton-compiler
                    coalton-asdf
                    
                    trivial-garbage
                    alexandria

                    # coalton/xmath
                    computable-reals

                    # coalton/docs
                    html-entities
                    yason
                    spinneret
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
              coalton-examples
            ;
          };

    overlay = final: prev: rec {
      coaltonFor = (final.callPackage packages {});

      # Re-wrap sbcl to update withPackages
      sbcl = final.wrapLisp {
        pkg = prev.sbcl; 
        faslExt = "fasl";
        flags = [
          "--dynamic-space-size"
          "4096"
        ];
        packageOverrides = final': prev': {
          inherit ((coaltonFor prev.sbcl)) coalton coalton-asdf coalton-compiler source-error coalton-examples;
        };
      };
    };

    forAllSystems = f: nixpkgs.lib.genAttrs allSystems (system: f {
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          cl-nix-lite.overlays.default
          overlay
        ];
      };
    });
      in
    {

      # Re-output packages built with sbcl
      packages = forAllSystems ({ pkgs, ... }:
        (pkgs.coaltonFor pkgs.sbcl)
      );

      overlays.default = lib.composeExtensions cl-nix-lite.overlays.default overlay;

      devShells = forAllSystems ({ pkgs }: {
        default = pkgs.sbclPackages.coalton;
      });
    };
}
