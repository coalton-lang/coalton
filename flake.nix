{
  description = "A flake for coalton";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
    cl-nix-lite.url = "github:hraban/cl-nix-lite/v0";
    cl-nix-lite.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ nixpkgs, flake-parts, cl-nix-lite, ... }:
  flake-parts.lib.mkFlake { inherit inputs; } {
    systems = nixpkgs.lib.platforms.all;

    perSystem = { self', config, pkgs, lib, system, ... }: {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          cl-nix-lite.overlays.default
        ];
      };

      packages = with pkgs.lispPackagesLite; rec {

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

        inherit
            (lispMultiDerivation {
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
                  pkgs.mpfr
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

      devShells.default = self'.packages.coalton;
    };
  };
}
