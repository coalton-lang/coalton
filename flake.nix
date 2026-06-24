{
  description = "A flake for coalton";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    cl-nix-lite.url = "github:hraban/cl-nix-lite/v0";
    cl-nix-lite.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, cl-nix-lite, ... }:
  let
    allSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    lib = nixpkgs.lib;

    overlay = final: prev: rec {
      coaltonFor = (final.callPackage ./nix/coalton.nix {});
    };

    forAllSystems = f: lib.genAttrs allSystems (system: f {
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

      # Output packages built with sbcl
      packages = forAllSystems ({ pkgs, ... }:
        (pkgs.coaltonFor pkgs.sbcl)
      );

      templates = {
        minimal = {
          path = ./nix/templates/minimal;
          description = "A minimal template using Coalton.";
        };
      };

      overlays = {
        default = lib.composeExtensions cl-nix-lite.overlays.default overlay;
        minimal = overlay;
      };


      devShells = forAllSystems ({ pkgs }: {
        default = pkgs.sbclPackages.coalton;
      });
    };
}
