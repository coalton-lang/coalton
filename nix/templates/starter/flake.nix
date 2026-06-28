{
  description = "my-app";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    coalton.url = "path:/Users/user/common-lisp/coalton";
    cl-nix-lite.url = "github:hraban/cl-nix-lite/v0";
  };

  outputs = { nixpkgs, cl-nix-lite, coalton, ... }:
  let
    allSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    lib = nixpkgs.lib;
    forAllSystems = f: lib.genAttrs allSystems
      (system: f {
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ cl-nix-lite.overlays.default ];
        };
      });

  in {
    devShells = forAllSystems ({ pkgs }: {
      default = with pkgs.lispPackagesLite;
      lispDerivation {
        src = pkgs.lib.cleanSource ./.;
        lispSystems = [ "my-app" ];
        lispDependencies = [ (coalton.mkCoalton pkgs pkgs.sbcl).coalton ];
        lispCheckDependencies = [ fiasco ];
      };
    });
  };
}
