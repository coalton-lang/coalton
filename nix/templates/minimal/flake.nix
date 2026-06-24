{
  description = "Describe your project here!";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    coalton = {
      url = "path:/Users/user/common-lisp/coalton";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, coalton, ... }:
  let
    allSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    lib = nixpkgs.lib;
    forAllSystems = f: lib.genAttrs allSystems
      (system: f {
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ coalton.overlays.default ];
        };
      });

  in {
    devShells = forAllSystems ({pkgs}:  {
      default = pkgs.mkShell {
        packages = with pkgs; [
          sbcl
          (coaltonFor sbcl).coalton
        ];
      };
    });
  };
}
