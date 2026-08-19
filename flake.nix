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

    mkCoalton = (pkgs: (pkgs.callPackage ./nix/coalton.nix { }));

    forAllSystems = f: nixpkgs.lib.genAttrs allSystems (system: f {
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ cl-nix-lite.overlays.default ];
      };
    });
  in
  {

    inherit mkCoalton;
    # Output packages built with sbcl
    packages = forAllSystems ({ pkgs, ... }:
    (mkCoalton pkgs pkgs.sbcl)
    );

    templates = {
      starter = {
        path = ./nix/templates/starter;
        description = "A Coalton starter project template.";
        welcomeText = builtins.readFile ./nix/templates/starter/README.md;
      };
    };

    devShells = forAllSystems ({ pkgs }: {
      default = pkgs.sbclPackages.coalton;
    });
  };
}
