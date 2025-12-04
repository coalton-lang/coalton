{
  description = "A flake for coalton";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ flake-parts.flakeModules.easyOverlay ];
      systems = nixpkgs.lib.platforms.all;
      perSystem = { config, pkgs, system, ... }:
        let
          ############ Settings ############
          ## Project name
          pname = "coalton";
          ## Source directory
          src = ./.;
          ## Dependencies
          lispLibs = lisp: with lisp.pkgs; [
            alexandria
            computable-reals
            concrete-syntax-tree
            eclector
            eclector-concrete-syntax-tree
            fiasco
            float-features
            fset
            named-readtables
            trivial-garbage
            trivial-gray-streams
          ];
          ## Non-Lisp dependencies
          nativeLibs = with pkgs; [ mpfr ];
          ## Supported Lisp implementations
          lispImpls = [
            "sbcl"
            "ccl"
            "ecl"
            "abcl"
          ];
          ##################################
          systems = [
            "coalton" # Main ASDF system for Coalton
            "coalton-compiler" # Compiler ASDF system for Coalton
            "coalton-asdf" # ASDF extension for .coal file
            "coalton/testing" # A Helper ASDF system to test software
          ];
          version = let
            txt = builtins.readFile "${src}/VERSION.txt";
            ver = builtins.replaceStrings [''"''] [""] txt;
          in ver;
          isAvailable = impl: let
            basePkgs = import nixpkgs { inherit system; overlays = []; };
            lisp = basePkgs.${impl};
          in (builtins.tryEval lisp).success
             && (builtins.elem system lisp.meta.platforms)
             && (!lisp.meta.broken);
          availableLispImpls = builtins.filter isAvailable lispImpls;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath nativeLibs;
          bundledPackage = { lisp }: rec {
            sourceErrorLib = lisp.buildASDFSystem {
              inherit version;
              pname = "source-error";
              src = "${src}/source-error";
              systems = [ "source-error" ];
              lispLibs = with lisp.pkgs; [ alexandria ];
            };
            mainLib = lisp.buildASDFSystem {
              inherit pname version src systems nativeLibs;
              lispLibs = (lispLibs lisp) ++ [ sourceErrorLib ];
            };
            lisp' = lisp.withPackages (ps: [ mainLib ]) // {
              inherit (lisp) meta;
            };
          };
          recipe = {
            sbcl = bundledPackage {
              lisp = pkgs.sbcl;
            };
            ccl = bundledPackage {
              lisp = pkgs.ccl;
            };
            ecl = bundledPackage {
              lisp = pkgs.ecl;
            };
            abcl = bundledPackage {
              lisp = pkgs.abcl;
            };
          };
          packages = impl: [
            {
              name = "${impl}-${pname}";
              value = recipe.${impl}.mainLib;
            }
          ];
          devPackages = impl:
            pkgs.${impl}.withPackages (ps: lispLibs pkgs.${impl});
          overlays = impl: [
            {
              name = impl;
              value = pkgs.${impl}.withOverrides
                (self: super: { ${pname} = config.packages."${impl}-${pname}"; });
            }
          ];
        in {
          overlayAttrs =
            builtins.listToAttrs (builtins.concatMap overlays availableLispImpls);
          devShells.default = pkgs.mkShell {
            inherit LD_LIBRARY_PATH;
            shellHook = ''
              export CL_SOURCE_REGISTRY=$PWD:$PWD/source-error
            '';
            packages = builtins.map devPackages availableLispImpls;
          };
          packages = builtins.listToAttrs
            (builtins.concatMap packages availableLispImpls);
        };
    };
}
