{ lispPackagesLiteFor, lib, mpfr }:

# Function to build Coalton for a particular lisp
lisp':
with lispPackagesLiteFor lisp'; rec {
  source-error = lispDerivation {
    lispSystem = "source-error";
    src = ../source-error;
    lispDependencies = [
      alexandria
    ];
  };

  coalton-compiler = lispDerivation {
    lispSystem = "coalton-compiler";
    src = lib.cleanSource ../.;
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
      root = ../.;
      fileset = lib.fileset.unions [
        ../coalton-asdf.asd
        ../coalton-asdf.lisp
        ../VERSION.txt
      ];
    };

    lispDependencies = [ coalton-compiler ];
  };

  inherit (lispMultiDerivation {
    src = lib.cleanSource ../.;
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
}
