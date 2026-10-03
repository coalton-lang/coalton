{ lispPackagesLiteFor, lib, mpfr, nix-gitignore }:

# Function to build Coalton for a particular lisp
lisp':
with lispPackagesLiteFor lisp'; let

  filterPaths = (paths: src:
  lib.cleanSourceWith {
    filter = (path: type:
    !builtins.elem (builtins.baseNameOf path) paths);
    inherit src;
  });

  coaltonSource = (filterPaths 
    [
      "nix"
      "scripts"
      "docs"
      "examples"
      "mine"
      "tests"
      ".github"
      "source-error"
    ]
    (nix-gitignore.gitignoreSource [] ../.));

  
in rec {
  source-error = lispDerivation {
    lispSystem = "source-error";
    src = ../source-error;
    lispDependencies = [
      alexandria
    ];
  };

  coalton-compiler = lispDerivation {
    lispSystem = "coalton-compiler";
    # XXX A limitation in cl-nix-lite means every derivation must have a different src.
    src = filterPaths [ "doc" "coalton.asd" ] coaltonSource;
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

  coalton = lispDerivation {
    src = coaltonSource;
    lispSystems = [ "coalton/library" "coalton" "coalton/xmath" "coalton/testing" "coalton/doc" ];

    lispDependencies = [
      coalton-compiler
      coalton-asdf
                    
      trivial-garbage
      alexandria

      # coalton/xmath
      computable-reals

      # coalton/doc
      html-entities
      yason
      spinneret

      # coalton/testing
      fiasco

    ];

    propagatedBuildInputs = [
      # coalton/xmath
      mpfr
    ];
  };
}
