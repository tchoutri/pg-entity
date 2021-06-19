let pkgs = import (fetchTarball path) {};
    path = https://github.com/NixOS/nixpkgs/archive/master.tar.gz;
in with pkgs;
  mkShell {
    buildInputs = [
      # Haskell Deps
      haskell.compiler.ghc8104
      cabal-install
      ghcid
      hlint
      haskellPackages.apply-refact
      stylish-haskell

      # DB Deps
      postgresql_13
      gmp
      zlib
      glibcLocales

      # Extra
      direnv
      parallel
      # mkdocs
      gnumake
    ];
  }
