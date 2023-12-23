{
  inputs.nixpkgs.url = "https://github.com/NixOS/nixpkgs/archive/2c2a09678ce2ce4125591ac4fe2f7dfaec7a609c.tar.gz";
  outputs = { self, nixpkgs }:
    let system = "x86_64-linux";
        pkgs = import nixpkgs { inherit system;};
        lib = pkgs.lib;
    in
      {
        devShells.${system}.default = pkgs.mkShell rec {
          buildInputs = with pkgs; [
            # Haskell Deps
            haskell.compiler.ghc8107
            cabal-install
            ghcid
            hlint
            cacert
            haskellPackages.apply-refact
            #fourmolu_0_6_0_0
            git
            haskellPackages.cabal-fmt

            # DB Deps
            postgresql_14
            gmp
            zlib
            glibcLocales
            haskellPackages.postgresql-simple-migration

            # Extra
            direnv
            yarn
            nodejs
          ];
          shellHook = ''
            PS1="<pg-entity> $PS1"
          '';
        };

      };
}
