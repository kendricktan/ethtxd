{ compiler ? "ghc865" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  dapptools = import sources.dapptools { };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      hevm = dapptools.haskellPackages.hevm;
      sbv = dapptools.haskellPackages.sbv;
      ethtxd = let ethtxd = hsuper.callCabal2nix "ethtxd" (gitignore ./.) { };
      in pkgs.haskell.lib.overrideCabal ethtxd (super: {
        # enableSharedExecutables = false;
        # enableSharedLibraries = false;
        configureFlags = [
          "--ghc-option=-optl=-pthread"
          # "--ghc-option=-optl=-static"
          # "--ghc-option=-optl=-L${
          #   pkgs.gmp6.override { withStatic = true; }
          # }/lib"
          # "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
          # "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
        ];
      });
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [ p.ethtxd ];
    buildInputs = with pkgs.haskellPackages; [
      cabal-install
      hlint
      ghcide
      stylish-haskell
      hindent
      pkgs.jq
    ];
    withHoogle = true;
    shellHook = ''
      export PATH=${toString ./bin}:$PATH
    '';
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages.ethtxd);
in {
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  ethtxd = myHaskellPackages.ethtxd;
}
