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
      ethtxd = hsuper.callCabal2nix "ethtxd" (gitignore ./.) { };
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
      pkgs.zlib
      pkgs.glibc
      pkgs.gmp6
      pkgs.libffi
      pkgs.secp256k1
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
