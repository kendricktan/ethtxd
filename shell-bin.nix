let
  sources = import ./nix/sources.nix;

  pkgs = import sources.nixpkgs { };

  libff = pkgs.callPackage (import ./nix/libff.nix) { };
  drv = pkgs.haskellPackages.shellFor {
    packages = p: [ ];
    buildInputs = [
      pkgs.zlib
      pkgs.glibc
      pkgs.gmp
      pkgs.gmp6
      pkgs.libffcall
      pkgs.libffi
      pkgs.secp256k1
      libff
    ];
  };
in if pkgs.lib.inNixShell then
  drv.overrideAttrs
  (_: { LD_LIBRARY_PATH = "${pkgs.secp256k1}/lib:${libff}/lib"; })
else
  drv
