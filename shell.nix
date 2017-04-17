{ pkgs ? (import <nixpkgs> {}) }:
let
  inherit (pkgs) haskellPackages stdenv;

  # Override the standard callPackage function with one that knows
  # about the yukibot packages.
  callPackage = stdenv.lib.callPackageWith (pkgs // haskellPackages // yukibotPackages);

  # The yukibot package set.  If this file doesn't exist, use
  # `gen-package-list.sh` to build it.
  yukibotPackages =
    import ./yukibot-packages.nix {inherit callPackage stdenv;};

  # Make extra packages available to ghci and mueval
  extraHaskellLibs = p: [ p.leancheck p.lens p.smallcheck p.random ] ++
    # mueval itself needs these packages
    [ p.QuickCheck p.show p.simple-reflect ];
  ghc'    = haskellPackages.ghcWithPackages extraHaskellLibs;
  hint'   = haskellPackages.hint.override { ghc = ghc'; };
  mueval' = haskellPackages.mueval.override { hint = hint'; };
in {
  yukibotEnv = stdenv.mkDerivation {
    name = "yukibot-env";
    buildInputs = [
      ghc'
      mueval'
      yukibotPackages.yukibot
    ];
    shellHook = "eval $(grep export ${ghc'}/bin/ghc)";
  };
}
