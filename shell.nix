{ pkgs ? (import <nixpkgs> {}) }:
let
  inherit (pkgs) haskellPackages stdenv;

  callPackage = stdenv.lib.callPackageWith (pkgs // haskellPackages // yukibotPackages);

  # Make extra packages available to ghci and mueval
  extraHaskellLibs = p: [ p.leancheck p.lens p.smallcheck p.random ] ++
    # mueval itself needs these packages
    [ p.QuickCheck p.show p.simple-reflect ];
  ghc'    = haskellPackages.ghcWithPackages extraHaskellLibs;
  hint'   = haskellPackages.hint.override { ghc = ghc'; };
  mueval' = haskellPackages.mueval.override { hint = hint'; };

  yukibotPackages = {
    yukibot                 = callPackage ./yukibot                 {};
    yukibot-backend-irc     = callPackage ./yukibot-backend-irc     {};
    yukibot-core            = callPackage ./yukibot-core            {};
    yukibot-plugin-channel  = callPackage ./yukibot-plugin-channel  {};
    yukibot-plugin-hello    = callPackage ./yukibot-plugin-hello    {};
    yukibot-plugin-linkinfo = callPackage ./yukibot-plugin-linkinfo {};
    yukibot-plugin-mueval   = callPackage ./yukibot-plugin-mueval   {};
    yukibot-plugin-seen     = callPackage ./yukibot-plugin-seen     {};
    yukibot-plugin-trigger  = callPackage ./yukibot-plugin-trigger  {};
  };
in {
  yukibotEnv = stdenv.mkDerivation {
    name = "yukibot-env";
    buildInputs = [
      ghc'
      mueval'
      yukibotPackages.yukibot
    ];
  };
}
