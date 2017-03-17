{ mkDerivation, base, stdenv, text, unordered-containers
, yukibot-core
}:
mkDerivation {
  pname = "yukibot-plugin-hello";
  version = "2.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base text unordered-containers yukibot-core
  ];
  homepage = "https://github.com/barrucadu/yukibot";
  description = "A \"hello world\" plugin for yukibot-core";
  license = stdenv.lib.licenses.mit;
}
