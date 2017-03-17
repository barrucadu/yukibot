{ mkDerivation, base, stdenv, text, unordered-containers
, yukibot-core
}:
mkDerivation {
  pname = "yukibot-plugin-channel";
  version = "2.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base text unordered-containers yukibot-core
  ];
  homepage = "https://github.com/barrucadu/yukibot";
  description = "A channel management plugin for yukibot-core";
  license = stdenv.lib.licenses.mit;
}
