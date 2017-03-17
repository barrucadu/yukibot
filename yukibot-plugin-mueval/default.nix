{ mkDerivation, base, process, random, stdenv, text
, unordered-containers, yukibot-core
}:
mkDerivation {
  pname = "yukibot-plugin-mueval";
  version = "2.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base process random text unordered-containers yukibot-core
  ];
  homepage = "https://github.com/barrucadu/yukibot";
  description = "A mueval plugin for yukibot-core";
  license = stdenv.lib.licenses.mit;
}
