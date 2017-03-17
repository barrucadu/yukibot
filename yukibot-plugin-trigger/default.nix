{ mkDerivation, base, mongoDB, random, regex-tdfa, stdenv, text
, unordered-containers, yukibot-core
}:
mkDerivation {
  pname = "yukibot-plugin-trigger";
  version = "2.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base mongoDB random regex-tdfa text unordered-containers
    yukibot-core
  ];
  homepage = "https://github.com/barrucadu/yukibot";
  description = "Match messages (by regex or literally) and respond";
  license = stdenv.lib.licenses.mit;
}
