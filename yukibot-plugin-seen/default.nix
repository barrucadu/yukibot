{ mkDerivation, base, mongoDB, stdenv, text, time
, unordered-containers, yukibot-core
}:
mkDerivation {
  pname = "yukibot-plugin-seen";
  version = "2.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base mongoDB text time unordered-containers yukibot-core
  ];
  homepage = "https://github.com/barrucadu/yukibot";
  description = "Keep track of active users";
  license = stdenv.lib.licenses.mit;
}
