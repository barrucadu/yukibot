{ mkDerivation, aeson, base, bson, bytestring, exceptions, filepath
, free, hashable, htoml, http-client, http-conduit, lens, mongoDB
, network-uri, parsec, random, stdenv, stm, text, time, unix
, unordered-containers
}:
mkDerivation {
  pname = "yukibot-core";
  version = "2.0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bson bytestring exceptions filepath free hashable htoml
    http-client http-conduit lens mongoDB network-uri parsec random stm
    text time unix unordered-containers
  ];
  homepage = "https://github.com/barrucadu/yukibot";
  description = "A library for network-service bots";
  license = stdenv.lib.licenses.mit;
}
