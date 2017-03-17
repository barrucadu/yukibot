{ mkDerivation, aeson, base, contravariant, http-conduit, hxt
, hxt-tagsoup, lens, lens-aeson, network-uri, stdenv, text, time
, unordered-containers, yukibot-core
}:
mkDerivation {
  pname = "yukibot-plugin-linkinfo";
  version = "2.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base contravariant http-conduit hxt hxt-tagsoup lens
    lens-aeson network-uri text time unordered-containers yukibot-core
  ];
  homepage = "https://github.com/barrucadu/yukibot";
  description = "Linkinfo plugin for yukibot-core";
  license = stdenv.lib.licenses.mit;
}
