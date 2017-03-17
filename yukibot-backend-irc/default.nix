{ mkDerivation, base, bytestring, filepath, irc-client, stdenv, stm
, text, transformers, yukibot-core
}:
mkDerivation {
  pname = "yukibot-backend-irc";
  version = "2.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring filepath irc-client stm text transformers
    yukibot-core
  ];
  homepage = "https://github.com/barrucadu/yukibot";
  description = "An IRC backend for yukibot-core";
  license = stdenv.lib.licenses.mit;
}
