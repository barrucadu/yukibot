{ mkDerivation, base, stdenv, yukibot-backend-irc, yukibot-core
, yukibot-plugin-channel, yukibot-plugin-hello
, yukibot-plugin-linkinfo, yukibot-plugin-mueval
, yukibot-plugin-seen, yukibot-plugin-trigger
}:
mkDerivation {
  pname = "yukibot";
  version = "2.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base yukibot-backend-irc yukibot-core yukibot-plugin-channel
    yukibot-plugin-hello yukibot-plugin-linkinfo yukibot-plugin-mueval
    yukibot-plugin-seen yukibot-plugin-trigger
  ];
  homepage = "https://github.com/barrucadu/yukibot";
  description = "An IRC bot for the ##compsoc-uk-anime channel on Freenode";
  license = stdenv.lib.licenses.mit;
}
