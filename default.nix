{ mkDerivation, aeson, base, bytestring, containers, errors
, hashable, io-streams, lens, lens-aeson, monad-loops, mtl, network
, network-uri, stdenv, text, time, time-locale-compat, tls
, transformers, websockets, wreq, wuss
}:
mkDerivation {
  pname = "slack-api";
  version = "0.12";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers errors hashable io-streams lens
    lens-aeson monad-loops mtl network network-uri text time
    time-locale-compat tls transformers websockets wreq wuss
  ];
  executableHaskellDepends = [ base lens mtl text ];
  testHaskellDepends = [ base ];
  description = "Bindings to the Slack RTM API";
  license = stdenv.lib.licenses.mit;
}
