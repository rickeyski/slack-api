{ cabal, aeson, errors, HsOpenSSL, ioStreams, lens, lensAeson
, monadLoops, mtl, network, opensslStreams, text, time
, transformers, websockets, wreq
}:

cabal.mkDerivation (self:
let
  lib         = self.stdenv.lib;
  isWithin    = p: dirPath: lib.hasPrefix (toString dirPath) (toString p);
  cabalFilter = path: type: (let pathBaseName = baseNameOf path; in
                               !(lib.hasSuffix "~" pathBaseName) &&
                               !(lib.hasSuffix "#" pathBaseName) &&
                               !(lib.hasPrefix "." pathBaseName) &&
                               (
                                   pathBaseName == "slack-api.cabal" ||
                                   pathBaseName == "LICENSE"              ||
                                   pathBaseName == "Setup.hs"             ||
                                   isWithin path ./src                    ||
                                   false
                               )
                            );
in {
  pname = "slack-api";

  version = "0.1.3";
  src = builtins.filterSource cabalFilter ./.;
  #
  # sha256 = "12f9w8s5ir3skdr2dhlvr94f3sfbqjky5ppc943wj60sz0s7lha1";

  buildDepends = [
    aeson errors HsOpenSSL ioStreams lens lensAeson monadLoops mtl
    network opensslStreams text time transformers websockets wreq
  ];
  meta = {
    description = "Bindings for the Slack RTM API";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
