{ mkDerivation, aeson, base, bytestring, case-insensitive
, containers, data-default-class, hex, hslogger, http-client
, http-types, lens, optparse-applicative, scotty, stdenv
, streaming-commons, text, time, unix-time, unordered-containers
, wai, wai-cors, warp, wreq, yaml, yuntan-common-scotty
, yuntan-common-signature, static ? false
}:
let config = import ./config.nix {static = static;};
in mkDerivation {
  pname = "yuntan-gateway";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  enableSharedExecutables = false;
  enableSharedLibraries = false;
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive containers hex hslogger
    http-client http-types lens scotty text time unix-time
    unordered-containers wai wreq yuntan-common-scotty
    yuntan-common-signature
  ];
  executableHaskellDepends = [
    aeson base bytestring data-default-class http-client lens
    optparse-applicative scotty streaming-commons wai-cors warp wreq
    yaml
  ];
  homepage = "https://github.com/Lupino/yuntan-gateway#readme";
  license = stdenv.lib.licenses.bsd3;
  configureFlags = config.configureFlags;
}
