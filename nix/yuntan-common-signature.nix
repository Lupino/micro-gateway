{ mkDerivation, aeson, base, byteable, bytestring, cryptohash
, fetchgit, hex, scientific, stdenv, text, unordered-containers
, vector
}:
mkDerivation {
  pname = "yuntan-common-signature";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/Lupino/yuntan-common.git";
    sha256 = "0q08dgrgci71k73kirpp0mammn9hwk986vh404nll64xw33mdmw9";
    rev = "cfc461bd81713b3d9373bc12fe6063ae13a9339f";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/yuntan-common-signature; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base byteable bytestring cryptohash hex scientific text
    unordered-containers vector
  ];
  homepage = "https://github.com/Lupino/yuntan-common#readme";
  description = "The common utils for haskell program";
  license = stdenv.lib.licenses.bsd3;
}
