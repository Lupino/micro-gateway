{ static ? false, compiler ? "default" }:

let
  config = import ./nix/config.nix {static=static;};
  pkgs = config.pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  yuntan-common-signature       = haskellPackages.callPackage ./nix/yuntan-common-signature.nix {};
  yuntan-gateway = haskellPackages.callPackage ./nix/yuntan-gateway.nix {
    inherit yuntan-common-signature;
    inherit static;
  };

in {
  inherit yuntan-gateway;
}
