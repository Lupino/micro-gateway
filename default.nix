{ static ? false, compiler ? "default" }:

let
  config = import ./nix/config.nix {static=static;};
  pkgs = config.pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  yuntan-common-json            = haskellPackages.callPackage ./nix/yuntan-common-json.nix {};
  yuntan-common-list-result     = haskellPackages.callPackage ./nix/yuntan-common-list-result.nix {
    inherit yuntan-common-json;
  };
  yuntan-common-result          = haskellPackages.callPackage ./nix/yuntan-common-result.nix {
    inherit yuntan-common-json;
  };
  yuntan-common-scotty          = haskellPackages.callPackage ./nix/yuntan-common-scotty.nix {
    inherit yuntan-common-list-result;
    inherit yuntan-common-result;
  };
  yuntan-common-signature       = haskellPackages.callPackage ./nix/yuntan-common-signature.nix {};
  yuntan-gateway = haskellPackages.callPackage ./nix/yuntan-gateway.nix {
    inherit yuntan-common-scotty;
    inherit yuntan-common-signature;
  };

in {
  inherit yuntan-gateway;
}
