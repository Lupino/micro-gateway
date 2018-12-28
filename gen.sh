#!/usr/bin/env bash

gen_nix_with_subpath() {
    cabal2nix $1 --revision $2 --subpath $3 > nix/$4.nix
}

YUNTAN_COMMON_REPO=https://github.com/Lupino/yuntan-common.git
YUNTAN_COMMON_REV=cfc461bd81713b3d9373bc12fe6063ae13a9339f

gen_nix_with_subpath $YUNTAN_COMMON_REPO $YUNTAN_COMMON_REV yuntan-common-signature yuntan-common-signature
