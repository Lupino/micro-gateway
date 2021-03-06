PLATFORM ?= musl64
STRIP = strip
PKG ?= micro-gateway
COMPILER = ghc8105

ifeq ($(PLATFORM),aarch64-multiplatform-musl)
STRIP = aarch64-unknown-linux-musl-strip
else
ifeq ($(PLATFORM),muslpi)
STRIP = armv6l-unknown-linux-musleabihf-strip
COMPILER = ghc884
else

endif

endif

all: package

dist/$(PLATFORM):
	mkdir -p $@

dist/$(PLATFORM)/%: dist/$(PLATFORM)
	nix-build -A projectCross.$(PLATFORM).hsPkgs.$(PKG).components.exes.$(shell basename $@) --argstr compiler-nix-name $(COMPILER)
	cp -f result/bin/$(shell basename $@) $@
	chmod +w $@
	nix-shell --run "$(STRIP) -s $@" --argstr compiler-nix-name $(COMPILER) --arg crossPlatforms "ps: with ps; [$(PLATFORM)]"
	chmod -w $@

simple-gateway: dist/$(PLATFORM)/simple-gateway


package: simple-gateway
	cd dist/$(PLATFORM) && tar cjvf ../micro-gateway-linux-$(PLATFORM).tar.bz2 *

clean:
	rm -rf dist

help:
	@echo make PLATFORM=muslpi
	@echo make PLATFORM=musl64
	@echo make PLATFORM=aarch64-multiplatform-musl
