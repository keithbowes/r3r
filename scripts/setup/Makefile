SHELL = /bin/sh

CHECKINSTALL ?= $(call programpath,checkinstall)
LN ?= $(call programpath,ln)
MAKEPACKAGE ?= $(call programpath,makepackage)

ARCH ?= i386
RPMSOURCEDIR ?= /usr/src/rpm
PKGDIR ?= $(RPMSOURCEDIR)/RPMS/$(ARCH)

include ../common.make
curdir = $(PWD)

all:
	@echo "You shouldn't run this Makefile directly"

dist-autopackage: r3r-$(VERSION).x86.package

dist-deb:
	$(MAKE) checkinstall TYPE=debian
	$(MOVE) $(wildcard $(curdir)/../../*.deb) $(curdir)/../../../r3r-$(VERSION)-$(R3R_UI)-1_$(ARCH).deb

dist-rpm:
	$(MAKE) checkinstall TYPE=rpm VERSION="$(subst -,_,$(VERSION))"
	$(MOVE) $(PKGDIR)/r3r-$(subst -,_,$(VERSION)_$(R3R_UI))-1.$(ARCH).rpm \
	 	$(curdir)/../../..

dist-slackware:
	$(MAKE) checkinstall TYPE=slackware
	$(MOVE) $(wildcard ../../*.tgz) ../../..

dist-inno_setup:
	cd is && $(MAKE) installer

checkinstall:
	$(LN) -fs $(curdir)/description-pak ../../description-pak
	cd ../.. && $(CHECKINSTALL) --default --type=$(TYPE) \
		--install=no --pkgname=r3r --pkgversion="$(VERSION)_$(R3R_UI)" \
		--pkgarch=$(ARCH) --pkgrelease=1 \
		--pkglicense=none --nodoc --deldesc=yes --deldoc=yes \
		--delspec=yes --backup=no --pkgsource=$(curdir)/../.. \
		--maintainer=zooplah@users.sourceforge.net

clean: distclean

distclean:
	$(DEL) *.package autopackage/default.apspec is/r3r.iss is/setup.exe
	$(DEL) r3r.xml* *.meta
	$(DEL) ../../description-pak
	$(DELTREE) ../../doc-pak

r3r-$(VERSION).x86.package: autopackage/default.apspec
	$(MAKEPACKAGE)
	$(MOVE) r3r-$(VERSION).x86.package \
		../../..

autopackage/default.apspec: autopackage/_default.apspec
	$(SED) \
	       	-e 's/@VERSION@/$(VERSION)/g' \
      		autopackage/_default.apspec > \
		autopackage/default.apspec
