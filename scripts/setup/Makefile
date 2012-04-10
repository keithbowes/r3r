SHELL = /bin/sh

CHECKINSTALL ?= $(call programpath,checkinstall)
LN ?= $(call programpath,ln)

ARCH ?= $(CPU_TARGET)
RPMSOURCEDIR ?= ~/rpmbuild
PKGDIR ?= $(RPMSOURCEDIR)/RPMS/$(ARCH)

include ../common.make
curdir = $(PWD)

all:
	@echo "You shouldn't run this Makefile directly"

install: r3r.desktop
	$(MKDIR) $(appdir)
	$(INSTALL) r3r.desktop $(appdir)
	-update-desktop-database

dist-deb: LANGUAGE=

dist-deb:
	$(MAKE) checkinstall TYPE=debian VERSION=$(VERSION)
	$(MOVE) $(wildcard $(srcdir)/../../*.deb) $(srcdir)/../../../r3r-$(VERSION)-$(R3R_UI)-1_$(ARCH).deb

dist-rpm:
	$(MAKE) checkinstall TYPE=rpm VERSION="$(subst -,_,$(VERSION))"
	$(MOVE) $(PKGDIR)/r3r-$(subst -,_,$(VERSION)_$(R3R_UI))-1.$(ARCH).rpm \
	 	$(curdir)/../../..

dist-inno_setup:
	cd is && $(MAKE) installer

checkinstall:
	$(LN) -fs $(curdir)/description-pak ../../description-pak
	cd ../.. && $(CHECKINSTALL) --default --type=$(TYPE) \
		--install=no --pkgname=r3r --pkgversion="$(VERSION)_$(R3R_UI)" \
		--pkgarch=$(ARCH) --pkgrelease=1 --pkggroup="Networking/News" \
		--pkglicense=none --nodoc --deldesc=yes --deldoc=yes \
		--delspec=yes --backup=no --pkgsource=$(curdir)/../.. \
		--maintainer=zooplah@users.sourceforge.net

clean: distclean

distclean:
	$(DEL) r3r.desktop
	$(DEL) is/r3r.iss is/setup.exe
	$(DEL) ../../description-pak
	$(DELTREE) ../../doc-pak

r3r.desktop: r3r.desktop.in
	-$(SED) -e 's|@bindir@|$(bindir)|g' -e 's|@icondir@|$(icondir)|g' $< > $@
