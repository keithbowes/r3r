SHELL = /bin/sh

CHECKINSTALL ?= $(call programpath,checkinstall)

RPMSOURCEDIR ?= ~/rpmbuild
PKGDIR ?= $(RPMSOURCEDIR)/RPMS/$(ARCH)

CITARGET ?= "make install-prog"

include ../common.make
curdir = $(PWD)

all:
	@echo "You shouldn't run this Makefile directly"

install: r3r.desktop
	$(MKDIR) $(appdir)
	$(INSTALL) r3r.desktop $(appdir)
ifndef MAKING_PACKAGE
	-update-desktop-database
endif

dist-deb:
	$(MAKE) checkinstall CITARGET="$(CITARGET)" TYPE=debian VERSION=$(VERSION)
	$(MV) $(srcdir)/../../r3r_0-$(PKGRELEASE)_$(ARCH).deb \
		$(srcdir)/../../../r3r-$(VERSION)-$(R3R_UI)-$(PKGRELEASE)_$(ARCH).deb

dist-rpm:
	$(MAKE) checkinstall CITARGET="$(CITARGET)" TYPE=rpm VERSION="$(subst -,_,$(VERSION))"
	$(MV) $(PKGDIR)/r3r-$(subst -,_,$(VERSION)_$(R3R_UI))-$(PKGRELEASE).$(ARCH).rpm \
	 	$(curdir)/../../..

dist-inno_setup:
	cd is && $(MAKE) installer

checkinstall:
	$(LN) $(curdir)/description-pak ../../description-pak
	cd ../.. && $(CHECKINSTALL) --default --type=$(TYPE) \
		--install=no --pkgname=r3r --pkgversion="$(VERSION)_$(R3R_UI)" \
		--pkgarch=$(ARCH) --pkgrelease=$(PKGRELEASE) \
		--pkggroup="Networking/News" --pkglicense=BSD --nodoc \
		--deldesc=yes --deldoc=yes --delspec=yes --backup=no \
		--pkgsource=$(curdir)/../.. \
		--maintainer=zooplah@users.sourceforge.net \
		$(CITARGET)

clean: distclean

distclean:
	$(RM) $(wildcard r3r.desktop)
	$(RM) $(wildcard is/r3r.iss is/setup.exe)
	$(RM) $(wildcard ../../description-pak)
	$(RMRF) $(wildcard ../../doc-pak)

r3r.desktop: r3r.desktop.in
	-$(SED) -e 's|@bindir@|$(bindir)|g' -e 's|@icondir@|$(icondir)|g' $< > $@
ifeq ($(R3R_UI),tui)
	$(ECHO) "Terminal=true" >> $@
else
	-$(SED) -i -e 's/\nTerminal=true//' $@
endif
	-$(TOUCH) $<
