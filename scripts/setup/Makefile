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

dist-rpm:
	$(MAKE) checkinstall CITARGET="$(CITARGET)" TYPE=rpm VERSION="$(subst -,_,$(VERSION))"
	$(MV) $(PKGDIR)/r3r_$(R3R_UI)$(SSL)-$(subst -,_,$(VERSION))-$(PKGRELEASE).$(ARCH).rpm \
	 	$(curdir)/../../..

dist-inno_setup:
	cd is && $(MAKE) installer

checkinstall:
	$(LN) $(curdir)/description-pak ../../description-pak
	cd ../.. && $(CHECKINSTALL) --default --type=$(TYPE) \
		--install=no --pkgname=r3r_$(R3R_UI)$(SSL) \
		--pkgversion="$(VERSION)" \
		--pkgarch=$(ARCH) --pkgrelease=$(PKGRELEASE) \
		--pkggroup="Networking/News" --pkglicense=BSD --nodoc \
		--deldesc=yes --deldoc=yes --delspec=yes --backup=no \
		--pkgsource=$(curdir)/../.. \
		--maintainer=zooplah@gmail.com \
		$(CITARGET)

clean: distclean

distclean:
	$(RM) $(wildcard r3r.desktop)
	$(RM) $(wildcard is/r3r.iss is/setup.exe)
	$(RM) $(wildcard ../../description-pak)
	$(RMRF) $(wildcard ../../doc-pak)

r3r.desktop: r3r.desktop.in
	$(CP) $^ $@
ifeq ($(R3R_UI),tui)
	$(ECHO) "Terminal=true" >> $@
else
	-$(SED) -i -e 's/\nTerminal=true//' $@
endif
	-$(TOUCH) $<
