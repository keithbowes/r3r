USE_PAX ?= 1

ifeq ($(USE_PAX),1)
	PAX = pax
	PAXFLAGS = -w -x ustar
else
	PAX = tar
	PAXFLAGS = -cf -
endif

.PHONY: check

include scripts/common.make

all: docs
	cd $(srcdir)/icons && $(MAKE)
	cd $(srcdir)/src && $(MAKE)
	$(MOVE) $(srcdir)/src/ui/$(R3R_UI)/r3r$(TARGETEXEEXT) $(builddir)/r3r-$(R3R_UI)$(TARGETEXEEXT)

check:
ifdef USE_FPC
	@$(ECHO) $(call checkprog,fpc)
else
ifdef USE_GPC
	@$(ECHO) $(call checkprog,gpc)
	@$(ECHO) $(call checkprog,gp)
endif
endif
	@$(ECHO) $(call checkunit,SysUtils)
ifeq ($(DEFS_SOCKETS),SOCKETS_SYNAPSE)
	@$(ECHO) $(call checkunit,BlckSock)
	@$(ECHO) $(call checkunit,SynaUtil)
else
ifeq ($(DEFS_SOCKETS),SOCKETS_BSD)
	@$(ECHO) $(call checkprog,$(notdir $(CC)))
ifdef inWindows
	@$(ECHO) $(call checklib,wsock32)
else
ifdef inUnix
	@$(ECHO) $(call checklib,c)
endif
endif
else
	$(error Unsupported sockets library)
endif
endif
ifeq ($(R3R_UI),tui)
ifndef USE_NCRT
	@$(ECHO) $(call checkunit,CRT)
else
	@$(ECHO) $(call checkunit,nCRT)
	@$(ECHO) $(call checkunit,nCurses)
endif
else
ifeq ($(R3R_UI),wx)
	@$(ECHO) $(call checkprog,$(notdir $(CXX)))
	@$(ECHO) $(call checkprog,wx-config)
else
ifeq ($(R3R_UI),classic)
	$(warning This UI is NOT maintained)
	@$(ECHO) $(call checkunit,LResources)
else
ifeq ($(R3R_UI),tv)
	$(warning This UI is NOT maintained)
	@$(ECHO) $(call checkunit,FvConsts)
else
ifneq ($(R3R_UI),html)
	$(error Unknown UI)
endif
endif
endif
endif
endif
ifeq ($(DEFS_SETTINGS),SETTINGS_INI)
	@$(ECHO) $(call checkunit,IniFiles)
else
ifeq ($(DEFS_SETTINGS),SETTINGS_REG)
	@$(ECHO) $(call checkunit,Registry)
endif
endif
	@$(ECHO) $(call checklib,expat)
	@$(ECHO) $(call checklib,idn)
	@$(ECHO) $(call checklib,intl)
ifdef inWindows
	@$(ECHO) $(call checkprog,png2ico)
endif
	-@$(DEL) $(wildcard *check*)
	@$(ECHO) $(call success)

check_clean: _clean
	$(RM) $(wildcard check.pas check$(TARGETEXEEXT) check$(OEXT) check$(PPUEXT))

install:
	$(MKDIR) $(bindir)
	cd $(srcdir)/doc && $(MAKE) install
	cd $(srcdir)/icons && $(MAKE) install
	cd $(srcdir)/src && $(MAKE) install
	$(INSTALLEXE) $(builddir)/r3r-$(R3R_UI)$(TARGETEXEEXT) $(bindir)
ifdef inUnix
	$(INSTALLEXE) $(srcdir)/r3r $(bindir)
	$(INSTALLEXE) $(srcdir)/r3r-settitle $(bindir)
endif

# Documentation rules
docs:
	cd $(srcdir)/doc && $(MAKE)

dist-docs: docs
	$(PAX) $(PAXFLAGS) doc/api | gzip -cf > ../r3r-$(VERSION)-api.tar.gz


# Uninstallation rules
uninstall:
	cd $(srcdir)/doc && $(MAKE) uninstall
	cd $(srcdir)/icons && $(MAKE) uninstall
	cd $(srcdir)/src && $(MAKE) uninstall
	$(DEL) $(bindir)/r3r
	$(foreach ui,$(uis),$(DEL) $(bindir)/r3r-$(ui)$(TARGETEXEEXT); )
	$(DEL) $(bindir)/r3r-settitle
	-$(RMDIR) $(bindir)
	$(DELTREE) $(rdatadir)
	-$(RMDIR) $(prefix)

# Distribution rules
dist-build:
	$(MAKE) R3R_UI=tui
	$(MAKE) R3R_UI=wx

dist: dist-build
	$(MKDIR) ../r3r-$(VERSION)-$(PLATFORM)
	$(MKDIR) ../r3r-$(VERSION)-$(PLATFORM)/bin
ifdef inUnix
	$(INSTALLEXE) r3r ../r3r-$(VERSION)-$(PLATFORM)/bin
endif
	$(INSTALLEXE) $(builddir)/r3r-tui$(EXEEXT) ../r3r-$(VERSION)-$(PLATFORM)/bin
	$(INSTALLEXE) $(builddir)/r3r-wx$(EXEEXT) ../r3r-$(VERSION)-$(PLATFORM)/bin
	$(INSTALLEXE) $(srcdir)r3r-settitle ../r3r-$(VERSION)-$(PLATFORM)/bin
	$(INSTALLEXE) $(srcdir)src/utils/opml/r3r_opml$(EXEEXT) ../r3r-$(VERSION)-$(PLATFORM)/bin
	$(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/lib
	$(INSTALLEXE) $(srcdir)/src/libr3r/$(SHAREDLIBPREFIX)libr3r_shared$(SHAREDLIBEXT) ../r3r-$(VERSION)-$(PLATFORM)/lib
	$(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/icons
	$(COPY) $(srcdir)/icons/r3r.png  ../r3r-$(VERSION)-$(PLATFORM)/share/icons
	$(foreach l, $(subst .mo,,$(notdir $(wildcard src/libr3r/po/*.mo))), $(MKDIR) ../r3r-$(VERSION)-$(PLATFORM)/share/locale/LC_MESSAGES/$l; $(COPY) src/libr3r/po/$l.mo ../r3r-$(VERSION)-$(PLATFORM)/share/locale/LC_MESSAGES/$(l)/libr3r.mo;)
	$(foreach l, $(subst .mo,,$(notdir $(wildcard src/ui/$(R3R_UI)/po/*.mo))), $(MKDIR) ../r3r-$(VERSION)-$(PLATFORM)/share/locale/LC_MESSAGES/$l; $(COPY) src/ui/$(R3R_UI)/po/$l.mo ../r3r-$(VERSION)-$(PLATFORM)/share/locale/LC_MESSAGES/$l/r3r_$(R3R_UI).mo;)
	$(foreach l, $(subst .mo,,$(notdir $(wildcard src/utils/opml/po/*.mo))), $(MKDIR) ../r3r-$(VERSION)-$(PLATFORM)/share/locale/LC_MESSAGES/$l; $(COPY) src/utils/opml/po/$l.mo ../r3r-$(VERSION)-$(PLATFORM)/share/locale/LC_MESSAGES/$l/r3r_opml.mo;)
	$(PAX) $(PAXFLAGS) ../r3r-$(VERSION)-$(PLATFORM) | \
		gzip -f > ../r3r-$(VERSION)-$(PLATFORM).tar.gz
	$(DELTREE) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)

dist-src: clean
	-$(MKDIR) ../r3r-$(VERSION)
	-$(COPY) -rf * ../r3r-$(VERSION)
	$(PAX) $(PAXFLAGS) ../r3r-$(VERSION) | gzip -cf > r3r-$(VERSION)-src.tar.gz 
	$(DELTREE) ../r3r-$(VERSION)
	$(MOVE) r3r-$(VERSION)-src.tar.gz ..

dist-autopackage: dist-build
	cd scripts/setup && $(MAKE) dist-autopackage

dist-deb: dist-build
	cd scripts/setup && $(MAKE) dist-deb

dist-rpm: dist-build
	cd scripts/setup && $(MAKE) dist-rpm

dist-slackware: dist-build
	cd scripts/setup && $(MAKE) dist-slackware

dist-inno_setup: dist-build
	cd scripts/setup && $(MAKE) dist-inno_setup

# Cleaning rules
clean:
	cd $(srcdir)/doc && $(MAKE) clean
	cd $(srcdir)/icons && $(MAKE) clean
	cd $(srcdir)/scripts/setup && $(MAKE) clean
	cd $(srcdir)/src && $(MAKE) clean
	$(DEL) config.make
	$(DEL) description-pak
	$(foreach ui,$(uis),$(DEL) $(builddir)/r3r-$(ui)$(TARGETEXEEXT); )
