USE_PAX ?= 1
USE_XZ ?= 1

ifneq ($(USE_PAX),0)
	PAX = $(call programpath,pax)
	PAXFLAGS = -w -x ustar
else
	PAX = $(call programpath,tar)
	PAXFLAGS = -cf -
endif

ifneq ($(USE_XZ),0)
GZIP = $(call programpath,xz)
GZIPFLAGS = -c
GZIPEXT = xz
else
GZIP = $(call programpath,gzip)
GZIPFLAGS ?= -cf
GZIPEXT = gz
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
ifneq ($(DEFS_SOCKETS), SOCKETS_NONE)
	$(error Unsupported sockets library)
endif
endif
endif
ifeq ($(R3R_UI),tui)
ifeq ($(NO_NCURSES),0)
ifdef inUnix
	@$(ECHO) $(call checkunit,nCRT)
	@$(ECHO) $(call checkunit,nCurses)
else
	@$(ECHO) $(call checkunit,CRT)
endif
else
	@$(ECHO) $(call checkunit,CRT)
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
ifneq ($(USE_EXPAT),0)
	@$(ECHO) $(call checklib,expat)
endif
ifneq ($(USE_ICONV),0)
ifdef inUnix
	@$(ECHO) $(call checklib,iconv)
else
	@$(ECHO) $(call checklib,libiconv)
endif
endif
ifneq ($(USE_IDN),0)
	@$(ECHO) $(call checklib,idn)
endif
ifneq ($(USE_NLS),0)
	@$(ECHO) $(call checklib,intl)
endif
ifneq ($(USE_PCRE),0)
	@$(ECHO) $(call checklib,pcre)
endif
ifneq ($(USE_READLINE),0)
	@$(ECHO) $(call checklib,readline)
endif
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
	$(PAX) $(PAXFLAGS) doc/api | $(GZIP) $(GZIPFLAGS) > ../r3r-$(VERSION)-api.tar.$(GZIPEXT)


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
	$(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)
	$(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/bin
ifdef inUnix
	$(INSTALLEXE) $(srcdir)/r3r $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/bin
endif
	$(INSTALLEXE) $(builddir)/r3r-tui$(EXEEXT) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/bin
	$(INSTALLEXE) $(builddir)/r3r-wx$(EXEEXT) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/bin
	$(INSTALLEXE) $(srcdir)/r3r-settitle ../r3r-$(VERSION)-$(PLATFORM)/bin
	$(INSTALLEXE) $(srcdir)/src/utils/opml/r3r_opml$(EXEEXT) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/bin
	$(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/lib
	$(INSTALLEXE) $(srcdir)/src/libr3r/$(SHAREDLIBPREFIX)libr3r_shared$(SHAREDLIBEXT) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/lib
	$(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/include
	$(COPY) $(srcdir)/src/ui/wx/libr3r.h $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/include
	$(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/icons
	$(COPY) $(srcdir)/icons/r3r.png  $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/icons
	$(foreach l, $(subst .mo,,$(notdir $(wildcard $(srcdir)/src/libr3r/po/*.mo))), $(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/locale/$l/LC_MESSAGES; $(COPY) $(srcdir)/src/libr3r/po/$l.mo $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/locale/$l/LC_MESSAGES/libr3r.mo;)
	$(foreach l, $(subst .mo,,$(notdir $(wildcard $(srcdir)/src/ui/tui/po/*.mo))), $(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/locale/$l/LC_MESSAGES; $(COPY) $(srcdir)/src/ui/tui/po/$l.mo $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/locale/$l/LC_MESSAGES/r3r_tui.mo;)
	$(foreach l, $(subst .mo,,$(notdir $(wildcard $(srcdir)/src/ui/wx/po/*.mo))), $(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/locale/$l/LC_MESSAGES; $(COPY) $(srcdir)/src/ui/wx/po/$l.mo $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/locale/$l/LC_MESSAGES/r3r_wx.mo;)
	$(foreach l, $(subst .mo,,$(notdir $(wildcard $(srcdir)/src/utils/opml/po/*.mo))), $(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/locale/$l/LC_MESSAGES; $(COPY) $(srcdir)/src/utils/opml/po/$l.mo $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/locale/$l/LC_MESSAGES/r3r_opml.mo;)
	$(PAX) $(PAXFLAGS) ../r3r-$(VERSION)-$(PLATFORM) | \
		$(GZIP) $(GZIPFLAGS) > ../r3r-$(VERSION)-$(PLATFORM).tar.$(GZIPEXT)
	$(DELTREE) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)

dist-src: clean
	cd $(srcdir)
	-$(MKDIR) ../r3r-$(VERSION)
	-$(COPY) -rf * ../r3r-$(VERSION)
	cd .. && $(PAX) $(PAXFLAGS) r3r-$(VERSION) | \
		$(GZIP) $(GZIPFLAGS) > r3r-$(VERSION)-src.tar.$(GZIPEXT)
	$(DELTREE) ../r3r-$(VERSION)

dist-autopackage: dist-build
	cd scripts/setup && $(MAKE) dist-autopackage

dist-deb: all
	cd scripts/setup && $(MAKE) dist-deb

dist-rpm: all
	cd scripts/setup && $(MAKE) dist-rpm

dist-slackware: all
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
