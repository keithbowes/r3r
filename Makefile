PAX = $(call programpath,pax)
PAXFLAGS = -w -x ustar

XZ = $(call programpath,xz)
XZFLAGS = -c

CSUM ?= $(call programpath,sha256sum)
CSUMFLAGS ?=
CSUMOUT ?= r3r-$(VERSION).sha256

STRIP ?= $(call programpath,strip)
STRIPFLAGS ?=

UPX ?= $(call programpath,upx)
UPXFLAGS ?=

sign = cd $(srcdir)/..; $(if $(wildcard $(srcdir)/../$(CSUMOUT)),$(CSUM) $(CSUMFLAGS) $1 >> $(CSUMOUT),$(CSUM) $(CSUMFLAGS) $1 > $(CSUMOUT))

.PHONY: check

include scripts/common.make

all build: docs
	cd $(srcdir)/icons && $(MAKE)
	cd $(srcdir)/src && $(MAKE)
	$(MV) $(srcdir)/src/ui/$(R3R_UI)/r3r$(TARGETEXEEXT) $(builddir)/r3r-$(R3R_UI)$(TARGETEXEEXT)

_configure:
	@$(ECHO) $(call checkprog,fpc)
	@$(ECHO) $(call checkunit,SysUtils)
ifeq ($(DEFS_SOCKETS),SOCKETS_SYNAPSE)
	@$(ECHO) $(call checkunit,BlckSock)
else
ifeq ($(DEFS_SOCKETS),SOCKETS_LIBCURL)
	@$(ECHO) $(call checklib,curl)
	@$(ECHO) $(call checkunit,CurlCore)
	@$(ECHO) $(call checkunit,CurlEasy)
	@$(ECHO) $(call checkunit,CurlSList)
	@$(ECHO) $(call checkunit,CurlVer)
else
ifneq ($(DEFS_SOCKETS),SOCKETS_NONE)
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
ifneq ($(R3R_UI),html)
	$(error Unknown UI)
endif
endif
endif
ifeq ($(DEFS_SETTINGS),SETTINGS_INI)
	@$(ECHO) $(call checkunit,IniFiles)
else
ifeq ($(DEFS_SETTINGS),SETTINGS_LIBINI)
	@$(ECHO) $(call checkunit,LibIni)
	@$(ECHO) $(call checklib,ini)
else
ifeq ($(DEFS_SETTINGS),SETTINGS_REG)
	@$(ECHO) $(call checkunit,Registry)
endif
endif
endif
ifneq ($(USE_EXPAT),0)
ifdef forWindows
	@$(ECHO) $(call checklib,libexpat-1)
else
	@$(ECHO) $(call checklib,expat)
endif
endif
ifneq ($(USE_ICONV),0)
ifneq ($(USE_LIBICONV),0)
ifndef forWindows
	@$(ECHO) $(call checklib,iconv)
else
	@$(ECHO) $(call checklib,libiconv)
endif
endif
endif
ifneq ($(USE_IDN),0)
	@$(ECHO) $(call checklib,idn)
endif
ifneq ($(USE_NLS),0)
	@$(ECHO) $(call checklib,intl)
endif
ifeq ($(DEFS_REGEXP),REGEXP_PCRE)
ifdef forWindows
	@$(ECHO) $(call checklib,libpcre-0)
else
	@$(ECHO) $(call checklib,pcre)
endif
else
ifeq ($(DEFS_REGEXP),REGEXP_REGEXPR)
	@$(ECHO) $(call checkunit,RegExpr)
endif
endif
ifneq ($(USE_READLINE),0)
ifeq ($(USE_LIBEDIT),0)
	@$(ECHO) $(call checklib,readline)
else
	@$(ECHO) $(call checklib,edit)
endif
endif
ifdef forWindows
	@$(ECHO) $(call checkprog,png2ico)
endif
	-@$(RM) $(wildcard *check*)
	@$(ECHO) $(call success)

check_clean: _clean
	$(RM) $(wildcard check.pas check$(TARGETEXEEXT) check$(OEXT) check$(PPUEXT))

install: install-docs install-header install-lib install-prog

install-docs:
	cd $(srcdir)/doc && $(MAKE) install

install-header:
	cd src && $(MAKE) install-header

install-lib:
	cd src && $(MAKE) install-lib

install-prog:
	$(MKDIR) $(bindir)
	cd $(srcdir)/icons && $(MAKE) install
	cd $(srcdir)/scripts/setup && $(MAKE) install
	cd $(srcdir)/src && $(MAKE) install-prog
	cd $(srcdir)/src/libr3r/po && $(MAKE) install
	$(INSTALLEXE) $(builddir)/r3r-$(R3R_UI)$(TARGETEXEEXT) $(bindir)
	$(INSTALLEXE) $(srcdir)/r3r $(bindir)
ifeq ($(R3R_UI),tui)
	$(INSTALLEXE) $(srcdir)/r3r-settitle $(bindir)
endif

install-strip: install
	-$(STRIP) $(STRIPFLAGS) $(bindir)/r3r-$(R3R_UI)$(TARGETEXEEXT) \
		$(bindir)/r3r-conv$(TARGETEXEEXT) $(bindir)/r3r_opml$(TARGETEXEEXT) \
		$(libdir)/$(SHAREDLIBPREFIX)libr3r_shared$(SHAREDLIBEXT)
	-$(UPX) $(UPXFLAGS) $(bindir)/r3r-$(R3R_UI)$(TARGETEXEEXT) \
		$(bindir)/r3r-conv$(TARGETEXEEXT) $(bindir)/r3r_opml$(TARGETEXEEXT) \
		$(libdir)/$(SHAREDLIBPREFIX)libr3r_shared$(SHAREDLIBEXT)

# Documentation rules
docs:
	cd $(srcdir)/doc && $(MAKE)

dist-docs: docs
	$(PAX) $(PAXFLAGS) doc/api | $(XZ) $(XZFLAGS) > ../r3r-$(VERSION)-api.tar.xz
	$(call sign,r3r-$(VERSION)-api.tar.xz)

# Uninstallation rules
uninstall:
	cd $(srcdir)/doc && $(MAKE) uninstall
	cd $(srcdir)/icons && $(MAKE) uninstall
	cd $(srcdir)/src && $(MAKE) uninstall
	$(RM) $(wildcard $(bindir)/r3r)
	$(foreach ui,$(uis),$(RM) $(wildcard $(bindir)/r3r-$(ui)$(TARGETEXEEXT)); )
	$(RM) $(wildcard $(bindir)/r3r-settitle)
	-$(RMDIR) $(wildcard $(bindir))
	$(RM) $(wildcard $(appdir)/r3r.desktop)
	$(RMRF) $(wildcard $(rdatadir))
	-$(RMDIR) $(wildcard $(prefix))

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
	$(INSTALLEXE) $(srcdir)/src/utils/conv/r3r-conv$(EXEEXT) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/bin
	$(INSTALLEXE) $(srcdir)/src/utils/opml/r3r_opml$(EXEEXT) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/bin
	$(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/lib
	$(INSTALLEXE) $(srcdir)/src/libr3r/$(SHAREDLIBPREFIX)libr3r_shared$(SHAREDLIBEXT) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/lib
	$(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/include
	$(CP) $(srcdir)/src/ui/wx/libr3r.h $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/include
	$(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/icons
	$(CP) $(srcdir)/icons/r3r.png  $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/icons
	$(foreach l, $(subst .mo,,$(notdir $(wildcard $(srcdir)/src/libr3r/po/*.mo))), $(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/locale/$l/LC_MESSAGES; $(CP) $(srcdir)/src/libr3r/po/$l.mo $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/locale/$l/LC_MESSAGES/libr3r.mo;)
	$(foreach l, $(subst .mo,,$(notdir $(wildcard $(srcdir)/src/ui/tui/po/*.mo))), $(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/locale/$l/LC_MESSAGES; $(CP) $(srcdir)/src/ui/tui/po/$l.mo $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/locale/$l/LC_MESSAGES/r3r_tui.mo;)
	$(foreach l, $(subst .mo,,$(notdir $(wildcard $(srcdir)/src/ui/wx/po/*.mo))), $(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/locale/$l/LC_MESSAGES; $(CP) $(srcdir)/src/ui/wx/po/$l.mo $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/locale/$l/LC_MESSAGES/r3r_wx.mo;)
	$(foreach l, $(subst .mo,,$(notdir $(wildcard $(srcdir)/src/utils/opml/po/*.mo))), $(MKDIR) $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/locale/$l/LC_MESSAGES; $(CP) $(srcdir)/src/utils/opml/po/$l.mo $(srcdir)/../r3r-$(VERSION)-$(PLATFORM)/share/locale/$l/LC_MESSAGES/r3r_opml.mo;)
	$(PAX) $(PAXFLAGS) ../r3r-$(VERSION)-$(PLATFORM) | \
		$(XZ) $(XZFLAGS) > ../r3r-$(VERSION)-$(PLATFORM).tar.xz
	$(RMRF) $(wildcard $(srcdir)/../r3r-$(VERSION)-$(PLATFORM))
	$(call sign,r3r-$(VERSION)-$(PLATFORM).tar.xz)

dist-src: clean
	cd $(srcdir)
	-$(MKDIR) ../r3r-$(VERSION)
	-$(CP) -rf * ../r3r-$(VERSION)
	cd .. && $(PAX) $(PAXFLAGS) r3r-$(VERSION) | \
		$(XZ) $(XZFLAGS) > r3r-$(VERSION)-src.tar.xz
	$(RMRF) $(wildcard ../r3r-$(VERSION))
	$(call sign,r3r-$(VERSION)-src.tar.xz)

dist-rpm:
	cd $(srcdir)/scripts/setup && $(MAKE) dist-rpm CITARGET="$(CITARGET)"
	$(call sign,r3r_$(R3R_UI)$(SSL)-$(subst -,_,$(VERSION))-$(PKGRELEASE).$(ARCH).rpm)

dist-rpm-dev:
	$(MAKE) dist-rpm CITARGET="$(MAKE) install-header" R3R_UI=devel

dist-rpm-docs:
	$(MAKE) dist-rpm CITARGET="$(MAKE) install-docs" R3R_UI=docs

dist-rpm-prog:
	$(MAKE) dist-rpm CITARGET="$(MAKE) install-prog" R3R_UI=$(R3R_UI)

dist-rpm-shared:
	$(MAKE) dist-rpm CITARGET="$(MAKE) install-lib" R3R_UI=shared

dist-inno_setup: OS_TARGET=win32
dist-inno_setup: all
	cd $(srcdir)/scripts/setup && $(MAKE) dist-inno_setup
	$(call sign,r3r_$(R3R_UI)-$(VERSION)-setup.exe)

# Cleaning rules
clean:
	cd $(srcdir)/doc && $(MAKE) clean
	cd $(srcdir)/icons && $(MAKE) clean
	cd $(srcdir)/scripts/setup && $(MAKE) clean
	cd $(srcdir)/src && $(MAKE) clean
	$(RM) $(wildcard config.make)
	$(RM) $(wildcard description-pak)
	$(foreach ui,$(uis),$(RM) $(wildcard $(builddir)/r3r-$(ui)$(TARGETEXEEXT)); )
