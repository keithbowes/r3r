# vi:filetype=make

SHELL = /bin/sh

ifndef OS_TARGET
ifneq ($(findstring ;,$(PATH)),)
ifdef ComSpec
inDOS=1
ifdef OS
inWindows=1
endif #OS
else
inOtherOS=1
endif #COMSPEC
else
inUnix=1
endif #inUnix
endif #OS_TARGET

ifdef OS_TARGET
ifeq ($(OS_TARGET),linux)
forUnix=1
else
ifneq ($(findstring bsd,$(OS_TARGET)),)
forUnix=1
else
ifeq ($(OS_TARGET),solaris)
forUnix=1
else
ifneq ($(findstring go,$(OS_TARGET)),)
forDOS=1
else
ifeq ($(OS_TARGET),emx)
forDOS=1
forOS2=1
else
ifneq ($(findstring win,$(OS_TARGET)),)
forDOS=1
forWindows=1
endif #forWindows
endif #forOS2
endif #forDOS
endif #Solaris
endif #*BSD
endif #Linux
endif #OS_TARGET

# Now what programs
CUT ?= $(call programpath,cut)
RMRF ?= $(call programpath,rm) -fr
MV ?= mv -f
PWD ?= $(call programpath,pwd)
RM ?= $(call programpath,rm) -f
RMDIR ?= $(call programpath,rmdir)
SED ?= $(call programpath,sed)
TOUCH ?= $(call programpath,touch)

LN ?= $(call programpath,ln) -sf

GIT ?= $(call programpath,git)

ifndef inDOS
CP ?= $(call programpath,cp)
CPR ?= $(CP) -r
ECHO ?= $(call programpath,echo)
INSTALLPROG ?= $(call programpath,install)
else
CP ?= copy
CPR ?= $(CP)
ECHO ?= $(call programpath,gecho)
INSTALLPROG ?= $(call programpath,ginstall)
endif

INSTALL=$(INSTALLPROG) -m 644
INSTALLEXE=$(INSTALLPROG) -m 755
MKDIR=$(INSTALLPROG) -d -m 755

# General prefixes and suffixes
OEXT ?= .o # .obj in Delphi
PPUEXT ?= .ppu
STATICLIBEXT = .a

# OS-dependent prefixes and suffixes
ifdef inUnix
EXEEXT=
SHAREDLIBEXT=.so
SHAREDLIBPREFIX=lib
else
ifdef inDOS
EXEEXT=.exe
SHAREDLIBEXT=.dll
SHAREDLIBPREFIX=
endif #inDOS
endif #inUnix

ifdef forDOS
TARGETEXEEXT=.exe
TARGETSHAREDLIBEXT=.dll
TARGETSHAREDLIBPREFIX=
else
TARGETEXEEXT=
TARGETSHAREDLIBEXT=.so
TARGETSHAREDLIBPREFIX=lib
endif

TARGETEXEEXT ?= $(EXEEXT)
TARGETSHAREDLIBEXT ?= $(SHAREDLIBEXT)
TARGETSHAREDLIBPREFIX ?= $(SHAREDLIBPREFIX)

ifdef inDOS
SEARCHPATH=$(subst ;, ,$(subst \,/,$(PATH)))
else
SEARCHPATH=$(subst :, ,$(PATH))
endif

# Misc functions
programpath = $(firstword $(strip $(wildcard $(addsuffix /$(1)$(EXEEXT),$(SEARCHPATH)))))

ifneq ($(call programpath,$(notdir $(SHELL))),)
checklib=$(shell $(ECHO) "Checking for -l$(1)... "; $(ECHO) 'program Check; {$$linklib $(1)} begin end.' > check.pas; $(PC) $(PCFLAGS) check.pas > /dev/null 2>&1; if test $$? -eq 0; then ($(ECHO) "yes"; if test ! -f status.sh; then $(ECHO) "ok=1; export ok" > status.sh; fi); else ($(ECHO) "no"; $(ECHO) "ok=0; export ok" > status.sh); fi;)
checkprog=$(shell $(ECHO) "Checking for $(1)\'s path... "; if test -z $(shell $(ECHO) $(call programpath,$(1))); then ($(ECHO) "not found"; $(ECHO) "ok=0; export ok" > status.sh); else $(ECHO) $(call programpath,$(1)); fi;)
checkunit=$(shell $(ECHO) "Checking for unit $(1)... "; $(ECHO) "program Check; uses $(1); begin end." > check.pas; $(PC) $(PCFLAGS) check.pas > /dev/null 2>&1; if test $$? -eq 0; then ($(ECHO) "yes"; if test ! -f status.sh; then $(ECHO) "ok=1; export ok" > status.sh; fi); else ($(ECHO) "no"; $(ECHO) "ok=0; export ok" > status.sh); fi;)
success=$(shell source ./status.sh; if test $$ok -eq 1; then $(ECHO) "You can safely build now"; else $(ECHO) "You\'re missing some requirements\; can\'t build"; fi; $(RM) $(wildcard status.sh))
else
checklib=$(shell $(ECHO) "Testing -l$(1)..."; $(ECHO) "program Check; {$$linklib} begin end." > check.pas; $(PC) $(PCFLAGS) check.pas)
checkprog=$($shell $(ECHO) "Checking for $(1)'s path; $(ECHO) $(call programpath,$(1)))
checkunit=$(shell $(ECHO) "Testing unit $(1)..."; $(ECHO) "program Check"; uses $(1); begin end." > check.pas" > check.pas; $(PC) $(PCFLAGS) check.pas)
success=$(shell $(ECHO) Consult the messages above to ascertain whether you can successfully compile)
endif

# OK, the actual start of the Makefile
VERSION = $(shell $(GIT) describe --always --tag)

SRCDIR ?= .
top_srcdir ?= $(SRCDIR)
srcdir ?= $(top_srcdir)

BUILDDIR ?= .
top_builddir ?= $(BUILDDIR)
builddir ?= $(top_builddir)

export SRCDIR BUILDDIR

EXEOUT ?= $(builddir)

sinclude config.make

PREFIX ?= $(DESTDIR)

ifeq ($(PREFIX),)
ifdef inUnix
PREFIX = /usr/local
else
ifdef OS
PREFIX = /
else
PREFIX = /usr/local
endif # OS
endif # inUnix
endif # PREFIX

prefix = $(PREFIX)

appdir = $(datadir)/applications
bindir = $(prefix)/bin
datadir = $(prefix)/share
docdir = $(datadir)/doc/r3r
icondir = $(datadir)/icons
incdir = $(prefix)/include
libdir = $(prefix)/lib
localedir = $(datadir)/locale
rdatadir = $(datadir)/r3r
skindir = $(rdatadir)/skins

uis = html tui wx

PCFLAGS += $(DEFS) $(PCFLAGS_BASE) $(PCFLAGS_DEBUG) $(PCFLAGS_EXTRA) \
	$(UNITDIRS)

# Defines, for enabling different features/dialect syntax
DEFS = $(foreach opt, $(DEFS_EXTRA) $(DEFS_SETTINGS) $(DEFS_SOCKETS), $(DEFFLAG)$(opt))

UNITDIRS=$(sort $(foreach d,$(wildcard $(addsuffix /*,$(UNIT_DIRS))),$(DIRFLAG)$(dir $(d))))
override COMPILER_OPTIONS += $(PCFLAGS)

USE_EXPAT ?= 1
USE_ICONV ?= 1
USE_IDN ?= 1
USE_LIBIDN2 ?= 0
USE_NLS ?= 1
DEFS_REGEXP ?= REGEXP_PCRE

EXPAT_VERSION ?= 2.0.1

USE_READLINE ?= 1

NO_NCURSES ?= 0

NO_X ?= 1
ifdef forUnix
ifeq ($(NO_X),0)
override DEFS_EXTRA+=USE_X
endif
endif

USE_LIBEDIT ?= 0
USE_LIBICONV ?= 0
USE_SSL ?= 1
SSL=$(if $(findstring $(USE_SSL),0),_nossl,$(if $(findstring $(USE_SSL),1),_ssl,))

ifneq ($(USE_NLS),0)
override DEFS_EXTRA+=USE_NLS
endif

ifneq ($(USE_LIBIDN2),0)
override DEFS_EXTRA+=USE_LIBIDN2
ifeq ($(USE_NLS),0)
$(error LibIDN2 *requires* gettext)
endif
else
ifneq ($(USE_IDN),0)
override DEFS_EXTRA+=USE_IDN
endif
endif

ifneq ($(USE_ICONV),0)
override DEFS_EXTRA+=USE_ICONV
endif

ifneq ($(USE_EXPAT),0)
override DEFS_EXTRA+=USE_EXPAT
ifneq ($(EXPAT_VERSION),)
override DEFS_EXTRA+=EXPAT_$(subst .,_,$(EXPAT_VERSION))
endif

ifneq ($(findstring EXPAT_2_1,$(DEFS_EXTRA)),)
override DEFS_EXTRA+=EXPAT_2_0_1
endif

ifneq ($(findstring EXPAT_2_0_1,$(DEFS_EXTRA)),)
override DEFS_EXTRA+=EXPAT_2_0
endif

ifneq ($(findstring EXPAT_2_0,$(DEFS_EXTRA)),)
override DEFS_EXTRA+=EXPAT_1_2
endif

ifneq ($(findstring EXPAT_1_2,$(DEFS_EXTRA)),)
override DEFS_EXTRA+=EXPAT_1_1
endif

ifneq ($(findstring EXPAT_1_1,$(DEFS_EXTRA)),)
override DEFS_EXTRA+=EXPAT_1_0
endif
endif

ifneq ($(NO_NCURSES),0)
override DEFS_EXTRA+=NO_NCURSES
endif

ifneq ($(USE_LIBICONV),0)
override DEFS_EXTRA+=USE_LIBICONV
endif

ifeq ($(DEFS_REGEXP),REGEXP_PCRE)
override DEFS_EXTRA+=USE_PCRE
else
ifeq ($(DEFS_REGEXP),REGEXP_REGEXPR)
override DEFS_EXTRA+=USE_REGEXPR
endif
endif

ifneq ($(USE_READLINE),0)
override DEFS_EXTRA+=USE_READLINE
endif

ifneq ($(USE_LIBEDIT),0)
override DEFS_EXTRA+=USE_LIBEDIT
endif

ifneq ($(USE_SSL),0)
override DEFS_EXTRA+=USE_SSL
endif

R3R_UI ?= tui

FPC ?= $(call programpath,fpc)
PC=$(FPC)
DEFFLAG=-d
PCFLAGS_BASE=-Mclass -Mclassicprocvars -Sh -FE$(EXEOUT) -FU$(builddir) -Fu$(builddir)

ifdef forWindows
override PCFLAGS_BASE+=-WR
endif

DIRFLAG=-Fu
ifdef DEBUG
PCFLAGS_DEBUG=-Ci -Co -Cr -gh -gl
ifeq ($(USE_VALGRIND),1)
override PCFLAGS_DEBUG+=-gv
endif

ifneq ($(R3R_UI),wx)
PCFLAGS_DEBUG += -Ct
endif # R3R_UI
else
PCFLAGS_DEBUG=-CX -Xs -XX
endif # DEBUG

DEFS_SOCKETS ?= SOCKETS_SYNAPSE

ifdef forWindows
DEFS_SETTINGS ?= SETTINGS_REG
else
DEFS_SETTINGS ?= SETTINGS_INI
endif # inWindows

ifdef CPU_TARGET
override PCFLAGS_BASE+=-P$(CPU_TARGET)
endif

ifdef OS_TARGET
override PCFLAGS_BASE+=-T$(OS_TARGET)
endif

BUILD_SHARED ?= 1

ifeq ($(DEFS_SETTINGS),SETTINGS_LIBINI)
override DEFS_EXTRA+=INI_ADD_EXTRAS
endif

# Let's try to figure out what OS we're using
HOST ?= $(PLATFORM)
ifneq ($(HOST),)
CPU_TARGET ?= $(shell $(ECHO) $(HOST) | $(CUT) -d '-' -f 1)
OS_TARGET ?= $(shell $(ECHO) $(HOST) | $(CUT) -d '-' -f 3)
endif

ifeq ($(CPU_TARGET),)
override CPU_TARGET = $(shell $(PC) -iTP)
endif

ifeq ($(OS_TARGET),)
override OS_TARGET = $(shell $(PC) -iTO)
endif

ARCH ?= $(CPU_TARGET)
PKGRELEASE ?=1

export CC DEFS DEFS_SOCKETS DESTDIR R3R_UI VERSION \
	OS_TARGET CPU_TARGET \
	bindir datadir prefix rootdir

default: all

_all:
ifeq ($(wildcard $(builddir)),)
	@$(MKDIR) $(builddir)
endif

all: _all

_clean:
	$(RM) $(wildcard *$(OEXT))
	$(RM) $(wildcard *$(PPUEXT))
	$(RM) $(wildcard *$(STATICLIBEXT))
	$(RM) $(wildcard *.mo) $(wildcard *.po~)

cleanbuild:
ifneq ($(BUILDDIR),$(SRCDIR))
	cd $(BUILDDIR) && $(MAKE) _clean
endif

clean: _clean cleanbuild

distclean: clean

%$(PPUEXT): %.pas
	$(PC) $(PCFLAGS) $<

# Gettext

.SUFFIXES:
.SUFFIXES: .mo .po .pot

po_files = $(wildcard *.po)
LINGUAS ?= $(subst .po,,$(po_files))
ifneq ($(LINGUAS),)
mo_files = $(addsuffix .mo,$(LINGUAS))
else
mo_files = $(subst .po,.mo,$(po_files))
override LINGUAS := $(subst .mo,,$(mo_files))
endif

MSGFMT ?= $(call programpath,msgfmt)
MSGMERGE ?= $(call programpath,msgmerge)
XGETTEXT ?= $(call programpath,xgettext)

MSGFMTFLAGS ?= -c --statistics
