programpath = $(strip $(wildcard $(addsuffix /$(1)$(EXEEXT),$(SEARCHPATH))))

PROGNAME = r3r
VERSION = 2.0-beta3
TIMESTAMP=$(shell date +%s)

PREFIX ?= $(DESTDIR)

ifeq ($(PREFIX),)
ifdef inUnix
PREFIX = /usr/local
else
ifneq ($(OS),)
PREFIX = /
endif
endif
endif

prefix = $(PREFIX)

bindir = $(prefix)/bin
datadir = $(prefix)/share
icondir=$(datadir)/icons
libdir = $(prefix)/lib
localedir = $(datadir)/locale

rootdir ?= $(BASEDIR)

SED ?= sed
TOUCH ?= touch

PCFLAGS += $(DEFS) $(PCFLAGS_BASE) $(PCFLAGS_DEBUG) $(PCFLAGS_EXTRA) \
	$(UNITDIRS)

# Defines, for enabling different features/dialect syntax
DEFS = $(foreach opt, $(DEFS_EXTRA) $(DEFS_SETTINGS) $(DEFS_SOCKETS), $(DEFFLAG)$(opt))

UNITDIRS=$(foreach dir,$(wildcard $(UNIT_DIRS)),$(DIRFLAG)$(dir))

override COMPILER_OPTIONS += $(PCFLAGS)

ifdef inWinNT
inWindows = 1
else
ifdef inCygwin
inWindows = 1
endif #inCygwin
endif # inWinNT

ifdef inUnix
EXEEXT = 
endif

ifndef COMPILER_OVERRIDE
ifneq ($(call programpath,fpc),)
USE_FPC=1
else
ifneq ($(call programpath,gpc)),)
USE_GPC=1
endif # USE_GPC
endif # USE_FPC
endif # COMPILER_OVERRIDE

ifdef USE_FPC
DEFFLAG=-d
PC=fpc
PCFLAGS_BASE=-FU. -Mdelphi -Sh -WR
DIRFLAG=-Fu
ifndef RELEASE
PCFLAGS_DEBUG=-Ci -Co -Cr -gh -gl

ifneq ($(R3R_UI),wx)
PCFLAGS_DEBUG += -Ct
endif # R3R_UI
endif # RELEASE

DEFS_SOCKETS ?= SOCKETS_SYNAPSE

ifneq ($(inWindows),)
DEFS_SETTINGS ?= SETTINGS_REG
else
DEFS_SETTINGS ?= SETTINGS_INI
endif # inWindows

BUILD_SHARED=1

else
ifdef USE_GPC
PC=gpc
PCFLAGS_BASE=--automake --extended-syntax --no-warning
UNITFLAGS=-c
PROGFLAGS=-o $(PROGNAME)$(EXEEXT) $(LDFLAGS)

DEFFLAG=-D
DEFS_SETTINGS ?= SETTINGS_TAB
DEFS_SOCKETS ?= SOCKETS_BSD
DIRFLAG=--unit-path=
PPUEXT=.gpi

ifndef RELEASE
PCFLAGS_DEBUG=-ggdb3
endif # RELEASE
endif # USE_GPC
endif # USE_FPC

ifndef RELEASE
R3R_UI ?= tui
else
R3R_UI ?= wx
endif #RELEASE

export DEFS DEFS_SOCKETS R3R_UI VERSION \
	bindir datadir prefix rootdir

_all: Makefile

all: _all

_clean:
	$(DEL) $(wildcard *$(OEXT))
	$(DEL) $(wildcard *$(PPUEXT))
	$(DEL) $(wildcard *$(RSTEXT))
	$(DEL) $(wildcard *$(STATICLIBEXT))

clean: _clean

%$(PPUEXT): %.pas
	$(PC) $(UNITFLAGS) $(PCFLAGS) $<

Makefile: Makefile.fpc
	-fpcmake -Tall
