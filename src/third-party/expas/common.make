programpath = $(strip $(wildcard $(addsuffix /$(1)$(EXEEXT),$(SEARCHPATH))))

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
DEFS = $(foreach opt, $(DEFS_EXTRA) $(DEFS_SETTINGS), $(DEFFLAG)$(opt))

UNITDIRS=$(foreach dir,$(UNIT_DIRS),$(DIRFLAG)$(dir))

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
PCFLAGS_BASE=-FU. -Mobjfpc -Sh -WR
DIRFLAG=-Fu
ifndef RELEASE
PCFLAGS_DEBUG=-Ci -Co -Cr -gh -gl

ifneq ($(R3R_UI),wx)
# The wx UI incorrectly reports stack checking problems whenever
# threads are used
PCFLAGS_DUBUG+=-Ct
endif # R3R_UI
endif # RELEASE

ifneq ($(inWindows),)
DEFS_SETTINGS ?= SETTINGS_REG
else
DEFS_SETTINGS ?= SETTINGS_INI
endif # inWindows

else
ifdef USE_GPC
PC=gpc
PCFLAGS_BASE=--automake --cstrings-as-strings --no-warnings --pointer-arithmetic
DEFFLAG=-D
DEFS_SETTINGS ?= SETTINGS_BIN
DIRFLAG=-B

ifndef RELEASE
PCFLAGS_DEBUG=-ggdb3
endif # RELEASE
endif # USE_GPC
endif # USE_FPC

R3R_UI ?= tui

export DEFS PCFLAGS R3R_UI VERSION \
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
	$(PC) $(PCFLAGS) $<

Makefile: Makefile.fpc
	-fpcmake -Tall
