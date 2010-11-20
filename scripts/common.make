# vim:filetype=make

programpath = $(firstword $(strip $(wildcard $(addsuffix /$(1)$(EXEEXT),$(SEARCHPATH)))))

VERSION = 2.1

PREFIX ?= $(DESTDIR)

ifeq ($(PREFIX),)
ifdef inUnix
PREFIX = /usr/local
else
ifdef OS
PREFIX = /
endif # inUnix
endif # OS
endif # PREFIX

ifeq ($(SHAREDLIBEXT),.so)
	SHAREDLIBPREFIX = lib
else
	SHAREDLIBPREFIX =
endif

prefix = $(PREFIX)

bindir = $(prefix)/bin
datadir = $(prefix)/share
icondir = $(datadir)/icons
libdir = $(prefix)/lib
localedir = $(datadir)/locale

rootdir ?= $(BASEDIR)

SED ?= sed
TOUCH ?= touch

PCFLAGS += $(DEFS) $(PCFLAGS_BASE) $(PCFLAGS_DEBUG) $(PCFLAGS_EXTRA) \
	$(UNITDIRS)

# Defines, for enabling different features/dialect syntax
DEFS = $(foreach opt, $(DEFS_EXTRA) $(DEFS_SETTINGS) $(DEFS_SOCKETS), $(DEFFLAG)$(opt))

UNITDIRS=$(sort $(foreach d,$(wildcard $(addsuffix /*,$(UNIT_DIRS))),$(DIRFLAG)$(dir $(d))))

override COMPILER_OPTIONS += $(PCFLAGS)

ifdef inWinNT
inWindows = 1
else
ifdef inCygWin
inWindows = 1
endif # inCygWin
endif # inWinNT

ifdef inUnix
EXEEXT =
endif

ifneq ($(or $(USE_GPC),$(USE_FPC)),)
COMPILER_OVERRIDE=1
endif

ifdef COMPILER_OVERRIDE
ifdef USE_FPC
PC=$(call programpath,fpc)
else
ifdef USE_GPC
PC=$(call programpath,gp)
CC=$(call programpath,gpc)
export CC
endif # USE_GPC
endif # USE_FPC
else
PC=$(call programpath,fpc)
ifeq ($(findstring fpc,$(PC)),fpc)
USE_FPC=1
else
PC=$(call programpath,gp)
CC=$(call programpath,gpc)
export CC
ifeq ($(findstring gp,$(PC)),gp)
USE_GPC=1
else
$(error No Pascal compiler detected)
endif # USE_GPC
endif # USE_FPC
endif # COMPILER_OVERRIDE

ifdef USE_FPC
override COMPILER=FPC $(shell $(PC) -iW)
PLATFORM=$(shell $(PC) -iTO)-$(shell $(PC) -iTP)
DELP=delp -eq .

DEFFLAG=-d
PCFLAGS_BASE=-FU. -Mdelphi -Sh -WR
DIRFLAG=-Fu
ifdef DEBUG
PCFLAGS_DEBUG=-Ci -Co -Cr -gh -gl

ifneq ($(R3R_UI),wx)
PCFLAGS_DEBUG += -Ct
endif # R3R_UI
else
PCFLAGS_DEBUG=-CX -Xs -XX
endif # DEBUG

ifeq ($(R3R_UI), tui)
ifdef inUnix
override DEFS_EXTRA += HAS_SCREENHEIGHTWIDTH
endif # inUnix
endif # R3R_UI

DEFS_SOCKETS ?= SOCKETS_SYNAPSE

ifdef inWindows
DEFS_SETTINGS ?= SETTINGS_REG
else
DEFS_SETTINGS ?= SETTINGS_INI
endif # inWindows

ifdef DEBUG
R3R_UI ?= tui
else
R3R_UI ?= wx
endif # DEBUG

BUILD_SHARED ?= 1

else
ifdef USE_GPC
GPC=$(call programpath,gpc)
override COMPILER=GPC $(shell $(GPC) -dumpversion)
PLATFORM=$(shell $(GPC) -dumpmachine)
DELP=$(DEL) $(wildcard *.gpd)

PCFLAGS_BASE=--extended-syntax --no-write-clip-strings \
						 -DFree=Destroy -DPtrUInt=PtrWord $(LDFLAGS)
DEFFLAG=-D
DEFS_SETTINGS ?= SETTINGS_TAB
DEFS_SOCKETS ?= SOCKETS_BSD
DIRFLAG=--unit-path=
PPUEXT=.gpi

ifdef DEBUG
PCFLAGS_DEBUG=--pointer-checking --progress-messages \
	--stack-checking -ggdb3
else
PCFLAGS_DEBUG=--no-io-checking --no-pointer-checking \
							--no-range-checking --no-stack-checking \
							--no-warning
endif # DEBUG

R3R_UI ?= tui
endif # USE_GPC
endif # USE_FPC

export DEFS DEFS_SOCKETS R3R_UI VERSION \
	bindir datadir prefix rootdir

_all: Makefile

all: _all

_clean:
	$(DEL) $(wildcard *$(OEXT))
	$(DEL) $(wildcard *$(PPUEXT))
	$(DEL) $(wildcard *$(RSTEXT))
	$(DEL) $(wildcard *$(STATICLIBEXT))
	$(DELP)

clean: _clean

distclean: clean

%$(PPUEXT): %.pas
	$(PC) $(PCFLAGS) $<

Makefile: Makefile.fpc
	-fpcmake -Tall
