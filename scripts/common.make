# vim:filetype=make

programpath = $(firstword $(strip $(wildcard $(addsuffix /$(1)$(EXEEXT),$(SEARCHPATH)))))

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

UNITDIRS=$(sort $(foreach d,$(wildcard $(addsuffix /*,$(UNIT_DIRS))),$(DIRFLAG)$(dir $(d))))

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

ifneq ($(or $(USE_GPC),$(USE_FPC)),)
COMPILER_OVERRIDE=1
endif

ifneq ($(COMPILER_OVERRIDE),)
ifneq ($(USE_FPC),)
PC=$(call programpath,fpc)
else
ifneq ($(USE_GPC),)
PC=$(call programpath,gp)
endif # USE_GPC
endif # USE_FPC
else
PC=$(call programpath,fpc)
ifndef ($(findstring fpc,$(PC)))
USE_FPC=1
else
PC=$(call programpath,gp)
ifneq ($(findstring gp,$(PC)))
USE_GPC=1
else
$(error No Pascal compiler detected)
endif # USE_GPC
endif # USE_FPC
endif # COMPILER_OVERRIDE

ifdef USE_FPC
override COMPILER=FPC $(shell $(PC) -iW)
DELP=delp -eq .

DEFFLAG=-d
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

ifndef RELEASE
R3R_UI ?= tui
else
R3R_UI ?= wx
endif #RELEASE

BUILD_SHARED=1

else
ifdef USE_GPC
override COMPILER=$(shell $(call programpath,gpc) --version | head -n 1)
DELP=$(DEL) $(wildcard *.gpd)

PCFLAGS_BASE=--extended-syntax --no-write-clip-strings \
						 -DFree=Destroy -DPtrUInt=PtrWord $(LDFLAGS)
DEFFLAG=-D
DEFS_SETTINGS ?= SETTINGS_TAB
DEFS_SOCKETS ?= SOCKETS_BSD
DIRFLAG=--unit-path=
PPUEXT=.gpi
EXTRACLEANFILES=.gpd

ifndef RELEASE
PCFLAGS_DEBUG=--pointer-checking --progress-messages \
	--stack-checking -ggdb3
else
PCFLAGS_DEBUG=--no-io-checking --no-range-checking --no-warning
endif # RELEASE

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

%$(PPUEXT): %.pas
	$(PC) $(PCFLAGS) $<

Makefile: Makefile.fpc
	-fpcmake -Tall
