# vi:filetype=make

SHELL = /bin/sh

# Let's try to figure out what OS we're using
ifndef OS_TARGET
ifneq ($(findstring :,$(PATH)),)
inUnix=1
else
ifdef COMSPEC
inDOS=1
ifdef OS
inWindows=1
endif #OS
else
inOtherOS=1
endif #COMSPEC
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
endif #inWindows
endif #inOS2
endif #inDOS
endif #Solaris
endif #*BSD
endif #Linux
endif #OS_TARGET

# Now what programs
DELTREE ?= $(call programpath,rm) -fr
MOVE ?= mv -f
PWD ?= $(call programpath,pwd)
RM ?= $(call programpath,rm) -f
RMDIR ?= $(call programpath,rmdir)
SED ?= $(call programpath,sed)
TOUCH ?= $(call programpath,touch)

MSGFMT ?= $(call programpath,msgfmt)
MSGMERGE ?= $(call programpath,msgmerge)

MSGFMTFLAGS ?= --statistics

ifndef inDOS
COPY ?= $(call programpath,cp)
ECHO ?= $(call programpath,echo)
INSTALLPROG ?= $(call programpath,install)
else
COPY ?= copy
ECHO ?= $(call programpath,gecho)
INSTALLPROG ?= $(call programpath,ginstall)
endif

INSTALL=$(INSTALLPROG) -m 644
INSTALLEXE=$(INSTALLPROG) -m 755
MKDIR=$(INSTALLPROG) -d -m 755

CP=$(COPY)
DEL=$(RM)
MV=$(MOVE)

# General prefixes and suffixes
GPDEXT ?= *.gpd # GNU Pascal
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

ifdef forUnix
TARGETEXEEXT=
TARGETSHAREDLIBEXT=.so
TARGETSHAREDLIBPREFIX=lib
else
ifdef forDOS
TARGETEXEEXT=.exe
TARGETSHAREDLIBEXT=.dll
TARGETSHAREDLIBPREFIX=
endif
endif

TARGETEXEEXT ?= $(EXEEXT)
TARGETSHAREDLIBEXT ?= $(SHAREDLIBEXT)
TARGETSHAREDLIBPREFIX ?= $(SHAREDLIBPREFIX)

ifdef inDOS
SEARCHPATH=$(subst :, ,$(subst ;,:,$(PATH)))
else
SEARCHPATH=$(subst :, ,$(PATH))
endif

# Misc functions
programpath = $(firstword $(strip $(wildcard $(addsuffix /$(1)$(EXEEXT),$(SEARCHPATH)))))

ifneq ($(call programpath,$(notdir $(SHELL))),)
checklib=$(shell $(ECHO) "Checking for -l$(1)... "; $(ECHO) 'program Check; {$$linklib $(1)} begin end.' > check.pas; $(PC) $(PCFLAGS) check.pas > /dev/null 2>&1; if test $$? -eq 0; then ($(ECHO) "yes"; if test ! -f status.sh; then $(ECHO) "export ok=1" > status.sh; fi); else ($(ECHO) "no"; $(ECHO) "export ok=0" > status.sh); fi;)
checkprog=$(shell $(ECHO) "Checking for $(1)\'s path... "; if test -z $(shell $(ECHO) $(call programpath,$(1))); then ($(ECHO) "not found"; $(ECHO) "export ok=0" > status.sh); else $(ECHO) $(call programpath,$(1)); fi;)
checkunit=$(shell $(ECHO) "Checking for unit $(1)... "; $(ECHO) "program Check; uses $(1); begin end." > check.pas; $(PC) $(PCFLAGS) check.pas > /dev/null 2>&1; if test $$? -eq 0; then ($(ECHO) "yes"; if test ! -f status.sh; then $(ECHO) "export ok=1" > status.sh; fi); else ($(ECHO) "no"; $(ECHO) "export ok=0" > status.sh); fi;)
success=$(shell source ./status.sh; if test $$ok -eq 1; then $(ECHO) "You can safely build now"; else $(ECHO) "You're missing some requirements; can't build"; fi; $(RM) status.sh)
else
checklib=$(shell $(ECHO) "Testing -l$(1)..."; $(ECHO) "program Check; {$$linklib} begin end." > check.pas" > check.pas; $(PC) $(PCFLAGS) check.pas)
checkprog=$($shell $(ECHO) "Checking for $(1)'s path; $(ECHO) $(call programpath,$(1)))
checkunit=$(shell $(ECHO) "Testing unit $(1)..."; $(ECHO) "program Check"; uses $(1); begin end." > check.pas" > check.pas; $(PC) $(PCFLAGS) check.pas)
success=$(shell $(ECHO) Consult the messages above to ascertain whether you can successfully compile)
endif

# OK, the actual start of the Makefile
VERSION = 2.1.1

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
PREFIX = build
endif # OS
endif # inUnix
endif # PREFIX

prefix = $(PREFIX)

bindir = $(prefix)/bin
datadir = $(prefix)/share
docdir = $(rdatadir)/docs
icondir = $(datadir)/icons
libdir = $(prefix)/lib
localedir = $(datadir)/locale
rdatadir = $(datadir)/r3r
skindir = $(rdatadir)/skins

uis = classic html tui tv wx

PCFLAGS += $(DEFS) $(PCFLAGS_BASE) $(PCFLAGS_DEBUG) $(PCFLAGS_EXTRA) \
	$(UNITDIRS)

# Defines, for enabling different features/dialect syntax
DEFS = $(foreach opt, $(DEFS_EXTRA) $(DEFS_SETTINGS) $(DEFS_SOCKETS), $(DEFFLAG)$(opt))

UNITDIRS=$(sort $(foreach d,$(wildcard $(addsuffix /*,$(UNIT_DIRS))),$(DIRFLAG)$(dir $(d))))
override COMPILER_OPTIONS += $(PCFLAGS)

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
endif # USE_GPC
endif # USE_FPC
else
PC=$(call programpath,fpc)
ifeq ($(findstring fpc,$(PC)),fpc)
USE_FPC=1
else
PC=$(call programpath,gp)
CC=$(call programpath,gpc)
ifeq ($(findstring gp,$(PC)),gp)
USE_GPC=1
else
$(warning No Pascal compiler detected)
endif # USE_GPC
endif # USE_FPC
endif # COMPILER_OVERRIDE

ifdef USE_FPC
override COMPILER=FPC $(shell $(PC) -iW)
PLATFORM=$(shell $(PC) -iTO)-$(shell $(PC) -iTP)

DEFFLAG=-d
PCFLAGS_BASE=-Mdelphi -Sh -WR -FE$(EXEOUT) -FU$(builddir) -Fu$(builddir)
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
ifndef USE_NCRT
override DEFS_EXTRA += HAS_SCREENHEIGHTWIDTH
else
override DEFS_EXTRA += USE_NCRT
endif # USE_NCRT
else
ifndef inWindows # as far as I can tell, only Windows' CRT unit doesn't have those constants
override DEFS_EXTRA += HAS_SCREENHEIGHTWIDTH
endif # inWindows
endif # inUnix
endif # R3R_UI

DEFS_SOCKETS ?= SOCKETS_SYNAPSE

ifdef inWindows
DEFS_SETTINGS ?= SETTINGS_REG
else
DEFS_SETTINGS ?= SETTINGS_TAB
endif # inWindows

ifdef DEBUG
R3R_UI ?= tui
else
R3R_UI ?= wx
endif # DEBUG

ifdef OS_TARGET
override PCFLAGS_BASE+=-T$(OS_TARGET)
endif

BUILD_SHARED ?= 1
export USE_FPC
else
ifdef USE_GPC
GPC=$(call programpath,gpc)
override COMPILER=GPC $(shell $(GPC) -dumpversion)
PLATFORM=$(shell $(GPC) -dumpmachine)

PCFLAGS_BASE=--extended-syntax --no-write-clip-strings \
						 --unit-destination-path=$(builddir)\
						 -DFree=Destroy -DPtrUInt=PtrWord \
						 -DNO_SUPPORTS_UNICODE $(LDFLAGS)
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
export USE_GPC
endif # USE_GPC
endif # USE_FPC

export CC DEFS DEFS_SOCKETS DESTDIR R3R_UI VERSION \
	bindir datadir prefix rootdir

dofault: all

_all:
	@$(MKDIR) $(builddir)

all: _all

_clean:
	$(DEL) $(wildcard *$(GPDEXT))
	$(DEL) $(wildcard *$(OEXT))
	$(DEL) $(wildcard *$(PPUEXT))
	$(DEL) $(wildcard *$(STATICLIBEXT))

cleanbuild:
	cd $(BUILDDIR) && make _clean

clean: _clean cleanbuild

distclean: clean

%$(PPUEXT): %.pas
	$(PC) $(PCFLAGS) $<
