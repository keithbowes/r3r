.POSIX: 

SHELL = sh

# Extensions for compiling
EXEEXT=

PCFLAGS = $(DEFS) $(DEFS_EXTRA) $(PCFLAGS_BASE) $(PCFLAGS_DEBUG) $(PCFLAGS_EXTRA)
# Defines, for enabling different features/dialect syntax
DEFS = -dSETTINGS_INI

INSTALL = install
INSTALLFLAGS = -c
INSTALL_DATA = $(INSTALL) $(INSTALLFLAGS) -m 644
INSTALL_PROGRAM = $(INSTALL) $(INSTALLFLAGS) -m 755

R3R_UI = tui
VERSION = `cat .version`

prefix = /usr/local
exec_prefix = $(prefix)
datarootdir = $(prefix)/share

default:
	@echo "Tisk, tisk!  You haven't read ./dok/english/BUILDING, have you?"

# Default building rule
all:
	if test ! -e .$(VERSION).built; then touch .$(VERSION).built; fi
	cd icons && $(MAKE) all
	cd libr3r && $(MAKE) PCFLAGS="$(PCFLAGS)" DEFS="$(DEFS)" OBJEXT="$(OBJEXT)" \
		 VERSION="`cat ../.version`" all
	cd $(R3R_UI) && $(MAKE) PCFLAGS="$(PCFLAGS)" all

# Installation rules
install:
	-mkdir $(prefix)
	-mkdir $(datarootdir)
	cd $(R3R_UI) && $(MAKE) datarootdir="$(datarootdir)" \
		exec_prefix="$(exec_prefix)" INSTALL_DATA="$(INSTALL_DATA)" \
		INSTALL_PROGRAM="$(INSTALL_PROGRAM)" install
	cd icons && $(MAKE) datarootdir="$(datarootdir)" \
		INSTALL_DATA="$(INSTALL_DATA)" install
	cd libr3r && $(MAKE) datarootdir="$(datarootdir)" \
		INSTALL_DATA="$(INSTALL_DATA)" install
	@echo Type $(MAKE) install-docs if you want to install the documentation.

install-strip: install
	-strip $(bindir)/r3r$(EXEEXT)
	-upx $(bindir)/r3r$(EXEEXT)

install-win32:
	$(MAKE) prefix="\"C:/Program Files/R3R\"" EXEEXT=.exe install

install-win32-strip:
	$(MAKE) prefix="\"C:/Program Files/R3R\"" EXEEXT=.exe install-strip

# Uninstallation rules
uninstall:
	cd $(R3R_UI) && $(MAKE) datarootdir="$(datarootdir)" prefix="$(prefix)" \
		uninstall
	cd docs && $(MAKE) datarootdir="$(datarootdir)" uninstall
	cd icons && $(MAKE) datarootdir="$(datarootdir)" uninstall
	cd libr3r && $(MAKE) datarootdir="$(datarootdir)" uninstall

uninstall-win32:
	$(MAKE) prefix="\"C:/Program Files/R3R\"" EXEEXT=.exe uninstall

# Documentation rules
docs html:
	cd docs && $(MAKE) datarootdir="$(datarootdir)" all

install-docs:
	cd docs && $(MAKE) datarootdir="$(datarootdir)" \
		INSTALL_DATA="$(INSTALL_DATA)" install

install-docs-win32:
	$(MAKE) prefix="\"C:/Program Files/R3R\"" install-docs

# Distribution rules
dist:
	tar cf - r3r$(EXEEXT) icons/r3r.png libr3r/po/*.mo $(R3R_UI)/po/*.mo | gzip \
		-f > r3r-$(VERSION).tar.gz

dist-src: maintainer-clean
	-mkdir ../r3r-src
	cp * ../r3r-src
	mv ../r3r-src r3r-$(VERSION)
	tar --exclude=.* --exclude=CVS -czf r3r-$(VERSION).tar.gz \
		r3r-$(VERSION)
	rm r3r-$(VERSION)
	mv r3r-$(VERSION).tar.gz ..

dist-linux:
	cd scripts/setup && $(MAKE) VERSION="`cat ../../.version`" \
	R3R_UI="$(R3R_UI)" dist-linux

dist-win32:
	cd scripts/setup && $(MAKE) VERSION="`cat ../../.version`" \
	R3R_UI="$(R3R_UI)" dist-win32

# Cleaning rules
mostlyclean:
	rm -f r3r$(EXEEXT)

cleanall: mostlyclean
	cd $(R3R_UI) && $(MAKE) clean
	cd libr3r && $(MAKE) clean

distclean: clean
	cd $(R3R_UI) && $(MAKE) distclean
	cd docs && $(MAKE) distclean
	cd icons && $(MAKE) distclean
	cd libr3r && $(MAKE) distclean
	cd scripts/setup && $(MAKE) distclean
	-delp -eq .
	rm -f .*.built

maintainer-clean: distclean

clean-win32:
	$(MAKE) EXEEXT=.exe distclean

clean:
	-$(MAKE) OBJEXT=.o UNITEXT=.ppu cleanall
	-$(MAKE) OBJEXT=.o UNITEXT=.gpi cleanall
	-$(MAKE) OBJEXT=.obj UNITEXT=.dcu cleanall

# Compilers
fpc:
	$(MAKE) PCFLAGS_BASE="-S2 -Sh -FU." PCFLAGS_DEBUG="-Xs- -gh -gl -CX- -XX-" \
		OBJEXT=.o PC_OUTDIR=-FE UNITDIR=-Fu OS=`fpc -iTO` fpc-cross

fpc-cross:
	$(MAKE) PC=fpc PCFLAGS_EXTRA="$(PCFLAGS_EXTRA) -T$(OS)" all

fpc-win32:
	$(MAKE) fpc PCFLAGS_EXTRA="$(PCFLAGS_EXTRA) -Twin32" \
		DEFS="-dSETTINGS_REGISTRY"

gpc:
	-$(MAKE) PC=gpc PCFLAGS_BASE="--automake -B po -B libr3r -o r3r" UNITEXT=.gpi
	@echo "Unless you really know what you're doing, the build has failed." \
	"Hopefully, it will work in the future."

for-borland-compilers:
	-$(MAKE) \
	PCFLAGS_BASE="-B -CC -E. -M -O$(R3R_UI) -Upo -Ulibr3r -V" DEFS=-DNO_GETTEXT \
				 OBJEXT=.obj UNITEXT=.dcu
	@echo "Compiling with Delphi/Kylix is currently unsupported.  If that" \
	"concerns you, send patches."

delphi:
	$(MAKE) PC=dcc32 for-borland-compilers

kylix:
	$(MAKE) PC=dcc for-borland-compilers

virtual-pascal vpc:
	@echo "Compiling with Virtual Pascal is unsupported.  If that concerns" \
	"you, send patches."
