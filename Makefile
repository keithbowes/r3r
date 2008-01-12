SHELL = sh

# Extensions for compiling
EXEEXT =

PCFLAGS = $(DEFS) $(DEFS_EXTRA) $(PCFLAGS_BASE) $(PCFLAGS_DEBUG) \
	$(PCFLAGS_EXTRA)
# Defines, for enabling different features/dialect syntax
DEFS = -dSETTINGS_INI

INSTALL = install
INSTALLFLAGS = -c
INSTALL_DATA = $(INSTALL) $(INSTALLFLAGS) -m 644
INSTALL_PROGRAM = $(INSTALL) $(INSTALLFLAGS) -m 755

R3R_UI = tui
VERSION = $(shell cat .version)

prefix = /usr/local
exec_prefix = $(prefix)
datarootdir = $(prefix)/share

export DEFS INSTALL_DATA INSTALL_PROGRAM OBJEXT PCFLAGS \
	PCFLAGS_EXTRA R3R_UI VERSION \
	datarootdir exec_prefix prefix

default:
	@echo "Tisk, tisk!  You haven't read ./dok/english/BUILDING, have you?"

# Default building rule
all:
	cd icons && $(MAKE) all
	cd libr3r && $(MAKE) all
	cd ui && $(MAKE) all

# Installation rules
install:
	-mkdir $(prefix)
	-mkdir $(datarootdir)
	cd icons && $(MAKE)  install
	cd libr3r && $(MAKE) install
	cd ui && $(MAKE) install
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
	cd docs && $(MAKE) uninstall
	cd icons && $(MAKE) uninstall
	cd libr3r && $(MAKE) uninstall
	cd ui && $(MAKE) uninstall

uninstall-win32:
	$(MAKE) prefix="\"C:/Program Files/R3R\"" EXEEXT=.exe uninstall

# Documentation rules
docs html:
	cd docs && $(MAKE) all

install-docs:
	cd docs && $(MAKE) install

install-docs-win32:
	$(MAKE) prefix="\"C:/Program Files/R3R\"" install-docs

# Distribution rules
dist:
	pax -w r3r$(EXEEXT) icons/r3r.png libr3r/po/*.mo \
	ui/$(R3R_UI)/po/*.mo | gzip -f > r3r-$(VERSION).tar.gz

dist-src: maintainer-clean
	-mkdir ../r3r-src
	-cp -rf * ../r3r-src
# Paranoia: Remove all the files in the CVS dirs
	rm -rf `find ../r3r-src | grep CVS`
	mv ../r3r-src r3r-$(VERSION)
	pax -w r3r-$(VERSION) | gzip -cf > r3r-$(VERSION).tar.gz 
	rm r3r-$(VERSION)
	mv r3r-$(VERSION).tar.gz ..

dist-linux:
	cd scripts/setup && $(MAKE) dist-linux

dist-win32:
	cd scripts/setup && $(MAKE) dist-win32

# Cleaning rules
mostlyclean:
	rm -f r3r$(EXEEXT)

cleanall: mostlyclean
	cd libr3r && $(MAKE) clean
	cd ui && $(MAKE) clean

distclean: clean
	cd docs && $(MAKE) distclean
	cd icons && $(MAKE) distclean
	cd libr3r && $(MAKE) distclean
	cd scripts/setup && $(MAKE) distclean
	cd ui && $(MAKE) distclean
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
	$(MAKE) PC="fpc -T$(OS)" all

fpc-win32:
	$(MAKE) PCFLAGS_EXTRA="$(PCFLAGS_EXTRA) -Twin32" \
		DEFS="-dSETTINGS_REGISTRY" fpc

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

# Deprecated rules

# tar is obsolete
dist-src-dep: maintainer-clean
	-mkdir ../r3r-src
	cp * ../r3r-src
	mv ../r3r-src r3r-$(VERSION)
	tar --exclude=.* --exclude=CVS -czf r3r-$(VERSION).tar.gz \
		r3r-$(VERSION)
	rm r3r-$(VERSION)
	mv r3r-$(VERSION).tar.gz ..

# ditto
dist-old:
	tar -cf - r3r$(EXEEXT) icons/r3r.png libr3r/po/*.mo \
	ui/$(R3R_UI)/po/*.mo | gzip -f > r3r-$(VERSION).tar.gz

# Virtual Pascal has been discontinued.
# But if anyone wants to make compatiblity units, then I'd be happy to accept
# the possibility to add the proper command-line commands here.
virtual-pascal vpc:
	@echo "Compiling with Virtual Pascal is unsupported.  If that concerns" \
	"you, send patches."
