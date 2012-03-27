UNIT_DIRS = ../../bindings/*

include ../../../scripts/common.make

all: r3r_opml$(PPUEXT)
	cd po && $(MAKE) all

clean:
	cd po && $(MAKE) clean
	$(DEL) $(EXEOUT)/r3r_opml$(TARGETEXEEXT)

install:
	cd po && $(MAKE) install
	$(INSTALLEXE) $(EXEOUT)/r3r_opml$(TARGETEXEEXT) $(bindir)

uninstall:
	cd po && $(MAKE) uninstall
	$(DEL) $(bindir)/r3r_opml$(TARGETEXEEXT)
