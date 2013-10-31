UNIT_DIRS = ../../bindings/*

include ../../../scripts/common.make

all: r3r_opml$(PPUEXT)
	cd po && $(MAKE) all

clean:
	cd po && $(MAKE) clean
	$(RM) $(wildcard $(EXEOUT)/r3r_opml$(TARGETEXEEXT))

install:
	cd po && $(MAKE) install
	$(INSTALLEXE) $(EXEOUT)/r3r_opml$(TARGETEXEEXT) $(bindir)

uninstall:
	cd po && $(MAKE) uninstall
	$(RM) $(wildcard $(bindir)/r3r_opml$(TARGETEXEEXT))
