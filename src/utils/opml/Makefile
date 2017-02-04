UNIT_DIRS = ../../bindings/*

include ../../../scripts/common.make

all: r3r_opml$(PPUEXT)

clean:
	$(RM) $(wildcard $(EXEOUT)/r3r_opml$(TARGETEXEEXT))

install:
	$(INSTALLEXE) $(EXEOUT)/r3r_opml$(TARGETEXEEXT) $(bindir)

uninstall:
	$(RM) $(wildcard $(bindir)/r3r_opml$(TARGETEXEEXT))
