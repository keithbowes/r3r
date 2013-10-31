include ../../../scripts/common.make

UNIT_DIRS = po ../*  ../../bindings/* ../../libr3r ../../libr3r/*

all: r3rconv$(PPUEXT)
ifneq ($(USE_NLS),0)
	cd po && $(MAKE) all
endif
	$(MV) r3rconv$(TARGETEXEEXT) r3r-conv$(TARGETEXEEXT)

r3rconv$(PPUEXT): atomwriter$(PPUEXT) conv$(PPUEXT) esfwriter$(PPUEXT) rss3writer$(PPUEXT) rsswriter$(PPUEXT)

install:
	$(INSTALLEXE) r3r-conv$(TARGETEXEEXT) $(bindir)
	cd po && $(MAKE) install

uninstall:
	$(RM) $(wildcard $(bindir)/r3r-conv$(TARGETEXEEXT))
	cd po && $(MAKE) uninstall

clean: _clean
	$(RM) $(wildcard r3r-conv$(TARGETEXEEXT))
	cd po && $(MAKE) clean
