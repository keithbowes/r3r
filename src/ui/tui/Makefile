EXEOUT=.
UNIT_DIRS=po ../../libr3r ../../libr3r/* ../../bindings/* \
					../../utils/*

include ../../../scripts/common.make
all:
ifneq ($(USE_NLS),0)
	cd po && $(MAKE)
endif
	cd ../../libr3r && $(MAKE) info.pas
	$(MAKE) rkeys$(PPUEXT) rtitle$(PPUEXT) skin$(PPUEXT) \
		tuifuncs$(PPUEXT) tui$(PPUEXT) r3r$(PPUEXT)

clean:
	cd po && $(MAKE) clean
	$(DEL) $(wildcard *.or) $(wildcard *.res)

install:
	cd docs && $(MAKE) install
	cd po && $(MAKE) install
	cd skins && $(MAKE) install

uninstall:
	cd docs && $(MAKE) uninstall
	cd po && $(MAKE) uninstall
	cd skins && $(MAKE) uninstall
