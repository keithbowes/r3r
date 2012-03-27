UNIT_DIRS=../* ../../libr3r ../../libr3r/* ../../bindings/* \
					../../utils/*

include ../../../scripts/common.make
all: libr3rstrings rdate$(PPUEXT)

libr3rstrings:
	cd ../../libr3r && $(MAKE) info.pas rupdate.pas
	cd ../../libr3r/po && $(MAKE)
