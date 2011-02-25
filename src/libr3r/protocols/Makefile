UNIT_DIRS=.. ../* ../../third-party/* ../../utils/* ../../wrappers/*
UNITS = rsock localfile headers http

include ../../../scripts/common.make

all:
	cd .. && $(MAKE) info.pas
	$(MAKE) $(addsuffix $(PPUEXT), $(UNITS))

formatsdeps: httpcache$(PPUEXT) sockconsts$(PPUEXT)
