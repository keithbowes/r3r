UNIT_DIRS=.. ../* ../../bindings/* ../../utils/*

ifneq ($(DEFS_SOCKETS),SOCKETS_NONE)
REMOTE_PROTOCOLS=headers httpcache http
endif

UNITS = rsock localfile $(REMOTE_PROTOCOLS)
include ../../../scripts/common.make

all:
	cd .. && $(MAKE) info.pas
	$(MAKE) $(addsuffix $(PPUEXT), $(UNITS))

formatsdeps: httpcache$(PPUEXT) sockconsts$(PPUEXT)
