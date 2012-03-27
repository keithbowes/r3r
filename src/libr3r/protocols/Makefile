UNIT_DIRS=.. ../* ../../bindings/* ../../utils/* ../../wrappers/*

ifneq ($(DEFS_SOCKETS),SOCKETS_NONE)
ifneq ($(DEFS_SOCKETS),SOCKETS_CURL)
REMOTE_PROTOCOLS=headers http
endif
endif

UNITS = rsock localfile $(REMOTE_PROTOCOLS)

include ../../../scripts/common.make

all:
	cd .. && $(MAKE) info.pas
	$(MAKE) $(addsuffix $(PPUEXT), $(UNITS))

formatsdeps: httpcache$(PPUEXT) sockconsts$(PPUEXT)
