ifneq ($(USE_EXPAT),0)
SAXCALLBACKS=saxcallbacks
endif

UNIT_DIRS=.. ../* ../../bindings/* ../../utils/*
UNITS=feeditem feed itemcallbacks nonxml esf rss3 $(SAXCALLBACKS) \
			xml dc mod_enclosure atom rss

include ../../../scripts/common.make

all:
	$(MAKE) deps
	$(MAKE) $(addsuffix $(PPUEXT), $(UNITS))

install:

deps:
	cd ../protocols && $(MAKE) formatsdeps
