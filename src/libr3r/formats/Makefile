UNIT_DIRS=.. ../* ../../third-party/* ../../utils/* ../../wrappers/*
UNITS=feeditem feed nonxml esf rss3 saxcallbacks xml dc \
			mod_enclosure atom rss

include ../../../scripts/common.make

all:
	$(MAKE) deps
	$(MAKE) $(addsuffix $(PPUEXT), $(UNITS))

install:

deps:
	cd ../protocols && $(MAKE) formatsdeps
