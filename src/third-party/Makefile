all:
ifeq ($(DEFS_SAX),SAX_EXPAT)
	cd expas && $(MAKE) unit
endif
ifeq ($(DEFS_SAX),SAX_LIBXML2)
	cd libxml2 && $(MAKE)
endif

clean:
	cd expas && $(MAKE) clean
	cd libxml2 && $(MAKE) clean
