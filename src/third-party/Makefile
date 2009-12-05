all:
	cd gettext && $(MAKE) all
ifeq ($(DEFS_SAX),SAX_LIBXML2)
	cd libxml2 && $(MAKE)
else
	cd expas && $(MAKE) unit
endif

clean:
	cd expas && $(MAKE) clean
	cd gettext && $(MAKE) clean
	cd libxml2 && $(MAKE) clean
