include ../../scripts/common.make

all:
ifneq ($(USE_EXPAT),0)
	cd expas && $(MAKE) unit
endif
ifneq ($(USE_NLS),0)
	cd gettext && $(MAKE) all
endif
ifneq ($(USE_ICONV),0)
	cd iconv && $(MAKE) unit
endif
ifneq ($(USE_IDN),0)
	cd libidn && $(MAKE) all
endif

clean:
	cd expas && $(MAKE) clean
	cd gettext && $(MAKE) clean
	cd iconv && $(MAKE) clean
	cd libidn && $(MAKE) clean
