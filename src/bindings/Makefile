include ../../scripts/common.make

all:
ifneq ($(USE_EXPAT),0)
	cd expas && $(LN) ../../../scripts/common.make \
		&& $(MAKE) unit
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
ifeq ($(DEFS_SETTINGS),SETTINGS_LIBINI)
	cd libini && $(MAKE) all
endif
ifeq ($(DEFS_SOCKETS),SOCKETS_LIBCURL)
	cd libcurl && $(MAKE) all
endif
ifneq ($(USE_PCRE),0)
	cd pcre && $(MAKE) all
endif
ifneq ($(USE_READLINE),0)
	cd readline && $(MAKE) all
endif

clean:
	cd expas && $(MAKE) clean
	cd gettext && $(MAKE) clean
	cd iconv && $(MAKE) clean
	cd libcurl && $(MAKE) clean
	cd libidn && $(MAKE) clean
	cd libini && $(MAKE) clean
	cd pcre && $(MAKE) clean
	cd readline && $(MAKE) clean
