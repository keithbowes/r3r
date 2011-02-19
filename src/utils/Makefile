all:
	cd opml && $(MAKE) all
	cd rlist && $(MAKE) all
	cd rstrings && $(MAKE) all
	cd strtok && $(MAKE) all
	cd rdate && $(MAKE) all
	cd rprop && $(MAKE) all
ifeq ($(DEFS_SETTINGS),SETTINGS_TAB)
	cd tabfiles && $(MAKE) all
endif

clean:
	cd opml && $(MAKE) clean
	cd rdate && $(MAKE) clean
	cd rlist && $(MAKE) clean
	cd rprop && $(MAKE) clean
	cd rstrings && $(MAKE) clean
	cd strtok && $(MAKE) clean
	cd tabfiles && $(MAKE) clean

install:
	cd opml && $(MAKE) install
