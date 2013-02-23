include ../../scripts/common.make

all:
	cd opml && $(MAKE) all
	cd rlist && $(MAKE) all
	cd rstrings && $(MAKE) all
	cd strtok && $(MAKE) all
	cd rdate && $(MAKE) all
	cd rprop && $(MAKE) all
	cd mailcap && $(MAKE) unit

clean:
	cd conv && $(MAKE) clean
	cd mailcap && $(MAKE) clean
	cd opml && $(MAKE) clean
	cd rdate && $(MAKE) clean
	cd rlist && $(MAKE) clean
	cd rprop && $(MAKE) clean
	cd rstrings && $(MAKE) clean
	cd strtok && $(MAKE) clean

install:
	cd conv && $(MAKE) install
	cd opml && $(MAKE) install

uninstall:
	cd conv && $(MAKE) uninstall
	cd opml && $(MAKE) uninstall

r3r-conv:
	cd conv && $(MAKE) all
