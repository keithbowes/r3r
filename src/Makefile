all:
	cd bindings && $(MAKE)
	cd utils && $(MAKE)
	cd libr3r && $(MAKE)
	cd ui && $(MAKE)

clean:
	cd bindings && $(MAKE) clean
	cd utils && $(MAKE) clean
	cd libr3r && $(MAKE) clean
	cd ui && $(MAKE) clean

install:
	cd libr3r && $(MAKE) install
	cd ui && $(MAKE) install
	cd utils && $(MAKE) install

install-header:
	cd libr3r && $(MAKE) install-header

install-lib:
	cd libr3r && $(MAKE) install-lib

uninstall:
	cd libr3r && $(MAKE) uninstall
	cd ui && $(MAKE) uninstall
	cd utils && $(MAKE) uninstall
