all:
	cd third-party && $(MAKE)
	cd utils && $(MAKE)
	cd libr3r && $(MAKE)
	cd ui && $(MAKE)

clean:
	cd third-party && $(MAKE) clean
	cd utils && $(MAKE) clean
	cd libr3r && $(MAKE) clean
	cd ui && $(MAKE) clean

install:
	cd libr3r && $(MAKE) install
	cd ui && $(MAKE) install

uninstall:
	cd libr3r && $(MAKE) uninstall
	cd ui && $(MAKE) uninstall
