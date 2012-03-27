all:
ifeq ($(DEFS_SOCKETS), SOCKETS_BSD)
	cd wrappers/sockwrap && $(MAKE) wrapper
endif
	cd bindings && $(MAKE)
	cd utils && $(MAKE)
	cd wrappers && $(MAKE)
	cd libr3r && $(MAKE)
	cd ui && $(MAKE)

clean:
	cd bindings && $(MAKE) clean
	cd utils && $(MAKE) clean
	cd libr3r && $(MAKE) clean
	cd ui && $(MAKE) clean
	cd wrappers && $(MAKE) clean

install:
	cd libr3r && $(MAKE) install
	cd ui && $(MAKE) install
	cd utils && $(MAKE) install

uninstall:
	cd libr3r && $(MAKE) uninstall
	cd ui && $(MAKE) uninstall
	cd utils && $(MAKE) uninstall
