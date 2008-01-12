SHELL = sh

dist-linux: r3r-$(VERSION).x86.package

dist-win32:
	cd is && $(MAKE) installer

distclean:
	rm -f *.package autopackage/default.apspec is/r3r.iss is/setup.exe

r3r-$(VERSION).x86.package: autopackage/default.apspec
	makepackage
	cp -f r3r-$(VERSION).x86.package ../../..

autopackage/default.apspec: autopackage/_default.apspec
	sed \
	       	-e 's/@VERSION@/$(VERSION)/g' \
		-e 's/@UI@/$(R3R_UI)/g' \
      		autopackage/_default.apspec > \
		autopackage/default.apspec
