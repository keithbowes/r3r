.POSIX: 

dist-linux: r3r-$(VERSION).x86.package

dist-win32:
	cd is && $(MAKE) installer

distclean:
	rm -f *.package autopackage/default.apspec is/r3r.iss is/setup.exe

r3r-$(VERSION).x86.package: autopackage/default.apspec
	makeinstaller
	mv r3r-$(VERSION).x86.package ../../..

autopackage/default.apspec: autopackage/_default.apspec
	sed 's/@VERSION@/$(VERSION)/g'  autopackage/_default.apspec > \
		autopackage/default.apspec.1
	sed 's/@UI@/$(R3R_UI)/g' autopackage/default.apspec.1 > \
		autopackage/default.apspec
	rm -f autopackage/default.apspec.1
