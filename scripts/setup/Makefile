SHELL = sh

ifeq ($(TYPE),debian)
	DEPS=libc6
else
	DEPS=glibc
endif

DEPS=libexpat1

ifeq ($(TYPE),debian)
	GTK_DEP=libgtk1.2
endif
ifeq ($(TYPE),rpm)
	GTK_DEP=libgtk+1.2
endif

ifeq ($(R3R_UI),classic)
	DEPS:="$(DEPS), $(GTK_DEP)"
endif

curdir = $(shell pwd)

all:
	@echo "You shouldn't run this Makefile directly"

dist-autopackage: r3r-$(VERSION).x86.package

dist-deb:
	$(MAKE) checkinstall TYPE=debian
	mv -f $(rootdir)/*.deb $(rootdir)/..

dist-rpm:
	$(MAKE) checkinstall TYPE=rpm VERSION="$(subst -,_,$(VERSION))"
	mv -f /usr/src/rpm/RPMS/i386/r3r-$(subst -,_,$(VERSION))-1.i386.rpm \
	 	$(rootdir)/..

dist-slackware:
	$(MAKE) checkinstall TYPE=slackware
	mv -f $(rootdir)/*.tgz $(rootdir)/..

dist-inno_setup:
	cd is && $(MAKE) installer

checkinstall:
	cd $(rootdir) && ln -fs $(curdir)/description-pak
	cd $(rootdir) && checkinstall --default --type=$(TYPE) \
		--install=no --pkgname=r3r --pkgversion="$(VERSION)" \
		--pkgarch=i386 --pkgrelease=1 --requires="$(DEPS)" \
		--pkglicense=BSD --nodoc --deldesc=yes --deldoc=yes \
		--delspec=yes --backup=no \
		--maintainer=zooplah@users.sourceforge.net

clean: distclean

distclean:
	rm -f *.package autopackage/default.apspec is/r3r.iss is/setup.exe
	rm -f r3r.xml* *.meta

r3r-$(VERSION).x86.package: autopackage/default.apspec
	makepackage
	mv -f r3r-$(VERSION).x86.package \
		../../..

autopackage/default.apspec: autopackage/_default.apspec
	sed \
	       	-e 's/@VERSION@/$(VERSION)/g' \
		-e 's/@UI@/$(R3R_UI)/g' \
      		autopackage/_default.apspec > \
		autopackage/default.apspec
