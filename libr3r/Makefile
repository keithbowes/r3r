.POSIX: 

.SUFFIXES:
.SUFFIXES: $(OBJEXT) .pas

all:
	$(MAKE) info.pas
	cd po && $(MAKE) all
	$(MAKE) rmessage$(OBJEXT)
	cd regexpr && $(MAKE) all
	cd formats && $(MAKE) all
	cd protocols && $(MAKE) all
	cd settings && $(MAKE) all
	$(MAKE) libr3r$(OBJEXT)

clean:
	cd formats && $(MAKE) clean
	cd po && $(MAKE) clean
	cd protocols && $(MAKE) clean
	cd regexpr && $(MAKE) clean
	cd settings && $(MAKE) clean
	rm -f *$(OBJEXT) *$(UNITEXT)

distclean:
	cd po && $(MAKE) distclean
	rm -f info.pas

install:
	cd po && $(MAKE) install

uninstall:
	cd po && $(MAKE) uninstall

.pas$(OBJEXT):
	$(PC) $(PCFLAGS) $(UNITDIR)formats $(UNITDIR)po $(UNITDIR)protocols \
		$(UNITDIR)regexpr $(UNITDIR)settings $<

info.pas: _info.pas ../.version
	sed 's/@VERSION@/$(VERSION)/g' _info.pas > info.pas
