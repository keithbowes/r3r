.POSIX: 

bindir=$(exec_prefix)/bin

.SUFFIXES:
.SUFFIXES: $(OBJEXT) .pas

all:
	touch r3r.pas
	cd po && $(MAKE) all
	$(MAKE) tui$(OBJEXT) r3r$(OBJEXT)

clean:
	cd po && $(MAKE) clean
	rm -f *$(OBJEXT) *$(UNITEXT)

distclean:
	cd po && $(MAKE) distclean

install:
	cd po && $(MAKE) install
	-mkdir $(bindir)
	$(INSTALL_PROGRAM) ../r3r$(EXEEXT) $(bindir)/r3r$(EXEEXT)

uninstall:
	cd po && $(MAKE) uninstall
	rm -f $(bindir)/r3r$(EXEEXT)

.pas$(OBJEXT):
	$(PC) $(PC_OUTDIR)../.. $(UNITDIR)po $(UNITDIR)../../libr3r $(UNITDIR)../../libr3r/* \
		$(PCFLAGS) $<
