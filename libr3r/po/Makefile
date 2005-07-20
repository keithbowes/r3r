.POSIX: 

localedir = $(datarootdir)/locale

.SUFFIXES:
.SUFFIXES: $(OBJEXT) .mo .pas .po .pot .rst

all:
	$(MAKE) libr3rstrings.rst
	$(MAKE) libr3rstrings.pot
	for lang in *.po; do $(MAKE) `echo $$lang | sed 's/\.po$$/\.mo/g'`; done

clean:
	rm -f *$(OBJEXT) *$(UNITEXT) *.rst

distclean:
	rm -f *.mo *.pot

install:
	-mkdir $(localedir)
	-for lang in *.mo; do l=`echo $$lang | sed 's/\.mo$$//g'`; \
		mkdir $(localedir)/$$l; mkdir $(localedir)/$$l/LC_MESSAGES; \
		$(INSTALL_DATA) $$lang $(localedir)/$$l/LC_MESSAGES/libr3r.mo; done

uninstall:
	rm -f $(localedir)/*/LC_MESSAGES/libr3r.mo

.rst.pot:
	-rstconv -i $< -o $@

.po.mo:
	-msgmerge -Uq $< libr3rstrings.pot
	msgfmt -o $@ $<

.pas.rst:
	$(PC) $(PCFLAGS) $<
