.POSIX: 

localedir = $(datarootdir)/locale

.SUFFIXES:
.SUFFIXES: $(OBJEXT) .mo .pas .po .pot .rst

all:
	$(MAKE) classicstrings.rst
	$(MAKE) classicstrings.pot
	for lang in *.po; do $(MAKE) `echo $$lang | sed 's/\.po$$/\.mo/g'`; done

clean:
	rm -f *$(OBJEXT) *$(UNITEXT) *.rst

distclean:
	rm -f *.mo *.pot

install:
	-mkdir $(localedir)
	-for lang in *.mo; do l=`echo $$lang | sed 's/\.mo$$//g'`; \
		mkdir $(localedir)/$$l; mkdir $(localedir)/$$l/LC_MESSAGES; \
		$(INSTALL_DATA) $$lang $(localedir)/$$l/LC_MESSAGES/r3r_classic.mo; done

uninstall:
	rm -f $(localedir)/*/LC_MESSAGES/r3r_classic.mo

.rst.pot:
	-rstconv -i $< -o $@

.po.mo:
	-msgmerge -Uq $< classicstrings.pot
	msgfmt -o $@ $<

.pas.rst:
	$(PC) $(PCFLAGS) $<
