SHELL = sh

localedir = $(datarootdir)/locale

.SUFFIXES:
.SUFFIXES: $(OBJEXT) .mo .pas .po .pot .rst

all: $(subst .po,.mo, $(wildcard *.po))
	$(MAKE) classicstrings.rst
	$(MAKE) classicstrings.pot
	for lang in *.po; do $(MAKE) `echo $$lang | sed 's/\.po$$/\.mo/g'`; done

clean:
	rm -f *$(OBJEXT) *$(UNITEXT) *.rst

distclean:
	rm -f *.mo *.pot

install:
	-mkdir $(localedir)
	-$(foreach l, $(subst .po,, $(wildcard *.po)), mkdir $(localedir)/$l; mkdir $(localedir)/$l/LC_MESSAGES; $(INSTALL_DATA) $l.mo $(localedir)/$l/LC_MESSAGES/r3r_classic.mo)

uninstall:
	rm -f $(localedir)/*/LC_MESSAGES/r3r_classic.mo

.rst.pot:
	-rstconv -i $< -o $@

.po.mo:
	-msgmerge -Uq $< classicstrings.pot
	msgfmt -o $@ $<

.pas.rst:
	$(PC) $(PCFLAGS) $<
