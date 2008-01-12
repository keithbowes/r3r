SHELL = sh

localedir = $(datarootdir)/locale

.SUFFIXES:
.SUFFIXES: $(OBJEXT) .mo .pas .po .pot .rst

all: $(subst .po,.mo, $(wildcard *.po))
	$(MAKE) libr3rstrings.rst
	$(MAKE) libr3rstrings.pot

clean:
	rm -f *$(OBJEXT) *$(UNITEXT) *.rst

distclean:
	rm -f *.mo *.pot

install:
	-mkdir $(localedir)
	-$(foreach l, $(subst .po,, $(wildcard *.po)), mkdir $(localedir)/$l; mkdir $(localedir)/$l/LC_MESSAGES; $(INSTALL_DATA) $l.mo $(localedir)/$l/LC_MESSAGES/libr3r.mo)

uninstall:
	rm -f $(localedir)/*/LC_MESSAGES/libr3r.mo

.rst.pot:
	-rstconv -i $< -o $@

.po.mo:
	-msgmerge -Uq $< libr3rstrings.pot
	msgfmt -o $@ $<

.pas.rst:
	$(PC) $(PCFLAGS) $<
