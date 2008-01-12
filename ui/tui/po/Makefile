SHELL = sh

localedir = $(datarootdir)/locale

.SUFFIXES:
.SUFFIXES: $(OBJEXT) .mo .pas .po .pot .rst

all: $(subst .po,.mo, $(wildcard *.po))
	$(MAKE) tuistrings.rst
	$(MAKE) tuistrings.pot

clean:
	rm -f *$(OBJEXT) *$(UNITEXT) *.rst

distclean:
	rm -f *.mo *.pot

install:
	-mkdir $(localedir)
	-$(foreach l, $(subst .po,, $(wildcard *.po)), mkdir $(localedir)/$l; mkdir $(localedir)/$l/LC_MESSAGES; $(INSTALL_DATA) $l.mo $(localedir)/$l/LC_MESSAGES/r3r_tui.mo)

uninstall:
	rm -f $(localedir)/*/LC_MESSAGES/r3r_tui.mo

.rst.pot:
	-rstconv -i $< -o $@

.po.mo:
	-msgmerge -Uq $< tuistrings.pot
	msgfmt -o $@ $<

.pas.rst:
	$(PC) $(PCFLAGS) $<
