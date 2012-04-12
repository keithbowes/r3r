include ../../../../scripts/common.make

all: r3r_conv.pot $(mo_files)

clean: _clean
	$(DEL) r3r_conv.pot

install:
	-$(foreach l, $(LINGUAS), $(MKDIR) $(localedir)/$l; $(MKDIR) $(localedir)/$l/LC_MESSAGES; $(INSTALL) $l.mo $(localedir)/$l/LC_MESSAGES/r3r_conv.mo;)

uninstall:
	$(DEL) $(wildcard $(localedir)/*/LC_MESSAGES/r3r_conv.mo)

r3r_conv.pot: ../r3r-conv.c
	-$(XGETTEXT) -k_ -o $@ $^

%.mo: %.po
	-$(MSGFMT) $(MSGFMTFLAGS) -o $@ $^

%.po: r3r_conv.pot
	-$(MSGMERGE) -Uq $@ $<
