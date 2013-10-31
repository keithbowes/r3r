include ../../../../scripts/common.make

UNIT_DIRS = ../../* ../../../bindings/* ../../../libr3r ../../../libr3r/*

all: r3r_conv.pot convstrings$(PPUEXT) $(mo_files)

install:
	-$(foreach l, $(LINGUAS), $(MKDIR) $(localedir)/$l; $(MKDIR) $(localedir)/$l/LC_MESSAGES; $(INSTALL) $l.mo $(localedir)/$l/LC_MESSAGES/r3r_conv.mo;)

uninstall:
	$(RM) $(wildcard $(localedir)/*/LC_MESSAGES/r3r_conv.mo)

r3r_conv.pot: convstrings.pas
	-$(XGETTEXT) -k_ -o $@ $^

%.mo: %.po
	-$(MSGFMT) $(MSGFMTFLAGS) -o $@ $^

%.po: r3r_conv.pot
	-$(MSGMERGE) -Uq $@ $<
	-$(TOUCH) $@ $^
