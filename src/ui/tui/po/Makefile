UNIT_DIRS = ../../../libr3r ../../../libr3r/* \
						../../../bindings/* ../../../utils/*

include ../../../../scripts/common.make

all: tuistrings$(PPUEXT)
ifneq ($(mo_files),)
	-$(MAKE) $(mo_files)
endif

install:
	-$(foreach l, $(LINGUAS), $(MKDIR) $(localedir)/$l; $(MKDIR) $(localedir)/$l/LC_MESSAGES; $(INSTALL) $l.mo $(localedir)/$l/LC_MESSAGES/r3r_tui.mo;)

uninstall:
	$(DEL) $(wildcard $(localedir)/*/LC_MESSAGES/r3r_tui.mo)

%.mo: %.po tuistrings.pot
	-$(MSGMERGE) -Uq $< tuistrings.pot
	-$(MSGFMT) $(MSGFMTFLAGS) -o $@ $<
