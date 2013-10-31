include ../../../../scripts/common.make

all:
ifneq ($(mo_files),)
	-$(MAKE) $(mo_files)
endif

install:
	-$(foreach l, $(LINGUAS), $(MKDIR) $(localedir)/$l; $(MKDIR) $(localedir)/$l/LC_MESSAGES; $(INSTALL) $l.mo $(localedir)/$l/LC_MESSAGES/r3r_opml.mo;)

uninstall:
	$(RM) $(wildcard $(localedir)/*/LC_MESSAGES/r3r_opml.mo)

%.mo: %.po r3r_opml.pot
	-$(MSGMERGE) -Uq $^
	-$(MSGFMT) $(MSGFMTFLAGS) -o $@ $<
