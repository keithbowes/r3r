UNIT_DIRS = .. ../* ../../bindings/* ../../utils/*

include ../../../scripts/common.make

all: libr3rstrings$(PPUEXT)
ifneq ($(mo_files),)
	-$(MAKE) $(mo_files)
endif

install:
	-$(foreach l, $(LINGUAS), $(MKDIR) $(localedir)/$l; $(MKDIR) $(localedir)/$l/LC_MESSAGES; $(INSTALL) $l.mo $(localedir)/$l/LC_MESSAGES/libr3r.mo;)

uninstall:
	$(RM) $(wildcard $(localedir)/*/LC_MESSAGES/libr3r.mo)

%.mo: %.po libr3rstrings.pot
ifneq ($(USE_NLS),0)
	-$(MSGMERGE) -Uq $< libr3rstrings.pot
	-$(MSGFMT) $(MSGFMTFLAGS) -o $@ $<
else
	-$(TOUCH) $<
endif
