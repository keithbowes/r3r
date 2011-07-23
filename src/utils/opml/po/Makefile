.SUFFIXES:
.SUFFIXES: .mo .po .pot

po_files = $(wildcard *.po)
LINGUAS ?= $(subst .po,,$(po_files))
mo_files= $(addsuffix .mo, $(LINGUAS)) 

include ../../../../scripts/common.make

all:
	-$(MAKE) $(mo_files)

clean:
	$(DEL) $(wildcard *.mo) $(wildcard *.po~)

install:
	-$(foreach l, $(LINGUAS), $(MKDIR) $(localedir)/$l; $(MKDIR) $(localedir)/$l/LC_MESSAGES; $(INSTALL) $l.mo $(localedir)/$l/LC_MESSAGES/r3r_opml.mo;)

uninstall:
	$(DEL) $(wildcard $(localedir)/*/LC_MESSAGES/r3r_opml.mo)

%.mo: %.po r3r_opml.pot
	-$(MSGMERGE) -Uq $^
	-$(MSGFMT) $(MSGFMTFLAGS) -o $@ $<
