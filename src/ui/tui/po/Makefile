.SUFFIXES:
.SUFFIXES: $(OEXT) .mo .pas .po .pot

UNIT_DIRS = ../../../libr3r ../../../libr3r/* \
						../../../third-party/* ../../../utils/* \
						../../../wrappers/*

po_files = $(wildcard *.po)
LINGUAS ?= $(subst .po,,$(po_files))
mo_files= $(addsuffix .mo, $(LINGUAS)) 

include ../../../../scripts/common.make

all: $(mo_files) tuistrings$(PPUEXT)

clean:
	$(DEL) $(wildcard *.mo) $(wildcard *.po~)

install:
	-$(foreach l, $(LINGUAS), $(MKDIR) $(localedir)/$l; $(MKDIR) $(localedir)/$l/LC_MESSAGES; $(INSTALL) $l.mo $(localedir)/$l/LC_MESSAGES/r3r_tui.mo;)

uninstall:
	$(DEL) $(wildcard $(localedir)/*/LC_MESSAGES/r3r_tui.mo)

%.mo: %.po tuistrings.pot
	-$(MSGMERGE) -Uq $< tuistrings.pot
	-$(MSGFMT) $(MSGFMTFLAGS) -o $@ $<
