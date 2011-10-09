.SUFFIXES:
.SUFFIXES: $(OEXT) .mo .pas .po .pot

UNIT_DIRS = .. ../* ../../third-party/* ../../utils/*

po_files = $(wildcard *.po)
LINGUAS ?= $(subst .po,,$(po_files))
mo_files = $(addsuffix .mo,$(LINGUAS))

include ../../../scripts/common.make

all: libr3rstrings$(PPUEXT)
ifneq ($(mo_files),)
	-$(MAKE) $(mo_files)
endif

clean:
	$(DEL) $(wildcard *.mo) $(wildcard *.po~)

install:
	-$(foreach l, $(LINGUAS), $(MKDIR) $(localedir)/$l; $(MKDIR) $(localedir)/$l/LC_MESSAGES; $(INSTALL) $l.mo $(localedir)/$l/LC_MESSAGES/libr3r.mo;)

uninstall:
	$(DEL) $(wildcard $(localedir)/*/LC_MESSAGES/libr3r.mo)

%.mo: %.po libr3rstrings.pot
ifneq ($(USE_NLS),0)
	-$(MSGMERGE) -Uq $< libr3rstrings.pot
	-$(MSGFMT) $(MSGFMTFLAGS) -o $@ $<
else
	-$(TOUCH) $<
endif

test:
	@$(ECHO) x$(LINGUAS)x
