include ../../common.make
ISCC ?= $(call programpath,iscc)
ISCCFLAGS ?=

ifeq ($(VERSION),)
$(error VERSION should be set.  Try running from the root dir)
endif

all installer: uptmpl setup.exe

setup.exe: r3r.iss
	$(ISCC) $(ISCCFLAGS) $<
	$(MV) $@  $(srcdir)/../../../../r3r_$(R3R_UI)-$(VERSION)-$@

r3r.iss: _r3r.iss
	$(SED) -e 's/@VERSION@/$(VERSION)/g' \
		-e 's/@UI@/$(R3R_UI)/g' \
		-e 's/ts=0/ts=$(shell date +%s)/' $< > $@

uptmpl:
	$(TOUCH) _r3r.iss
