include ../../common.make
ISCC ?= $(call programpath,iscc)
ISCCFLAGS ?=

ifeq ($(VERSION),)
$(error VERSION should be set.  Try running from the root dir)
endif

all installer: setup.exe

setup.exe: r3r.iss
	$(ISCC) $<
	$(MOVE) $@  $(srcdir)/../../../../r3r_$(R3R_UI)-$(VERSION)-$@

r3r.iss: _r3r.iss
	$(SED) -e 's/@VERSION@/$(VERSION)/g' \
		-e 's/@UI@/$(R3R_UI)/g' $< > $@
