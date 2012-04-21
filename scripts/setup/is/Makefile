include ../../common.make
ISCC ?= $(call programpath,iscc)
ISCCFLAGS ?=

ifeq ($(VERSION),)
$(error VERSION should be set.  Try running from the root dir)
endif

all installer: setup.exe

setup.exe: r3r.iss
	$(ISCC) $<
	$(MOVE) $@  $(srcdir)/../../../../r3r-$(VERSION)-$@

r3r.iss: _r3r.iss
	$(SED) -e 's/@VERSION@/$(VERSION)/g' $< > $@
