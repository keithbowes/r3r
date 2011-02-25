include ../../common.make
ISCC ?= ISCC

mos_lib = $(wildcard $(rootdir)/src/libr3r/po/*.mo)
mos_ui = $(wildcard $(rootdir)/src/ui/$(R3R_UI)/po/*.mo)

ifeq ($(VERSION),)
$(error VERSION should be set.  Try running from the root dir)
endif

all installer: setup.exe

setup.exe: r3r.iss
	$(ISCC) $<
	$(MOVE) $@  $(builddir)/r3r-$(VERSION)-$@

r3r.iss: _r3r.iss
	$(SED) -e 's/@VERSION@/$(VERSION)/g' $< > $@
