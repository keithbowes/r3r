include ../../scripts/common.make

all:
	cd $(R3R_UI) && $(MAKE) all

clean:
	$(foreach d,$(uis),cd $(d) && $(MAKE) clean && cd ..; )

install:
	cd $(R3R_UI) && $(MAKE) install
	$(MKDIR) $(incdir)
	$(INSTALL) wx/libr3r.h $(incdir)

uninstall:
	cd $(R3R_UI) && $(MAKE) uninstall
	$(DEL) $(incdir)/libr3r.h
