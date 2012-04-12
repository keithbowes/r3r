include ../../scripts/common.make

all:
	cd $(R3R_UI) && $(MAKE) all

clean:
	$(foreach d,$(uis),cd $(d) && $(MAKE) clean && cd ..; )

install:
	cd $(R3R_UI) && $(MAKE) install

uninstall:
	cd $(R3R_UI) && $(MAKE) uninstall
	$(DEL) $(incdir)/libr3r.h
