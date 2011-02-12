all:
	cd $(R3R_UI) && $(MAKE) all

clean:
	$(foreach d,$(wildcard */Makefile),cd $(dir $(d)) && $(MAKE) clean && cd ..; )

install:
	cd $(R3R_UI) && $(MAKE) install

uninstall:
	cd $(R3R_UI) && $(MAKE) uninstall
